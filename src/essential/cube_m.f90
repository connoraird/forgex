! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_cube_m module is a part of Forgex.
!
#ifdef IMPURE
#define pure
#endif
module forgex_cube_m
   use, intrinsic :: iso_fortran_env, only: int64, int32
   use :: forgex_parameters_m, only: BMP_SIZE, BMP_SIZE_BIT, bits_64, INVALID_CODE_POINT
   use :: forgex_bitmap_m, only: bmp_t
   use :: forgex_segment_m, only: segment_t, symbol_to_segment, &
      operator(.in.), SEG_INIT, SEG_EPSILON, operator(==), width_of_segment, invert_segment_list
   use :: forgex_utf8_m, only: ichar_utf8
   implicit none
   private


   type, public :: cube_t
      logical, private :: epsilon_flag = .false.
      type(bmp_t) :: bmp         ! for U+0000 .. U+FFFF BMP
      type(segment_t), allocatable :: sps(:) ! for U+10000 .. U+10FFFF SPs (SIP, SMP, etc.)
   contains
      procedure :: flag_epsilon => cube_flag__epsilon
      procedure :: is_flagged_epsilon => cube_flag__is_flagged_epsilon
      procedure :: cube_add__symbol
      procedure :: cube_add__segment
      procedure :: cube_add__segment_list
      procedure :: cube_add__cube
      procedure :: cube2seg => cube__bmp2seg
      procedure :: print_sps => cube__dump_sps
      procedure :: invert => cube__invert
      procedure :: num => cube__number_of_flagged_bits
      procedure :: first => cube__first_codepoint
      generic :: add => cube_add__symbol, cube_add__segment, cube_add__segment_list, cube_add__cube
   end type cube_t

   interface operator(.in.)
      module procedure :: cube_t__symbol_in_cube
   end interface

   interface assignment(=)
      module procedure :: cube_t__cube_assign
   end interface

   public :: operator(.in.)
   public :: assignment(=)

   integer :: q
   type(bmp_t), parameter, public :: white_bmp = bmp_t([(0_int64, q=0, BMP_SIZE-1)])

contains

   pure subroutine cube_t__cube_assign(a, b)
      implicit none
      type(cube_t), intent(inout) :: a
      type(cube_t), intent(in) :: b

      integer :: num

      a%epsilon_flag = b%epsilon_flag
      a%bmp%b(:) = b%bmp%b(:)

      if (.not. allocated(b%sps)) return
      num = ubound(b%sps, dim=1)
      a%sps = b%sps ! implicit reallocation

   end subroutine cube_t__cube_assign

   
   pure function cube_t__symbol_in_cube (symbol, cube) result(ret)
      implicit none
      character(*), intent(in) :: symbol
      type(cube_t), intent(in) :: cube
      logical :: ret

      integer :: cp

      cp = ichar_utf8(symbol)

      if (cp <= BMP_SIZE_BIT) then
         ret = iand(cube%bmp%b(cp/bits_64), ishft(1_int64, mod(cp, bits_64))) /= 0_int64
      else
         ret = symbol_to_segment(symbol) .in. cube%sps(:)
      end if

   end function cube_t__symbol_in_cube
   

   pure function cube_t__codepoint_in_cube (cp, cube) result(ret)
      implicit none
      integer(int32), intent(in) :: cp
      type(cube_t), intent(in) :: cube
      logical :: ret


      if (cp <= BMP_SIZE_BIT) then
         ret = iand(cube%bmp%b(cp/bits_64), ishft(1_int64, mod(cp, bits_64))) /= 0_int64
      else
         ret = cp .in. cube%sps(:)
      end if

   end function cube_t__codepoint_in_cube

!=====================================================================!

   pure subroutine cube_flag__epsilon(self)
      implicit none
      class(cube_t), intent(inout) :: self
      self%epsilon_flag = .true.
   end subroutine cube_flag__epsilon

   pure logical function cube_flag__is_flagged_epsilon(self)
      implicit none
      class(cube_t), intent(in) :: self
      cube_flag__is_flagged_epsilon = self%epsilon_flag
   end function cube_flag__is_flagged_epsilon

!=====================================================================!

   pure subroutine cube_add__symbol(self, symbol)
      implicit none
      class(cube_t), intent(inout) :: self
      character(*), intent(in) :: symbol

      integer :: cp
      cp = ichar_utf8(symbol)
      if (cp == -1) return ! WARNING: magic nubmer

      if (cp > BMP_SIZE_BIT) then
         call cube_add__segment(self, segment_t(cp, cp))
      else
         call self%bmp%add(cp)
      end if
   end subroutine cube_add__symbol
   
   pure subroutine cube_add__segment(self, segment)
      implicit none
      class(cube_t), intent(inout) :: self
      type(segment_t), intent(in) :: segment

      integer :: cp_min, cp_max, sps_size, i, j
      type(segment_t), allocatable :: tmp(:)
      type(segment_t) :: what_to_add

      if (segment == SEG_EPSILON) then
         call self%flag_epsilon()
         return
      end if

      cp_min = segment%min
      cp_max = segment%max

      call self%bmp%add(cp_min, cp_max)

      if (cp_max > BMP_SIZE_BIT) then

         what_to_add = segment_t(max(cp_min, BMP_SIZE_BIT), cp_max)

         if (allocated(self%sps)) then
            sps_size = size(self%sps, dim=1) + 1
            allocate(tmp(sps_size))
            j = 0
            do i = 1, size(self%sps)
               j = j + 1
               if (self%sps(i)%min < what_to_add%min) then
                  tmp(j) = self%sps(i)
               else
                  tmp(j) = what_to_add
               end if
            end do
            self%sps = tmp(1:sps_size) ! implicit reallocation

         else
            self%sps = [segment]
         end if
   
      end if

   end subroutine cube_add__segment


   pure subroutine cube_add__segment_list(self, seglist)
      implicit none
      class(cube_t), intent(inout) :: self
      type(segment_t), intent(in) :: seglist(:)

      integer :: cp_min, cp_max, siz, i, j, k, m, n, p

      type(segment_t), allocatable :: tmp(:), ret(:)
      type(segment_t) :: what_to_add


      if (allocated(self%sps)) then
         m = size(self%sps)
      else
         m = 0
      end if 
      n = size(seglist, dim=1)

      if (any(seglist == SEG_EPSILON)) then
         self%epsilon_flag = .true.
      end if

      siz = m + n
      allocate(tmp(n))
      allocate(ret(siz+1))

      k = 0 ! for tmp
      j = 1 ! for segments to add
      do while ( j <= n)
         cp_min = seglist(j)%min
         cp_max = seglist(j)%max

         call self%bmp%add(cp_min, cp_max)

         if (cp_max > BMP_SIZE_BIT) then
            k = k + 1
            what_to_add = segment_t(max(cp_min, BMP_SIZE_BIT), cp_max)
            tmp(k) = what_to_add
         end if

         j = j + 1
      end do

      p = 0
      joint: block
         type(segment_t), allocatable :: cache(:)
         if (allocated(self%sps)) then
            p = ubound(self%sps, dim=1)
            cache = self%sps
            deallocate(self%sps)
         end if

         allocate(self%sps(1:p+k))
         self%sps(1:p) = cache(1:p)
         self%sps(p+1:p+k) = tmp(1:k)
         return
         
      end block joint

   end subroutine cube_add__segment_list


   pure subroutine cube_add__cube(self, cube)
      implicit none
      class(cube_t), intent(inout) :: self
      type(cube_t), intent(in) :: cube

      integer :: i
      do i = 0, BMP_SIZE-1
         self%bmp%b(i) = ior(self%bmp%b(i), cube%bmp%b(i))
      end do

      if (allocated(cube%sps)) then
         call cube_add__segment_list(self, cube%sps)
      end if

   end subroutine cube_add__cube


   pure subroutine cube__invert(self)
      implicit none
      class(cube_t), intent(inout) :: self

      self%bmp%b(:) = not(self%bmp%b(:))

      if (.not. allocated(self%sps)) return

      ! ### NOT IMPLEMENTED SPS INVERTION PROCESS ### ! 

      call invert_segment_list(self%sps)

   end subroutine cube__invert


   pure function cube__number_of_flagged_bits(self) result(ret)
      implicit none
      class(cube_t), intent(in) :: self
      integer :: ret

      integer :: i

      ret = 0
      do i = 0, BMP_SIZE-1
         ret = ret + popcnt(self%bmp%b(i))
      end do
      
      if (allocated(self%sps)) then
         do i = 1, size(self%sps)
            ret = ret + width_of_segment(self%sps(i))
         end do
      end if

   end function cube__number_of_flagged_bits


   pure function cube__first_codepoint(self) result(ret)
      use :: forgex_parameters_m, only: UTF8_CODE_MAX
      implicit none
      class(cube_t), intent(in) :: self

      integer :: i, num, pos, ret, candi

      do i = 0, BMP_SIZE-1
         if (self%bmp%b(i) /= 0) then
            pos = trailz(self%bmp%b(i))
            ret = i*bits_64 + pos
            return
         end if
      end do

      ret = INVALID_CODE_POINT
      if (.not. allocated(self%sps)) return

      candi = UTF8_CODE_MAX
      do i = 1, size(self%sps)
         candi = min(candi, self%sps(i)%min)
      end do

      if (candi /= UTF8_CODE_MAX) then
         ret = candi
      else
         ret = -1
      end if

   end function cube__first_codepoint


!=====================================================================!

   pure subroutine cube__bmp2seg(self, segments)
      implicit none
      class(cube_t), intent(in) :: self
      type(segment_t), allocatable, intent(inout) :: segments(:)

      type(segment_t), allocatable :: tmp(:)

      integer :: m, n
      
      if (allocated(segments)) deallocate(segments)

      call self%bmp%bmp2seg(tmp)
      m = size(tmp, dim=1)
      
      if (allocated(self%sps)) then
         n = size(self%sps, dim=1)
      else 
         n = 0
      end if

      allocate(segments(m+n))

      segments(1:m) = tmp(1:m)

      if (n > 0) segments(m+1:m+n) = self%sps(1:n)


   end subroutine cube__bmp2seg


   subroutine cube__dump_sps(self)
      class(cube_t), intent(in) :: self
      
      integer :: i
      if (.not. allocated(self%sps)) return

      do i = 1, ubound(self%sps, dim=1)
         write(0,*) self%sps(i)%print()
      end do

   end subroutine cube__dump_sps

end module forgex_cube_m
   