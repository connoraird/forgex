#ifdef IMPURE
#define pure
#endif
module forgex_cube_m
   use, intrinsic :: iso_fortran_env, only: int64, int32
   use :: forgex_parameters_m, only: BMP_SIZE, BMP_SIZE_BIT, bits_64
   use :: forgex_bitmap_m, only: bmp_t
   use :: forgex_segment_m, only: segment_t, symbol_to_segment, operator(.in.), SEG_INIT
   use :: forgex_utf8_m, only: ichar_utf8
   implicit none
   private


   type, public :: cube_t
      type(bmp_t) :: bmp         ! for U+0000 .. U+FFFF BMP
      type(segment_t), allocatable :: sps(:) ! for U+10000 .. U+10FFFF SPs (SIP, SMP, etc.)
   contains
      procedure :: cube_init__from_bmp
      procedure :: cube_init__from_segment
      procedure :: cube_init__from_segment_list
      procedure :: cube_add__symbol
      procedure :: cube_add__segment
      procedure :: cube_add__segment_list
      procedure :: cube_add__cube
      procedure :: cube__erase
      procedure :: free => cube__free
      procedure :: cube2seg => cube__bmp2seg
      procedure :: print_sps => cube__dump_sps
      generic :: erase => cube__erase
      generic :: init => cube_init__from_bmp, cube_init__from_segment_list
      generic :: add => cube_add__symbol, cube_add__segment, cube_add__segment_list, cube_add__cube
   end type cube_t

   interface operator(.in.)
      module procedure :: cube_t__symbol_in_cube
   end interface

   public :: operator(.in.)

   integer :: q
   type(bmp_t), parameter, public :: white_bmp = bmp_t([(0_int64, q=0, BMP_SIZE-1)])

contains

   
   pure function cube_t__symbol_in_cube (symbol, cube) result(ret)
      implicit none
      character(*), intent(in) :: symbol
      type(cube_t(*)), intent(in) :: cube
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
      type(cube_t(*)), intent(in) :: cube
      logical :: ret


      if (cp <= BMP_SIZE_BIT) then
         ret = iand(cube%bmp%b(cp/bits_64), ishft(1_int64, mod(cp, bits_64))) /= 0_int64
      else
         ret = cp .in. cube%sps(:)
      end if

   end function cube_t__codepoint_in_cube

!=====================================================================!


   pure subroutine cube_init__from_bmp(self, bmp)
      implicit none
      class(cube_t), intent(inout) :: self
      type(bmp_t), intent(in) :: bmp

      self%bmp = bmp

   end subroutine cube_init__from_bmp


   pure subroutine cube_init__from_segment(self, seg)
      implicit none
      class(cube_t), intent(inout) :: self
      type(segment_t), intent(in) :: seg
      
      integer :: cp_min, cp_max

      self%bmp = white_bmp

      cp_min = seg%min
      cp_max = seg%max

      call self%bmp%add(cp_min, cp_max)

      if (cp_max > BMP_SIZE_BIT) then
         allocate(self%sps(1))
         self%sps(1)%min = max(cp_min, BMP_SIZE_BIT)
         self%sps(1)%max = cp_max
      end if
      

   end subroutine cube_init__from_segment


   pure subroutine cube__erase(self)
      implicit none
      class(cube_t), intent(inout) :: self

      self%bmp = white_bmp
      if (allocated(self%sps)) deallocate(self%sps)
      self%sps = [SEG_INIT]
   end subroutine cube__erase


   pure subroutine cube_init__from_segment_list(self, seglist)
      implicit none
      class(cube_t), intent(inout) :: self
      type(segment_t), intent(in) :: seglist(:)

      integer :: cp_min, cp_max
      integer :: siz_list, i, j

      type(segment_t), allocatable :: tmp(:)

      siz_list = size(seglist, dim=1)

      allocate(tmp(siz_list))
      
      j = 0
      do i = 1, siz_list
         cp_min = seglist(i)%min
         cp_max = seglist(i)%max

         call self%bmp%add(cp_min, cp_max)

         if (cp_max > BMP_SIZE_BIT) then
            j = j + 1
            tmp(j) = segment_t(max(cp_min, BMP_SIZE_BIT), cp_max)
         end if
      enddo
      if (allocated(self%sps)) deallocate(self%sps)

      if (j /= 0) then
         allocate(self%sps(j))
         self%sps(1:j) = tmp(1:j)
      end if

   end subroutine cube_init__from_segment_list


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

      cp_min = segment%min
      cp_max = segment%max

      call self%bmp%add(cp_min, cp_max)

      if (cp_max > BMP_SIZE_BIT) then

         what_to_add = segment_t(max(cp_min, BMP_SIZE_BIT), cp_max)

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
      merge: block
         if (allocated(self%sps)) then
            i = 1
            j = 1
            do while (i <= m .and. j <= k)
               if (self%sps(i)%min < tmp(i)%min) then
                  p = p + 1
                  ret(p) = self%sps(i)
                  i = i + 1
               else
                  p = p + 1
                  ret(p) = tmp(j)
                  j = j + 1
               end if
            end do

            do while (i <= m .and. k <=siz)
               p = p + 1
               ret(p) = self%sps(i)
               i = i + 1
            end do
            do while (j <= n .and. k <= siz)
               p = p + 1
               ret(p) = tmp(j)
               j = j + 1; k = k + 1
            end do
         else
            p = size(tmp, dim=1)
         end if
      end block merge

      if (allocated(self%sps)) deallocate(self%sps)
      allocate(self%sps(p))
      self%sps(:) = ret(1:p)

   end subroutine cube_add__segment_list


   pure subroutine cube_add__cube(self, cube)
      implicit none
      class(cube_t), intent(inout) :: self
      type(cube_t), intent(in) :: cube

      integer :: i
      do i = 0, BMP_SIZE-1
         self%bmp%b(i) = ior(self%bmp%b(i), cube%bmp%b(i))
      end do

      call cube_add__segment_list(self, cube%sps)

   end subroutine cube_add__cube

   pure subroutine cube__free(self)
      implicit none
      class(cube_t), intent(inout) :: self

      if (allocated(self%sps)) deallocate(self%sps)
   end subroutine cube__free

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
   