#ifdef IMPURE
#define pure
#endif
module forgex_cube_m
   use, intrinsic :: iso_fortran_env, only: int64
   use :: forgex_parameters_m, only: BMP_SIZE, BMP_SIZE_BIT
   use :: forgex_bitmap_m, only: bmp_t
   use :: forgex_segment_m, only: segment_t
   implicit none


   type, public :: cube_t
      type(bmp_t) :: bmp         ! for U+0000 .. U+FFFF BMP
      type(segment_t), allocatable :: sps(:) ! for U+10000 .. U+10FFFF SPs (SIP, SMP, etc.)
   contains
      procedure :: init =>  cube_init__from_bmp,  cube_init__from_segment, cube_init__from_segment_list
      procedure :: add => cube_add__segment, cube_add__segment_list
   end type cube_t

   integer :: q
   type(bmp_t), parameter :: white_bmp = bmp_t([(0_int64, q=0, BMP_SIZE-1)])

contains

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
      
      allocate(self%sps(j))
      self%sps(1:j) = tmp(1:j)

   end subroutine cube_init__from_segment_list


   pure subroutine cube_add__segment_list(self, seglist)
      implicit none
      class(cube_t), intent(inout) :: self
      type(segment_t), intent(in) :: seglist(:)

      integer :: cp_min, cp_max, siz, i, j, k, m, n

      type(segment_t), allocatable :: tmp(:), ret(:)
      type(segment_t) :: what_to_add
      
      if (.not. allocated(self%sps)) then
         m = size(self%sps)
      else
         m = 0
      end if 
      n = size(seglist, dim=1)

      siz = m + n      

      allocate(tmp(n))
      allocate(ret(siz))

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

      merge: block
         if (allocated(self%sps)) then
            i = 1
            j = 1
            k = 0
            do while (i <= m .and. j <= n)
               k = k + 1
               if (self%sps(i)%min < tmp(i)%min) then
                  ret(k) = self%sps(i)
                  i = i + 1
               else
                  ret(k) = tmp(j)
                  j = j + 1
               end if
            end do

            do while (i <= m)
               ret(k) = self%sps(i)
               i = i + 1; k = k + 1
            end do
            do while (j <= n)
               ret(k) = tmp(j)
               j = j + 1; k = k + 1
            end do
         end if
      end block merge

      if (allocated(self%sps)) deallocate(self%sps)
      allocate(self%sps(k))
      self%sps(:) = ret(1:k)
      
   end subroutine cube_add__segment_list



end module forgex_cube_m
   