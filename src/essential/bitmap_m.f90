#ifdef IMPURE
#define pure
#endif
module forgex_bitmap_m
   use, intrinsic :: iso_fortran_env, only: int64, int32
   use :: forgex_parameters_m, only: BMP_SIZE, BMP_SIZE_BIT, bits_64
   use :: forgex_utf8_m, only: is_valid_multiple_byte_character, ichar_utf8
   implicit none
   private

   type, public :: bmp_t
      integer(int64) :: b(0:BMP_SIZE-1) = 0_int64
         ! NOTE: 0-based index. 
         ! 65536 bits for Basic Multilingual Plane
   contains
      procedure :: bmp__add_character_range, bmp__add_character_char, bmp__add_character_codepoint
      generic :: add => bmp__add_character_range, bmp__add_character_char, bmp__add_character_codepoint
      procedure :: bmp2seg
   end type bmp_t


contains

   pure subroutine bmp__add_character_char(self, chara)
      implicit none
      class(bmp_t), intent(inout) :: self
      character(*), intent(in) :: chara

      integer(int32) :: cp, i, pos
      type(bmp_t) :: tmp

      if (.not. is_valid_multiple_byte_character(trim(chara))) error stop
      cp = ichar_utf8(trim(chara))
      i = cp / bits_64
      pos = mod(cp, bits_64)

      self%b(i) = ibset(self%b(i), pos)

   end subroutine bmp__add_character_char


   pure subroutine bmp__add_character_codepoint(self, cp)
      implicit none
      class(bmp_t), intent(inout) :: self
      integer(int32), intent(in) :: cp

      integer :: i, pos

      if (cp > BMP_SIZE_BIT) return

      i = cp / bits_64
      pos = mod(cp, bits_64)

      self%b(i) = ibset(self%b(i), pos)

   end subroutine bmp__add_character_codepoint


   pure subroutine bmp__add_character_range(self, min_cp, max_cp)
      implicit none
      class(bmp_t), intent(inout) :: self
      integer(int32), intent(in) :: min_cp, max_cp
      
      integer :: ib, ie ! (array) index begin, index end
      integer :: pb, pe ! (bit) position begin, position end
      integer :: i
      integer(int64) :: c1, c2

      if (min_cp > max_cp) return
      if (min_cp > BMP_SIZE_BIT) return
      if (min_cp == max_cp) then
         call bmp__add_character_codepoint(self, min_cp)
         return
      end if

      ib = min_cp / bits_64
      ie = min( max_cp/bits_64, BMP_SIZE-1)

      pb = mod(min_cp, bits_64)
      if (max_cp > BMP_SIZE_BIT) then
         pe = 64
      else
         pe = mod(max_cp, bits_64)
      end if

      c1 = self%b(ib)
      c2 = self%b(ie)

      if (ib == ie) then
         ! Set bits in the range min to max.
         self%b(ib) = ior(c1, shiftl( (ishft(1_8, pe - pb + 1) - 1), pb))
      else
         ! First integer: set pb to 63
         self%b(ib) = ior(c1, shiftl(not(0_int64), pb))

         ! The integers between have all bits set to 1.
         do i = ib +1, ie -1
            self%b(i) = -1_int64
         end do

         ! Last integer: set bits from 0 to pe.
         self%b(ie) = ior(c2, (ishft(1_int64, pe+1)-1))
      end if

   end subroutine bmp__add_character_range


   pure subroutine bmp2seg(self, segments)
      use :: forgex_segment_m, only: segment_t
      implicit none
      class(bmp_t), intent(in) :: self
      type(segment_t), intent(inout), allocatable :: segments(:)

      type(segment_t), allocatable :: tmp(:)
      integer :: i, j, jb, je, k
      logical :: in_range

      in_range = .false.

      k = 0
      do i = 0, BMP_SIZE-1
         do j = 0, bits_64-1
            if (btest(self%b(i), j)) then
               if (.not. in_range) then
                  jb = i*bits_64-j
                  in_range = .true.
               end if
            else
               if (in_range) then
                  je = i*bits_64+j -1
                  k = k + 1
                  tmp(k)%min = jb
                  tmp(k)%max = je
                  in_range = .false.
               end if
            end if
         end do
      end do

      if (in_range) then
         k = k + 1
         tmp(k)%min = jb
         tmp(k)%max = i*bits_64 + (bits_64-1)
      end if

      allocate(segments(k))
      segments(:) = tmp(1:k)
      deallocate(tmp)

   end subroutine bmp2seg

end module forgex_bitmap_m
