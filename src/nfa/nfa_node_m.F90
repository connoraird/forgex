! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_nfa_node_m module is a part of Forgex.
!
!! This file contains `nfa_t` class and its type-bound procedures.

!> The `forgex_nfa_m` module defines the data structure of NFA.
!> The `nfa_t` is defined as a class representing NFA.
#ifdef IMPURE
#define pure
#endif
module forgex_nfa_node_m
   use, intrinsic :: iso_fortran_env, only: stderr=>error_unit, int32
   use :: forgex_parameters_m, only: TREE_NODE_BASE, TREE_NODE_LIMIT, ALLOC_COUNT_INITTIAL, &
      NFA_NULL_TRANSITION, NFA_TRANSITION_UNIT, NFA_STATE_BASE, NFA_STATE_UNIT
   use :: forgex_syntax_tree_graph_m, only: tree_t
   use :: forgex_segment_m, only: segment_t, SEG_EPSILON, operator(/=)
   use :: forgex_cube_m, only: cube_t
   implicit none
   private


   type, public :: nfa_transition_t
      type(cube_t) :: c
      integer(int32) :: dst = NFA_NULL_TRANSITION
      integer(int32) :: own_j = NFA_NULL_TRANSITION
      logical        :: is_registered = .false.
   end type nfa_transition_t

   type, public :: nfa_state_node_t
      integer(int32) :: own_i
      type(nfa_transition_t), allocatable :: forward(:)
      integer(int32) :: forward_top
      integer(int32) :: alloc_count_f = ALLOC_COUNT_INITTIAL
   contains
      procedure :: add_transition => nfa__add_transition
      procedure :: realloc_forward => nfa__reallocate_transition_forward
      ! procedure :: merge_segment
   end type nfa_state_node_t

   type, public :: nfa_graph_t
      type(nfa_state_node_t), allocatable :: graph(:)
      integer(int32) :: top = 0
      integer(int32) :: entry = 0
      integer(int32) :: exit = 0
   contains
      procedure :: new_nfa_node => nfa_graph__new_node
      procedure :: is_exceeded => nfa_graph__is_exceeded
      procedure :: reallocate => nfa_graph__reallocate
   end type nfa_graph_t

contains

   pure subroutine nfa__add_transition (self, src, dst, seg)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self
      integer(int32), intent(in) :: src, dst
      type(segment_t), intent(in) :: seg(:)

      integer :: j, k

      j = NFA_NULL_TRANSITION
      if (allocated(self%forward) .and. any(seg /= SEG_EPSILON)) then
         do k = 1, self%forward_top
            if (dst == self%forward(k)%dst) then
               j = k
            end if
         end do
      end if
      
      if (j == NFA_NULL_TRANSITION) then
         j = self%forward_top
      end if

      if (.not. allocated(self%forward)) then
         call self%realloc_forward()
      end if

      call self%forward(j)%c%add(seg)
      self%forward(j)%dst = dst
      self%forward(j)%is_registered = .true.

      if (j == self%forward_top) self%forward_top = self%forward_top + 1

   end subroutine nfa__add_transition


   pure subroutine nfa__reallocate_transition_forward (self)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self

      type(nfa_transition_t), allocatable :: tmp(:)
      integer :: siz, j
      integer :: prev_count, new_part_begin, new_part_end

      siz = 0
      prev_count = 0
      new_part_begin = 0
      new_part_end = 0

      if (allocated(self%forward)) then
         siz = size(self%forward, dim=1)
         call move_alloc(self%forward, tmp)
      else
         siz = 0
      end if

      prev_count = self%alloc_count_f
      self%alloc_count_f = prev_count + 1

      new_part_begin = siz + 1
      new_part_end = NFA_TRANSITION_UNIT * 2**self%alloc_count_f

      allocate(self%forward(1:new_part_end))

      if (allocated(tmp)) then
         if (siz > 0) self%forward(1:siz) = tmp(1:siz)
      end if

      self%forward(1:new_part_end)%own_j = [(j, j=1, new_part_end)]

   end subroutine nfa__reallocate_transition_forward


!=====================================================================!
   pure subroutine nfa_graph__new_node(self)
      implicit none
      class(nfa_graph_t), intent(inout) :: self

      self%top = self%top + 1
   end subroutine nfa_graph__new_node


   pure function nfa_graph__is_exceeded(self) result(ret)
      implicit none
      class(nfa_graph_t), intent(in) :: self
      logical :: ret

      ret = ubound(self%graph, dim=1) < self%top

   end function nfa_graph__is_exceeded


   pure subroutine nfa_graph__reallocate(self)
      implicit none
      class(nfa_graph_t), intent(inout) :: self
      type(nfa_state_node_t), allocatable :: tmp(:)
      integer :: n

      n = ubound(self%graph, dim=1)
      call move_alloc(self%graph, tmp)

      allocate(self%graph(NFA_STATE_BASE:n*2))
      
      self%graph(NFA_STATE_BASE:n) = tmp(NFA_STATE_BASE:n)

      self%graph(n+1:n*2)%forward_top = 1
   end subroutine nfa_graph__reallocate

!=====================================================================!

   pure subroutine build_nfa_graph(tree, nfa, nfa_entry, nfa_exit, entire)
      implicit none
      type(tree_t), intent(in) :: tree
      type(nfa_graph_t), intent(inout) :: nfa
      integer(int32), intent(inout) :: nfa_entry, nfa_exit
      type(cube_t), intent(inout) :: entire

      integer(int32) :: i, ib, ie ! index for states array

      ib = NFA_STATE_BASE
      ie = NFA_STATE_UNIT

      ! initialize
      nfa%top = 0
      allocate(nfa%graph(ib:ie))
      nfa%graph(ib:ie)%own_i = [(i, i=ib, ie)]
      nfa%graph(:)%alloc_count_f = 0
      nfa%graph(:)%forward_top = 1

      call nfa%new_nfa_node()
      nfa%entry = nfa%top
      call nfa%new_nfa_node()
      nfa%exit = nfa%top


      call generate_nfa(tree, tree%top, nfa, nfa%entry, nfa%exit)

      ! do i = 1, nfa%top
      !    call nfa%graph(i)%merge_segments()
      ! end do

      ! call disjoin_nfa()

   end subroutine build_nfa_graph


   pure recursive subroutine generate_nfa(tree, idx, nfa, entry_i, exit_i)
      use :: forgex_enums_m, only: op_char, op_empty, op_closure, op_concat, op_repeat
      use :: forgex_parameters_m, only: INFINITE, INVALID_INDEX
      implicit none
      type(tree_t), intent(in) :: tree
      integer(int32), intent(in) :: idx
      type(nfa_graph_t), intent(inout) :: nfa
      integer(int32), intent(in) :: entry_i, exit_i

      integer :: i
      integer :: k
      integer :: node1, node2, entry_local

      if (idx == INVALID_INDEX) return

      select case(tree%nodes(i)%op)
      case (op_char)
         if (.not. allocated(tree%nodes(i)%c)) then
            error stop "ERROR: Character node of the AST do not have actual character list."
         end if
         ! Handle character operations by adding transition for each character.
         call nfa%graph(entry_i)%add_transition(entry_i, exit_i, tree%nodes(i)%c)
      
      case (op_empty)
         ! Handle empty opration by adding an epsilon transition
         call nfa%graph(entry_i)%add_transition(entry_i, exit_i, [SEG_EPSILON])
      
      case (op_closure)
         ! Handle closure (Kleene star) operations by creating new node and adding appropriate transition
         call generate_nfa_closure(tree, idx, nfa, entry_i, exit_i)
      
      case (op_concat)
         ! Handle concatenation operations by recursively generating NFA for left and right subtrees.
         call generate_nfa_concatenate(tree, idx, nfa, entry_i, exit_i)
      
      case (op_repeat)
         block
            integer(int32) :: min_repeat, max_repeat, j
            integer(int32) :: num_1st_repeat, num_2nd_repeat
            min_repeat = tree%nodes(i)%min_repeat
            max_repeat = tree%nodes(i)%max_repeat

            num_1st_repeat = min_repeat-1
            if (max_repeat == INFINITE) then
               num_1st_repeat = num_1st_repeat +1
            end if

            do j = 1, num_1st_repeat
               call nfa%new_nfa_node()

               if (nfa%is_exceeded()) call nfa%reallocate()

               node1 = nfa%top
               call generate_nfa(tree, tree%nodes(i)%left_i, nfa, entry_local, node1)
               entry_local = node1
            end do

            if (min_repeat == 0) then
               num_2nd_repeat = max_repeat - 1
            else
               num_2nd_repeat = max_repeat - min_repeat
            end if

            do j = 1, num_2nd_repeat
               call nfa%new_nfa_node()
               if (nfa%is_exceeded()) call nfa%reallocate()
               node2 = nfa%top

               call generate_nfa(tree, tree%nodes(i)%left_i, nfa, entry_local, node2)
               call nfa%graph(node2)%add_transition(node2, exit_i, [SEG_EPSILON])
               entry_local = node2
            end do
            

            if (min_repeat == 0) then
               call nfa%graph(entry_i)%add_transition(entry_i, exit_i, [SEG_EPSILON])
            end if

            if (max_repeat == INFINITE) then
               call generate_nfa_closure(tree, idx, nfa, entry_local, exit_i)
            else
               call generate_nfa(tree, tree%nodes(i)%left_i, nfa, entry_local, exit_i)
            end if

         end block
      case default ! for case (op_not_init)
         ! Handle unexpected cases.
         error stop "This will not heppen in 'generate_nfa'."
      end select
   end subroutine generate_nfa


   pure recursive subroutine generate_nfa_concatenate(tree, idx, nfa, entry_i, exit_i)
      implicit none
      type(tree_t), intent(in) :: tree
      type(nfa_graph_t), intent(inout) :: nfa
      integer(int32), intent(in) :: idx
      integer(int32), intent(in) :: entry_i, exit_i

      integer(int32) :: node1

      call nfa%new_nfa_node()
      if (nfa%is_exceeded()) call nfa%reallocate()

      node1 = nfa%top

      call generate_nfa(tree, tree%nodes(idx)%left_i, nfa, entry_i, node1)
      call generate_nfa(tree, tree%nodes(idx)%right_i, nfa, node1, exit_i)

   end subroutine generate_nfa_concatenate


   pure recursive subroutine generate_nfa_closure(tree, idx, nfa, entry_i, exit_i)
      implicit none
      type(tree_t), intent(in) :: tree
      type(nfa_graph_t), intent(inout) :: nfa
      integer(int32), intent(in) :: idx
      integer(int32), intent(in) :: entry_i, exit_i

      integer(int32) :: node1, node2

      call nfa%new_nfa_node()
      if (nfa%is_exceeded()) call nfa%reallocate()
      node1 = nfa%top

      call nfa%new_nfa_node()
      if (nfa%is_exceeded()) call nfa%reallocate()
      node2 = nfa%top

      call nfa%graph(entry_i)%add_transition(entry_i, node1, [SEG_EPSILON])
      call generate_nfa(tree, tree%nodes(idx)%left_i, nfa, node1, node2)

      call nfa%graph(node2)%add_transition(node2, node1, [SEG_EPSILON])
      call nfa%graph(node1)%add_transition(node1, exit_i, [SEG_EPSILON])

   end subroutine generate_nfa_closure

end module forgex_nfa_node_m