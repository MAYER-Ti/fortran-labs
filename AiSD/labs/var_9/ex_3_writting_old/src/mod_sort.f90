module mod_Sort
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use mod_IO

   implicit none


contains

   pure subroutine ShellSort(List)
      type(node), pointer, intent(inout) :: List 

      integer :: gap

      gap = 1
      call GetGap(List, gap)
      
      call Sort(List, gap)

   end subroutine ShellSort

   pure recursive subroutine GetGap(node, gap)
      type(node), pointer, intent(inout) :: node
      integer, intent(inout)             :: gap

      gap = gap * 3 + 1  
      
      if(Associated(node%next)) &
         call GetGap(node%next, gap)

   end subroutine GetGap

   pure subroutine Sort(node, gap)
      type(node), pointer, intent(inout) :: node
      integer, intent(inout)             :: gap

      integer :: i 
      logical :: isSwap

      isSwap = .false. 
      do i = gap+1,2,-1
         call Sort_val(node, i, isSwap)
      end do
   end subroutine Sort

   pure recursive subroutine Sort_val(node, gap, isSwap)
      type(node), pointer, intent(inout) :: node
      integer,intent(in)                 :: gap
      logical, intent(inout)             :: isSwap

      type(node), pointer :: nextNode



      if(Associated(node%next)) then 
         Sort_val(node%next, gap, isSwap)
      end if 




   end subroutine Sort_val
      
end module mod_Sort

