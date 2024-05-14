module mod_Sort
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use mod_IO

   implicit none
   private

   public :: Sort

contains

   pure recursive subroutine Sort(current, SortedList)
      type(node), pointer :: SortedList
      type(node), target, intent(inout)  :: current

      call SortedPut(SortedList, current)

      if(Associated(current%next)) &
         call Sort(current%next, SortedList)

   end subroutine

   pure recursive subroutine SortedPut(SortedCurrent, current)
      type(node), pointer               :: sortedCurrent
      type(node), target, intent(inout) :: current

      if (.not. Associated(sortedCurrent)) then
         ! Либо голова пока никуда не ссылается,
         ! либо дошли до поледнего элемента.
         sortedCurrent => current
      else if (LEN(current%string) >= LEN(sortedCurrent%string)) then
         call SortedPut(sortedCurrent%next_len, current)
      else
         current%next_len => sortedCurrent
         sortedCurrent => current
      end if
   end subroutine SortedPut
 
  end module mod_Sort
