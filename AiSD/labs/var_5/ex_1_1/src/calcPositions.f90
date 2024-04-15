module calcPositions 
   use Environment
   use globalVars
   implicit none

contains
   pure subroutine CalcPos(positions, outPos, outCount, countPositions)
      character(BLOCK_LEN, kind=CH_), intent(in)    :: positions(EMPLOYEE_COUNT)
      integer, allocatable, intent(out)             :: outCount(:)
      character(BLOCK_LEN, kind=CH_), allocatable, intent(inout) :: outPos(:)
      integer, intent(inout)                        :: countPositions   
 
      logical :: matched(EMPLOYEE_COUNT), locPosition(EMPLOYEE_COUNT)
      integer :: i, posAndCount(2, EMPLOYEE_COUNT)
      countPositions = 0 
      matched = .false.
      locPosition = .false.
      do i = 1,EMPLOYEE_COUNT
         ! Когда должность еще не обрабатывалась
         if (.not. matched(i)) then
             ! Посчитать новую должность
             countPositions = countPositions + 1
             ! Совпадает сама с собой 
             locPosition(i) = .true.
             ! Создание маски 
             locPosition(i+1:EMPLOYEE_COUNT) = positions(i+1:EMPLOYEE_COUNT) == positions(i)
             ! Записать количество одинаковых должностей 
             posAndCount(1, countPositions) = i ! Позиция с должностью 
             posAndCount(2, countPositions) = Count(locPosition) ! Кол-во сотрудников с этой должностью
             ! Обновить массив совпадений для следующих итераций цикла
             matched(i:EMPLOYEE_COUNT) = matched(i:EMPLOYEE_COUNT) .or. locPosition(i:EMPLOYEE_COUNT)
             locPosition = .false.
         end if
      end do
      allocate(outPos(countPositions), outCount(countPositions))
      do i = 1, countPositions
        outPos(i) = positions(posAndCount(1, i))
        outCount(i) = posAndCount(2, i) 
      end do   
 
 
   end subroutine CalcPos 


end module calcPositions 
