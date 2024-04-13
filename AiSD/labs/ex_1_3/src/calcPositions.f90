module calcPositions 
   use Environment
   use IOEmployee
   implicit none

contains
    pure subroutine CalcPos(employees, outPos, outCount, countPositions)
        type(employee), intent(in)                                 :: employees(EMPLOYEE_COUNT)
        character(BLOCK_LEN, kind=CH_), allocatable, intent(out) :: outPos(:) 
        integer, allocatable, intent(out)                          :: outCount(:)
        integer, intent(inout)                                     :: countPositions   
        
        logical :: matched(EMPLOYEE_COUNT), locPosition(EMPLOYEE_COUNT)
        integer :: i, j, posAndCount(2, EMPLOYEE_COUNT)

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
               do concurrent (j = i+1:EMPLOYEE_COUNT)
                  locPosition(j) = employees(j)%pos == employees(i)%pos
               end do
               ! Записать количество одинаковых должностей 
               posAndCount(1, countPositions) = i ! Позиция с должностью 
               posAndCount(2, countPositions) = Count(locPosition) ! Кол-во сотрудников с этой должностью
               ! Обновить массив совпадений для следующих итераций цикла
               matched(i:EMPLOYEE_COUNT) = matched(i:EMPLOYEE_COUNT) .or. locPosition(i:EMPLOYEE_COUNT)
               locPosition = .false.
           end if
        end do
        ! Запись данных в массивы
        allocate(outPos(countPositions), outCount(countPositions))
        do i = 1, countPositions
          outPos(i) = employees(posAndCount(1, i))%pos
          outCount(i) = posAndCount(2, i) 
        end do
   end subroutine CalcPos 


end module calcPositions 
