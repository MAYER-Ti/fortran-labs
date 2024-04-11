module calcPositions 
   use Environment
   use IOEmployee
   implicit none

contains
    pure subroutine CalcPos(positions, outPos, outCount, countPositions)
        character(kind=CH_), intent(in)  :: positions(EMPLOYEE_COUNT, BLOCK_LEN)
        character(kind=CH_), allocatable, intent(out) :: outPos(:,:) 
        integer, allocatable, intent(out) :: outCount(:)
        integer, intent(inout)           :: countPositions   
        
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
                  locPosition(j) = ALL(positions(:,j) == positions(:,i))
               end do
               !Трудности при замене на сечение!
               !locPosition(i+1:EMPLOYEE_COUNT) = positions(:,i+1:EMPLOYEE_COUNT) == positions(:,i)
               ! Записать количество одинаковых должностей 
               posAndCount(1, countPositions) = i ! Позиция с должностью 
               posAndCount(2, countPositions) = Count(locPosition) ! Кол-во сотрудников с этой должностью
               ! Обновить массив совпадений для следующих итераций цикла
               matched(i:EMPLOYEE_COUNT) = matched(i:EMPLOYEE_COUNT) .or. locPosition(i:EMPLOYEE_COUNT)
               locPosition = .false.
           end if
        end do
        ! Запись данных в массивы
        allocate(outPos(BLOCK_LEN, countPositions), outCount(countPositions))
        do i = 1, countPositions
          outPos(:,i) = positions(:, posAndCount(1, i))
          outCount(i) = posAndCount(2, i) 
        end do
   end subroutine CalcPos 


end module calcPositions 
