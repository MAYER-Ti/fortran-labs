module calcPositions 
   use Environment
   use IOEmployee
   implicit none

contains
    pure subroutine SearchPositions(positions, outPos, outCounts, sizePosCounts)
        character(BLOCK_LEN,kind=CH_), intent(in)               :: positions(EMPLOYEE_COUNT)
        character(BLOCK_LEN,kind=CH_), allocatable, intent(out) :: outPos(:)
        integer, allocatable, intent(out)                         :: outCounts(:)
        integer, intent(inout)                                    :: sizePosCounts

        integer :: CountAndPos(2, EMPLOYEE_COUNT)
        logical :: matched(EMPLOYEE_COUNT), locPosition(EMPLOYEE_COUNT)
        
        call RecCalcPos(positions, CountAndPos, sizePosCounts, matched, locPosition, EMPLOYEE_COUNT)     
        
        allocate(outPos(sizePosCounts), outCounts(sizePosCounts))
        do i = 1, sizePosCounts
           outPos(i) = positions(CountAndPos(1,i))
           outCounts = CountAndPos(2,i)
        end do

    end subroutine SearchPositions   

    pure recursive subroutine RecCalcPos(positions, CountAndPos, sizePosCounts, matched, locPosition, i) 
       character(BLOCK_LEN,kind=CH_), intent(in) :: positions
       integer, intent(inout) :: CountAndPos(2, EMPLOYEE_COUNT)
       integer, intent(inout) :: sizePosCounts, i
       logical, intent(inout) :: matched(EMPLOYEE_COUNT), locPosition(EMPLOYEE_COUNT)
       
       ! Базовый случай 
       if(i == 0) then
          return
       end if

       if (.not. matched(i)) then
          ! Посчитать новую должность
          sizePosCounts = sizePosCounts + 1
          ! Совпадает сама с собой 
          locPosition(i) = .true.
          ! Создание маски 
          locPosition(i+1:EMPLOYEE_COUNT) = positions(i+1:EMPLOYEE_COUNT) == positions(i)
          ! Записать количество одинаковых должностей 
          CountAndPos(1, sizePosCounts) = i ! Позиция с должностью 
          CountAndPos(2, sizePosCounts) = Count(locPosition) ! Кол-во сотрудников с этой должностью
          ! Обновить массив совпадений для следующих итераций цикла
          matched(i:) = matched(i:) .or. locPosition(i:)
          locPosition = .false.
       end if 
       
       RecCalcPos(positions, CountAndPos, sizePosCounts, matched, locPosition, i-1)
    end subroutine RecCalcPos

    pure subroutine CalcPos(empls, outPos, outCount, countPositions)
        type(employees), intent(in)                              :: empls
        character(BLOCK_LEN, kind=CH_), allocatable, intent(out) :: outPos(:) 
        integer, allocatable, intent(out)                        :: outCount(:)
        integer, intent(inout)                                   :: countPositions   
        
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
               locPosition(i+1:EMPLOYEE_COUNT) = empls%pos(i+1:EMPLOYEE_COUNT) == empls%pos(i)
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
          outPos(i) = empls%pos(posAndCount(1, i))
          outCount(i) = posAndCount(2, i) 
        end do
   end subroutine CalcPos 


end module calcPositions 
