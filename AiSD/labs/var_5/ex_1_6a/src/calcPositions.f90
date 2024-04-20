module Process 
   use Environment
   use IOEmployee
   implicit none

contains
  ! pure recursive subroutine Put_Pos(Elem, pos, counts)
  !    character(BLOCK_LEN, kind=CH_), intent(in) :: pos
  !    integer, intent(in)                        :: counts 
  !    
  !    type(nodePosCount), pointer  :: Elem

  !    if (.not. Associated(Elem)) then
  !       allocate (Elem)
  !       Elem%pos = pos 
  !       Elem%counts = counts
  !    else
  !      call Put(Elem%next, pos, counts)
  !   end if
  ! end subroutine Put_Pos


    pure subroutine SearchPositions(List, ListPosAndCount)
        type(nodeEmpl), pointer, intent(inout) :: List 
        type(nodePosCount), pointer, intent(inout) :: ListPosAndCount
        ! Подсчет должностей
        call RecCalcPos(List, ListPosAndCount)     

    end subroutine SearchPositions   

    pure recursive subroutine RecCalcPos(elemEmpl, ListPosAndCount) 
       type(nodeEmpl), pointer, intent(inout)     :: elemEmpl
       type(nodePosCount), pointer, intent(inout) :: ListPosAndCount

       if (Associated(elemEmpl)) then
          ! Если такой дожности нету, то добавить, если есть, то увеличить кол-во 
          call AddOrPlusPos(elemEmpl, ListPosAndCount)
          ! Следующий узел
          call RecCalcPos(elemEmpl%next, ListPosAndCount) 
       end if

       
      ! if(i > EMPLOYEE_COUNT) then 
      !    return
      ! end if 
      ! if (.not. matched(i)) then
      !    ! Посчитать новую должность
      !    sizeCountAndPos = sizeCountAndPos + 1
      !    ! Совпадает сама с собой 
      !    locPosition(i) = .true.
      !    ! Создание маски 
      !    locPosition(i+1:EMPLOYEE_COUNT) = positions(i+1:EMPLOYEE_COUNT) == positions(i)
      !    ! Записать количество одинаковых должностей 
      !    OutCountAndPos(1, sizeCountAndPos) = i ! Позиция с должностью 
      !    OutCountAndPos(2, sizeCountAndPos) = Count(locPosition) ! Кол-во сотрудников с этой должностью
      !    ! Обновить массив совпадений для следующих итераций цикла
      !    matched(i:) = matched(i:) .or. locPosition(i:)
      !    locPosition = .false.
      ! end if 
      ! call RecCalcPos(positions, OutCountAndPos, sizeCountAndPos, matched, locPosition, i + 1)
    end subroutine RecCalcPos
    pure recursive subroutine AddOrPlusPos(elemEmpl, elemPosAndCount)
       type(nodeEmpl), pointer, intent(inout)     :: elemEmpl
       type(nodePosCount), pointer, intent(inout) :: elemPosAndCount

       if(Associated(elemPosAndCount)) then
          ! Если такая должность уже встречалась
          if(elemEmpl%pos == elemPosAndCount%pos) then
              elemPosAndCount%counts = elemPosAndCount%counts + 1
          else 
              ! Если должность не та, то сравнить следующую
              call AddOrPlusPos(elemEmpl, elemPosAndCount%next)
          end if
       else
           ! Если должность не нашли, то добавить
           allocate(elemPosAndCount)
           elemPosAndCount%pos    = elemEmpl%pos
           elemPosAndCount%counts = 1
       end if
    end subroutine AddOrPlusPos

   ! pure subroutine CalcPos(empls, outPos, outCount, countPositions)
   !    type(employees), intent(in)                              :: empls
   !    character(BLOCK_LEN, kind=CH_), allocatable, intent(out) :: outPos(:) 
   !    integer, allocatable, intent(out)                        :: outCount(:)
   !    integer, intent(inout)                                   :: countPositions   
   !    
   !    logical :: matched(EMPLOYEE_COUNT), locPosition(EMPLOYEE_COUNT)
   !    integer :: i, posAndCount(2, EMPLOYEE_COUNT)

   !    countPositions = 0 
   !    matched = .false.
   !    locPosition = .false.
   !    do i = 1,EMPLOYEE_COUNT
   !       ! Когда должность еще не обрабатывалась
   !       if (.not. matched(i)) then
   !           ! Посчитать новую должность
   !           countPositions = countPositions + 1
   !           ! Совпадает сама с собой 
   !           locPosition(i) = .true.
   !           ! Создание маски 
   !           locPosition(i+1:EMPLOYEE_COUNT) = empls%pos(i+1:EMPLOYEE_COUNT) == empls%pos(i)
   !           ! Записать количество одинаковых должностей 
   !           posAndCount(1, countPositions) = i ! Позиция с должностью 
   !           posAndCount(2, countPositions) = Count(locPosition) ! Кол-во сотрудников с этой должностью
   !           ! Обновить массив совпадений для следующих итераций цикла
   !           matched(i:EMPLOYEE_COUNT) = matched(i:EMPLOYEE_COUNT) .or. locPosition(i:EMPLOYEE_COUNT)
   !           locPosition = .false.
   !       end if
   !    end do
   !    ! Запись данных в массивы
   !    allocate(outPos(countPositions), outCount(countPositions))
   !    do i = 1, countPositions
   !      outPos(i) = empls%pos(posAndCount(1, i))
   !      outCount(i) = posAndCount(2, i) 
   !    end do
   ! end subroutine CalcPos 


end module Process 
