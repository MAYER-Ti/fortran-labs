module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

    subroutine MovePartList(indexFirstLine, indexLastLine, indexToPaste, Code)
       integer, intent(in) :: indexFirstLine, indexLastLine, indexToPaste
       type(SourceLine), allocatable, target, intent(inout) :: Code

       type(SourceLine), allocatable :: beforeFirstLine
       type(SourceLine), allocatable :: lastLine
       type(SourceLine), allocatable :: toPasteLine
       type(SourceLine), allocatable :: tmp1
       type(SourceLine), allocatable :: tmp2

       call GetLineOnIndex(Code, indexFirstLine-1, 1, beforeFirstLine)
       call GetLineOnIndex(Code, indexLastLine,    1, lastLine)
       call GetLineOnIndex(Code, indexToPaste,     1, toPasteLine)
       
       ! Запомнить элемент после Paste
!       ! tmp2 = toPasteLine%next 
!       call move_alloc(toPasteLine%next, tmp2)
!       ! Элемент куда вставить ссылается на First
!       call move_alloc(beforeFirstLine%next, toPasteLine%next)
!       !toPasteLine%next => beforeFirstLine%next
!       ! Запомнить ссылку на элем. после Last
!       !tmp1 = lastLine%next
!       call move_alloc(lastLine%next, tmp1)
!       ! Элем. после Last ссылается на элемент после Paste
!       call move_alloc(tmp2, lastLine%next)
!       !lastLine%next => tmp2 
!       ! Элемент перед First ссылается на следующий после Last
!       !beforeFirstLine%next => tmp1
!       call move_alloc(tmp1, beforeFirstLine%next)

    end subroutine MovePartList 

    pure recursive subroutine GetLineOnIndex(line, searchIndex, indexCur, searchLine)
       integer, intent(in) :: searchIndex, indexCur
       type(SourceLine), allocatable, intent(inout) :: line
       type(SourceLine), allocatable, intent(inout) :: searchLine

       if(indexCur == searchIndex) then
          searchLine%string = line
       end if

       if (Allocated(line%next) .and. (indexCur < searchIndex)) &
          call GetLineOnIndex(line%next, searchIndex, indexCur+1, searchLine) 
    end subroutine GetLineOnIndex

end module Source_process
