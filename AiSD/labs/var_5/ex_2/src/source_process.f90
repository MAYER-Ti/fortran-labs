module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

    subroutine MovePartList(indexFirstLine, indexLastLine, indexToPaste, Code)
       integer, intent(in) :: indexFirstLine, indexLastLine, indexToPaste
       type(SourceLine), pointer, intent(inout) :: Code

       type(SourceLine), pointer :: beforeFirstLine
       type(SourceLine), pointer :: lastLine
       type(SourceLine), pointer :: toPasteLine
       type(SourceLine), pointer :: tmp1
       type(SourceLine), pointer :: tmp2

       call GetLineOnIndex(Code, indexFirstLine-1, 1, beforeFirstLine)
       call GetLineOnIndex(beforeFirstLine, indexLastLine, indexFirstLine-1, lastLine)
       call GetLineOnIndex(Code, indexToPaste, 1, toPasteLine)
       
       ! Запомнить ссылку на элем. после Last
       tmp1 => lastLine%next
       ! Запомнить элемент после Paste
       tmp2 => toPasteLine%next 
       ! Элемент куда вставить ссылается на First
       toPasteLine%next => beforeFirstLine%next
       ! Элем. после Last ссылается на элемент после Paste
       lastLine%next => tmp2 
       ! Элемент перед First ссылается на следующий после Last
       beforeFirstLine%next => tmp1

    end subroutine MovePartList 

    pure recursive subroutine GetLineOnIndex(line, searchIndex, indexCur, searchLine)
       integer, intent(in) :: searchIndex, indexCur
       type(SourceLine), target, intent(inout) :: line
       type(SourceLine), pointer, intent(inout) :: searchLine

       if(indexCur == searchIndex) then
          searchLine => line
       end if

       if (Associated(line%next) .and. (indexCur < searchIndex)) &
          call GetLineOnIndex(line%next, searchIndex, indexCur+1, searchLine) 
    end subroutine GetLineOnIndex

end module Source_process
