module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

    pure subroutine MovePartList(indexFirstLine, indexLastLine, indexToPaste, Code)
       integer, intent(in) :: indexFirstLine, indexLastLine
       integer, intent(inout) :: indexToPaste
       type(SourceLine), pointer, intent(inout) :: Code

       type(SourceLine), pointer :: CodeToInsert, TailToInsert

       call CutSearchDiap(Code, CodeToInsert, TailToInsert, indexFirstLine, indexLastLine, indexToPaste)
       call InsertDiap(Code, CodeToInsert, TailToInsert, indexToPaste)

    end subroutine MovePartList 

    pure subroutine CutSearchDiap(SourceCode, CodeToInsert, TailToInsert, indexFirstLine, indexLastLine, indexToPaste)
       type(SourceLine), pointer, intent(inout) :: SourceCode, CodeToInsert, TailToInsert
       integer, intent(inout) :: indexToPaste
       integer, intent(in) :: indexFirstLine, indexLastLine

       type(SourceLine), pointer :: tmpBeforeList, tmpAfterList

      call SearchDiap(SourceCode, CodeToInsert, TailToInsert, &
                      indexFirstLine, indexLastLine, &
                      tmpBeforeList, tmpAfterList, 1)

       tmpBeforeList%next => tmpAfterList%next

       if(indexToPaste > indexLastLine) &
           indexToPaste = indexToPaste-2 - (indexLastLine+1-indexFirstLine)
    end subroutine CutSearchDiap 

    pure recursive subroutine SearchDiap(SourceCodeLine, CodeToInsert, TailToInsert, &
                                         indexFirstLine, indexLastLine, &
                                         beforeList, afterList, indexCur)
       type(SourceLine), pointer, intent(inout) :: SourceCodeLine, CodeToInsert, TailToInsert
       integer, intent(in)                      :: indexFirstLine, indexLastLine, indexCur
       type(SourceLine), pointer, intent(inout) :: beforeList, afterList
       
          if(indexCur == indexFirstLine-1) then
             CodeToInsert  => SourceCodeLine%next
             beforeList => SourceCodeLine
              
          else if(indexCur == indexLastLine) then 
             TailToInsert => SourceCodeLine
             afterList => SourceCodeLine%next
             SourceCodeLine%next => Null()
          end if

       if(Associated(SourceCodeLine%next) .and. (indexCur < indexLastLine)) &
           call SearchDiap(SourceCodeLine%next, CodeToInsert, TailToInsert, &
                           indexFirstLine, indexLastLine, &
                           beforeList, afterList, indexCur+1)

    end subroutine SearchDiap
    pure subroutine InsertDiap(Code, CodeToInsert, TailToInsert, indexToPaste)
        type(SourceLine), pointer, intent(inout) :: Code, CodeToInsert, TailToInsert
        integer, intent(in)                      :: indexToPaste

        integer :: indexCur
        type(SourceLine), pointer :: curLine, tmp

        curLine => Code
        indexCur = 0
        if(indexToPaste == 0) then 
           tmp  => Code
           Code => CodeToInsert
           TailToInsert%next => tmp
       else
           do while(Associated(curLine)) 
              if(indexCur == indexToPaste) then 
                 tmp => curLine%next
                 curLine%next => CodeToInsert
                 TailToInsert%next => tmp
                 exit
              end if
              curLine => curLine%next
              indexCur = indexCur + 1
           end do
        end if 
    end subroutine InsertDiap

end module Source_process
