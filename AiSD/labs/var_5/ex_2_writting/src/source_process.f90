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

      ! type(SourceLine), pointer :: beforeFirstLine
      ! type(SourceLine), pointer :: lastLine
      ! type(SourceLine), pointer :: toPasteLine
      ! type(SourceLine), pointer :: tmp1
      ! type(SourceLine), pointer :: tmp2

       !call MoveToTail(CodeToInsert, TailToInsert, Code)
       call CutSearchDiap(Code, CodeToInsert, TailToInsert, indexFirstLine, indexLastLine, indexToPaste)
       !call WriteCode("Output.txt", CodeToInsert, "append", "--------- test ----------")
       call InsertDiap(Code, CodeToInsert, TailToInsert, indexToPaste)
       !call WriteCode("Output.txt", Code, "append", "--------- test source ----------")
       
       

      ! call GetLineOnIndex(Code, indexFirstLine-1, 1, beforeFirstLine)
      ! call GetLineOnIndex(beforeFirstLine, indexLastLine, indexFirstLine-1, lastLine)
      ! call GetLineOnIndex(Code, indexToPaste, 1, toPasteLine)
      ! 
      ! ! Запомнить элемент после Paste
      !  tmp2 => toPasteLine%next 
      ! ! call move_alloc(toPasteLine%next, tmp2)
      ! ! Элемент куда вставить ссылается на First
      ! ! call move_alloc(beforeFirstLine%next, toPasteLine%next)
      ! toPasteLine%next => beforeFirstLine%next
      ! ! Запомнить ссылку на элем. после Last
      ! tmp1 => lastLine%next
      ! !call move_alloc(lastLine%next, tmp1)
      ! ! Элем. после Last ссылается на элемент после Paste
      ! !call move_alloc(tmp2, lastLine%next)
      ! lastLine%next => tmp2 
      ! ! Элемент перед First ссылается на следующий после Last
      ! beforeFirstLine%next => tmp1
      ! !call move_alloc(tmp1, beforeFirstLine%next)

    end subroutine MovePartList 

    pure subroutine CutSearchDiap(SourceCode, CodeToInsert, TailToInsert, indexFirstLine, indexLastLine, indexToPaste)
       type(SourceLine), pointer, intent(inout) :: SourceCode, CodeToInsert, TailToInsert
       integer, intent(inout) :: indexToPaste
       integer, intent(in) :: indexFirstLine, indexLastLine

       integer :: indexCur
       type(SourceLine), pointer :: curLine
       type(SourceLine), pointer :: tmpBeforeList, tmpAfterList

       indexCur = 1
       curLine => SourceCode
       do while (Associated(curLine))
          if(indexCur == indexFirstLine-1) then
             CodeToInsert  => curLine%next
             tmpBeforeList => curLine
              
          else if(indexCur == indexLastLine) then 
             TailToInsert => curLine
             tmpAfterList => curLine%next
             curLine%next => Null()
             exit
          end if
          curLine => curLine%next
          indexCur = indexCur + 1
       end do

       tmpBeforeList%next => tmpAfterList%next

       if(indexToPaste > indexLastLine) &
           indexToPaste = indexToPaste-2 - (indexLastLine+1-indexFirstLine)

      ! if(indexFirstLine <= indexCur .and. indexCur <= indexLastLine) then
      !    ! Переместить line в CodeToInsert
      !    call MoveToTail(CodeToInsert, TailToInsert, line)
      !    ! Сдвинуть индекс вставки
      !    if(indexToPaste > indexLastLine) indexToPaste = indexToPaste - 1
      !    ! После перемещения ссылка line уже передвинута
      !    call CutSearchDiap(line%next, CodeToInsert, TailToInsert, indexFirstLine, indexLastLine, indexToPaste, indexCur+1) 
      ! else  if (Associated(line%next)) then 
      !   call CutSearchDiap(line%next,CodeToInsert, TailToInsert, indexFirstLine, indexLastLine, indexToPaste, indexCur+1)  
      ! end if

    end subroutine CutSearchDiap 
!
!    pure recursive subroutine GetLineOnIndex(line, searchIndex, indexCur, searchLine)
!       integer, intent(in) :: searchIndex, indexCur
!       type(SourceLine), pointer, intent(inout) :: line, searchLine
!
!       if(indexCur == searchIndex) then
!          searchLine => line
!       end if
!
!       if (Associated(line%next) .and. (indexCur < searchIndex)) &
!          call GetLineOnIndex(line%next, searchIndex, indexCur+1, searchLine) 
!    end subroutine GetLineOnIndex

!    pure subroutine MoveToTail(Head, Tail, Elem)
!       type(SourceLine), pointer  :: Head, Tail, Elem
!
!       type(SourceLine), pointer :: tmp
!       
!       if (.not. Associated(Head)) then
!          Head => Elem
!          Tail => Elem
!       else 
!          Tail => Elem
!       end if
!       
!      ! if (.not. Associated(Code)) then
!      !     ! Первый элемент
!      !     allocate(Code)
!      !     Code%String = line%String
!      !     Tail => Code
!      ! else if (.not. Associated(Tail%next)) then
!      !     allocate (Tail%next)
!      !     Tail%next%String = line%string
!      !     ! Передвинуть хвост
!      !     Tail => Tail%next
!      ! end if
!       ! Убрать элемент из списка 
!       tmp => Elem
!       Elem => Elem%next
!       deallocate(tmp)
!
!    end subroutine MoveToTail

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


       !   
       ! if(indexCur == indexToPaste) then
       !    tmp => line%next
       !    line%next => CodeToInsert
       !    TailToInsert%next => tmp
       ! else if(Associated(line%next)) then
       !     call InsertDiap(line%next, CodeToInsert, TailToInsert, indexToPaste, indexCur+1)
       ! end if

    end subroutine InsertDiap

end module Source_process
