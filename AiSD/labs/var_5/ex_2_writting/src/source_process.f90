module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

    subroutine MovePartList(indexFirstLine, indexLastLine, indexToPaste, Code)
       integer, intent(in) :: indexFirstLine, indexLastLine
       integer, intent(inout) :: indexToPaste
       type(SourceLine), pointer, intent(inout) :: Code

       type(SourceLine), pointer :: CodeToInsert, TailToInsert
       TailToInsert => Null()

      ! type(SourceLine), pointer :: beforeFirstLine
      ! type(SourceLine), pointer :: lastLine
      ! type(SourceLine), pointer :: toPasteLine
      ! type(SourceLine), pointer :: tmp1
      ! type(SourceLine), pointer :: tmp2

       call CutSearchDiap(Code, CodeToInsert, TailToInsert, indexFirstLine, indexLastLine, indexToPaste, 1)
       call InsertDiap(Code, CodeToInsert, TailToInsert, indexToPaste, 1)
       
       

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

    pure recursive subroutine CutSearchDiap(line, CodeToInsert, TailToInsert, indexFirstLine, indexLastLine, indexToPaste, indexCur)
       type(SourceLine), pointer, intent(inout) :: line, CodeToInsert, TailToInsert
       integer, intent(inout) :: indexToPaste
       integer, intent(in) :: indexFirstLine, indexLastLine, indexCur

       if(indexFirstLine <= indexCur .and. indexCur <= indexLastLine) then
          call PutTail(CodeToInsert, TailToInsert, line)
          ! Сдвинуть индекс вставки
          if(indexToPaste > indexLastLine) &
              indexToPaste = indexToPaste - 1
       end if

       if(Associated(line%next)) &
         call CutSearchDiap(line%next,CodeToInsert , TailToInsert, indexFirstLine, indexLastLine, indexToPaste, indexCur+1)  

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

    pure subroutine PutTail(Code, Tail, line)
       type(SourceLine), pointer  :: Code, Tail, line

       type(SourceLine), pointer :: tmp
       
       if (.not. Associated(Code)) then
           ! Первый элемент
           allocate(Code)
           Code%String = line%String
           Tail => Code
       else if (.not. Associated(Tail%next)) then
           allocate (Tail%next)
           Tail%next%String = line%string
           ! Передвинуть хвост
           Tail => Tail%next
      end if
      ! Убрать элемент из списка 
      tmp => line
      line => line%next
      deallocate(tmp)

    end subroutine PutTail

    pure recursive subroutine InsertDiap(line, CodeToInsert, TailToInsert, indexToPaste, indexCur)
        type(SourceLine), pointer, intent(inout) :: line, CodeToInsert, TailToInsert
        integer, intent(in)                      :: indexToPaste, indexCur

        type(SourceLine), pointer :: tmp

        if(indexCur == indexToPaste) then
           tmp => line%next
           line%next => CodeToInsert
           TailToInsert%next => tmp
        else if(Associated(line%next)) then
            call InsertDiap(line%next, CodeToInsert, TailToInsert, indexToPaste, indexCur+1)
        end if

    end subroutine InsertDiap

end module Source_process
