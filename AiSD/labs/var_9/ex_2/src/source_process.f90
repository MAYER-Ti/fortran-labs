module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

    pure subroutine MovePartList(F, L, M, List)
       integer, intent(in) :: F, L 
       integer, intent(inout) :: M 
       type(line), pointer, intent(inout) :: List 

       type(line), pointer :: ListPart, TailPart

       call CutPart(List, ListPart, TailPart, F, L, M)
       call InsertPart(List, ListPart, TailPart, M)

    end subroutine MovePartList 

    pure subroutine CutPart(List, ListPart, TailPart, F, L, M)
       type(line), pointer, intent(inout) :: List, ListPart,TailPart 
       integer, intent(inout) :: M 
       integer, intent(in) :: F, L 

       type(line), pointer :: tmpBeforeList, tmpAfterList

      call SearchPart(List, ListPart, TailPart, F, L, tmpBeforeList, tmpAfterList, 1)

       tmpBeforeList%next => tmpAfterList

       if(M > L) &
           M= M-2-(L+1-F)
    end subroutine CutPart 

    pure recursive subroutine SearchPart(curNode, ListPart, TailPart, F, L, beforeList, afterList, indexCur)
       type(line), pointer, intent(inout) :: curNode, ListPart, TailPart 
       integer, intent(in)                      :: F, L, indexCur
       type(line), pointer, intent(inout) :: beforeList, afterList
       
          if(indexCur == F-1) then
             ListPart => curNode%next
             beforeList => curNode
          else if(indexCur == L) then 
             TailPart => curNode 
             afterList => curNode%next
             curNode%next => Null()
          end if

       if(Associated(curNode%next) .and. (indexCur < L)) &
           call SearchPart(curNode%next, ListPart, TailPart, F, L, beforeList, afterList, indexCur+1)

    end subroutine SearchPart
    pure subroutine InsertPart(List, ListPart, TailPart, M)
        type(line), pointer, intent(inout) :: List, ListPart, TailPart 
        integer, intent(in)                      :: M 

        type(line), pointer :: tmp

        if(M == 0) then 
           tmp  => List
           List => ListPart 
           TailPart%next => tmp
       else
           call InsertPart_val(List, ListPart, TailPart, &
                               M, 0)
        end if 
    end subroutine InsertPart

    pure recursive subroutine InsertPart_val(curNode, ListPart, TailPart, M, indexCur)
       type(line), pointer, intent(inout) :: curNode, ListPart, TailPart
       integer, intent(in)                      :: M, indexCur

       type(line), pointer :: tmp

       if(Associated(curNode)) then
          if(indexCur == M) then 
             tmp => curNode%next
             curNode%next => ListPart 
             TailPart%next => tmp
          else
             call InsertPart_val(curNode%next, ListPart, TailPart, &
                                 M, indexCur+1)
          end if
       end if
    end subroutine InsertPart_val

end module Source_process
