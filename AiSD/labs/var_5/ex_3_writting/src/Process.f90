module Process 
   use Environment
   use IO
   implicit none

contains
   recursive subroutine Sort(SourceNode, SortedList)
      type(node), pointer, intent(inout) :: SourceNode
      type(node), pointer, intent(inout) :: SortedList

      call BindToSortedPart(sourceNode, SortedList)

      if(Associated(SourceNode%next)) &
         call Sort(SourceNode%next, SortedList)
   end subroutine Sort

   recursive subroutine BindToSortedPart(nodeToBind, nodeSorted) 
      type(node), target, intent(inout) :: nodeToBind
      type(node), pointer, intent(inout) :: nodeSorted

      type(node), pointer :: tmp

      if(.not. Associated(nodeSorted)) then 
         ! Eсли отсортированная часть закончилась
         nodeSorted => nodeToBind
      else if(LEN(nodeToBind%string) >= LEN(nodeSorted%string)) then 
         ! Искать место дальше
         call BindToSortedPart(nodeToBind, nodeSorted%next_len)
      else
         ! Вставить в отсортированной части
         nodeToBind%next_len => nodeSorted
         nodeSorted => nodeToBind
       ! tmp => nodeSorted
       ! nodeSorted => nodeToBind
       ! nodeSorted%next_len => nodeToBind
          






       !  tmp => nodeSorted
       ! ! tmp2 => nodeSorted
       !  nodeSorted => nodeToBind
       !  nodeSorted%next_len => tmp
      end if

      

   end subroutine BindToSortedPart

end module Process 
