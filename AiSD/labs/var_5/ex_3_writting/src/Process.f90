module Process 
   use Environment
   use IO
   implicit none

contains
   recursive subroutine Sort(SourceNode, SortedList)
      type(node), pointer, intent(inout) :: SourceNode
      type(node), pointer, intent(inout) :: SortedList

      write(*,*) SourceNode%string

      !call BindToSortedPart(sourceNode, SortedList)
      call test(sourceNode, SortedList)

      if(Associated(SourceNode%next)) &
         call Sort(SourceNode%next, SortedList)
   end subroutine Sort

   recursive subroutine test(nodeToBind, nodeSorted)
      type(node), target, intent(inout) :: nodeToBind
      type(node), pointer, intent(inout) :: nodeSorted
      !type(node), pointer :: tmp 
      
      !Пустое начало
      if(.not. Associated(nodeSorted)) then
          !nodeToBind%prev_len => nodeSorted
         nodeSorted => nodeToBind
      else if(LEN(nodeSorted%string) < LEN(nodeToBind%string)) then 
         call test(nodeToBind, nodeSorted%next_len)
      else 

         write(*,*) "s ",  nodeSorted%string, ", ", nodeToBind%string
         write(*,*) "L-", LEN(nodeSorted%string), " ", LEN(nodeToBind%string)

         nodeToBind%next_len => nodeSorted
         nodeSorted => nodeToBind
          

        ! tmp => nodeSorted%prev_len
        ! nodeSorted%prev_len => nodeToBind
        ! nodeToBind%prev_len => tmp
        ! 
        ! nodeToBind%next_len => nodeSorted

        ! tmp => nodeSorted%prev_len
        ! nodeToBind%prev_len => nodeSorted%prev_len
        ! nodeToBind%prev_len => nodeSorted
        ! tmp => nodeSorted
        ! nodeSorted => nodeToBind
        ! nodeToBind%next_len => tmp
          



     end if

   end subroutine

   recursive subroutine BindToSortedPart(nodeToBind, nodeSorted) 
      type(node), target, intent(inout) :: nodeToBind
      type(node), pointer, intent(inout) :: nodeSorted

!      type(node), pointer :: tmp

      if(.not. Associated(nodeSorted)) then 
          write(*,*) "add sort node"
         ! Eсли отсортированная часть закончилась
         nodeSorted => nodeToBind
         nodeSorted%next_len => Null()
      else if(LEN(nodeToBind%string) < LEN(nodeSorted%string)) then 
         ! Искать место дальше
          write(*,*) "search -", nodeToBind%string, nodeSorted%string
         call BindToSortedPart(nodeToBind, nodeSorted%next_len)
      else
          write(*,*) "bind search node -", nodeToBind%string, nodeSorted%string
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
