module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
   pure subroutine FirstForAlph(Group, searchStud)
      type(student), pointer, intent(inout) :: Group
      type(student), intent(inout)  :: searchStud  

      searchStud%sur = Group%sur
      searchStud%init = Group%init
      searchStud%date = Group%date

      call SearchFirstForAlph(Group, searchStud)
   end subroutine FirstForAlph 

   pure recursive subroutine SearchFirstForAlph(stud, searchStud)
      type(student), pointer, intent(inout) :: stud
      type(student), intent(inout) :: searchStud
      
      if(Associated(stud)) then
         if(stud%sur < searchStud%sur) then
            searchStud%sur = stud%sur
            searchStud%init = stud%init
            searchStud%date = stud%date
         end if
         call SearchFirstForAlph(stud%next, searchStud)
      end if
   end subroutine SearchFirstForAlph 

   pure subroutine Youngest(Group, searchStud)
      type(student), pointer, intent(inout) :: Group
      type(student), intent(inout)          :: searchStud

      call SearchYoungest(Group, searchStud)
   end subroutine Youngest

   pure recursive subroutine SearchYoungest(stud, searchStud)
      type(student), pointer, intent(inout) :: stud
      type(student), intent(inout)       :: searchStud
      
      if(Associated(stud)) then
         if(stud%date > searchStud%date) then
            searchStud%sur = stud%sur
            searchStud%init = stud%init
            searchStud%date = stud%date
         end if
         call SearchYoungest(stud%next, searchStud)
      end if
   end subroutine SearchYoungest 

end module SearchesGroup 
