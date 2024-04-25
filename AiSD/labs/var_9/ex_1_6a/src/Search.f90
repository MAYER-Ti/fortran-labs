module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains

   pure recursive subroutine FirstForAlph(stud, searchStud)
      type(student), pointer, intent(inout) :: stud
      type(student), pointer, intent(inout) :: searchStud
      
      if(Associated(stud)) then
         if(stud%sur < searchStud%sur) then
            searchStud => stud
         end if
         call FirstForAlph(stud%next, searchStud)
      end if
   end subroutine FirstForAlph 

   pure recursive subroutine Youngest(stud, searchStud)
      type(student), pointer, intent(inout)  :: stud
      type(student), pointer, intent(inout) :: searchStud
      
      if(Associated(stud)) then
         if(stud%date > searchStud%date) then
            searchStud => stud
         end if
         call Youngest(stud%next, searchStud)
      end if
   end subroutine Youngest 

end module SearchesGroup 
