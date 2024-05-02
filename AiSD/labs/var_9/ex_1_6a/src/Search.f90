module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains

   pure recursive subroutine FirstForAlph(searchStud, stud, firstStud)
      type(student), pointer, intent(inout) :: stud, firstStud, searchStud

      if(Associated(stud)) then
         if(stud%sur < firstStud%sur) then
            searchStud => stud
            call FirstForAlph(searchStud, stud%next, stud)
         else 
            searchStud => firstStud
            call FirstForAlph(searchStud, stud%next, firstStud)
         end if
      end if
   end subroutine FirstForAlph 

   pure recursive subroutine Youngest(searchStud, stud, youngestStud)
      type(student), pointer, intent(inout) :: stud, youngestStud,searchStud
      
      if(Associated(stud)) then
         if(stud%date > youngestStud%date) then
            searchStud => stud
            call Youngest(searchStud, stud%next, stud)
        else
            searchStud => youngestStud
            call Youngest(searchStud, stud%next, youngestStud)
         end if
      end if
   end subroutine Youngest 

end module SearchesGroup 
