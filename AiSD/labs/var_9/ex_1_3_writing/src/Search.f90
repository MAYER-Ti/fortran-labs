module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
   pure function SearchFirstForAlph(Group) result(stud)
      type(student), intent(in) :: Group(GROUP_COUNT)

      type(student) :: stud 
      integer :: searchIndex  

      searchIndex = FindLoc(Group(:)%sur, MinVal(Group(:)%sur, 1), 1)
      ! При MinLoc(Surnames,1) происходит неправильный поиск 
      
      stud = Group(searchIndex) 
   end function SearchFirstForAlph 

   pure function SearchYoungest(Group) result(stud)
      type(student), intent(in) :: Group(GROUP_COUNT)

      type(student) :: stud
 
      integer :: searchIndex  

      searchIndex = MaxLoc(Group(:)%date, 1)
      ! При MinLoc(Surnames,1) происходит неправильный поиск 
      
      stud = Group(searchIndex) 
 
   end function SearchYoungest

end module SearchesGroup 
