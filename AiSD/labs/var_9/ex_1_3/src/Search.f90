module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
   pure function SearchFirstForAlph(Group) result(searchIndex)
      type(student), intent(in) :: Group(GROUP_COUNT)

      integer :: searchIndex, i  

      searchIndex = 1
      do i = 2, GROUP_COUNT
         if(Group(searchIndex)%sur > Group(i)%sur) then
            searchIndex = i
         end if 
      end do
      
   end function SearchFirstForAlph 

   pure function SearchYoungest(Group) result(searchIndex)
      type(student), intent(in) :: Group(GROUP_COUNT)

      integer :: searchIndex  

      searchIndex = MaxLoc(Group(:)%date, 1)
      ! При MinLoc(Surnames,1) происходит неправильный поиск 
      
   end function SearchYoungest

end module SearchesGroup 
