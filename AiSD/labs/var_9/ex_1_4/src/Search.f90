module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
   pure function SearchFirstForAlph(surnames) result(searchIndex)
      character(SURNAME_LEN, kind=CH_), intent(in) :: surnames(GROUP_COUNT) 

      integer :: searchIndex, i  

      searchIndex = 1
      do i = 2, GROUP_COUNT
         if(surnames(searchIndex) > surnames(i)) then
            searchIndex = i
         end if 
      end do
      
   end function SearchFirstForAlph 

   pure function SearchYoungest(dates) result(searchIndex)
      integer, intent(in) :: dates(GROUP_COUNT)

      integer :: searchIndex  

      searchIndex = MaxLoc(dates, 1)
      ! При MinLoc(Surnames,1) происходит неправильный поиск 
      
   end function SearchYoungest

end module SearchesGroup 
