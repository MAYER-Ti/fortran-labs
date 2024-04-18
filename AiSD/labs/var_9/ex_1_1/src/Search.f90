module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
   pure function SearchFirstForAlph(Surnames) result(searchIndex)
      character(SURNAME_LEN, kind=CH_), intent(in) :: Surnames(GROUP_COUNT)
 
      integer :: searchIndex, i  
      
      !searchIndex = FindLoc(Surnames, MinVal(Surnames, 1), 1)
      !searchIndex = maxLoc(Surnames(:), 1) 
      searchIndex = 1
      do i = 2, GROUP_COUNT
         if(Surnames(searchIndex) > Surnames(i)) then
            searchIndex = i
         end if 
      end do
      
   end function SearchFirstForAlph 

   pure function SearchYoungest(Dates) result(searchIndex)
      integer, intent(in) :: Dates(GROUP_COUNT)
 
      integer :: searchIndex 

      searchIndex = MaxLoc(Dates, 1)
 
   end function SearchYoungest

end module SearchesGroup 
