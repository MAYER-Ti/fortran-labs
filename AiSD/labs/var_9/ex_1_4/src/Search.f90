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

end module SearchesGroup 
