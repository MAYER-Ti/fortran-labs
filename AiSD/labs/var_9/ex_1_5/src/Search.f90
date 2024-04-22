module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
   pure function SearchFirstForAlph(surnames) result(searchIndex)
      character(SURNAME_LEN, kind=CH_), intent(in) :: surnames(GROUP_COUNT)

      integer :: searchIndex  

      searchIndex = 1
      call SearchFirstForAlphVal(surnames, searchIndex, 2)
            
   end function SearchFirstForAlph 
   pure recursive subroutine SearchFirstForAlphVal(surnames, searchIndex, i)
      character(SURNAME_LEN, kind=CH_), intent(in) :: surnames(GROUP_COUNT)
      integer, intent(inout)                       :: searchIndex
      integer, intent(in)                          :: i

      if(surnames(searchIndex) > surnames(i)) then
         searchIndex = i
      end if

      if(i < GROUP_COUNT) then
         call SearchFirstForAlphVal(surnames, searchIndex, i+1)
      end if

   end subroutine SearchFirstForAlphVal 

   pure function SearchYoungest(dates) result(searchIndex)
      integer, intent(in) :: dates(GROUP_COUNT) 

      integer :: searchIndex  

      searchIndex = MaxLoc(dates, 1)
      
   end function SearchYoungest

end module SearchesGroup 
