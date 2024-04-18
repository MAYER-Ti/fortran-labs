module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
    pure function SearchFirstForAlph(Surnames) result(searchIndex) 
      character(kind=CH_), intent(in)  :: Surnames(SURNAME_LEN, GROUP_COUNT)
 
      integer :: searchIndex, i, j 

      searchIndex = 1
      do i = 2, GROUP_COUNT
         do j = 1, SURNAME_LEN
            ! Если символ одинаковый, то сравнивать следующий
            if (Surnames(j,searchIndex) == Surnames(j,i)) then
                cycle
            end if 
            ! Если символ меньше, то запомнить индекс 
            if (Surnames(j,searchIndex) > Surnames(j,i)) then
               searchIndex = i 
            end if 
            ! Если символы не равны, то в любом случае переходим к след. студенту
            exit
         end do
      end do
 
   end function SearchFirstForAlph 

   pure function SearchYoungest(Dates) result(searchIndex)
      integer, intent(in) :: Dates(GROUP_COUNT)
 
      integer :: searchIndex  

      searchIndex = MaxLoc(Dates, 1)
      
   end function SearchYoungest

end module SearchesGroup 
