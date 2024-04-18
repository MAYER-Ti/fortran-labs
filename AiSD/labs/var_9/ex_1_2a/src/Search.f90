module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
    pure function SearchFirstForAlph(Surnames) result(searchIndex)
      character(kind=CH_), intent(in) :: Surnames(GROUP_COUNT, SURNAME_LEN)
 
      integer :: searchIndex, i, j 
      ! i - Номер студента, j - номер символа в фамилии

      ! searchIndex = FindLoc(Surnames(:,1), MinVal(Surnames(:,1), 1), 1) 
      ! При MinLoc(Surnames(1:),1) происходит неправильный поиск 

      searchIndex = 1
      do i = 2, GROUP_COUNT
         do j = 1, SURNAME_LEN
            ! Если символ одинаковый, то сравнивать следующий
            if (Surnames(searchIndex,j) == Surnames(i,j)) then
                cycle
            end if 
            ! Если символ меньше, то запомнить индекс 
            if (Surnames(searchIndex,j) > Surnames(i,j)) then
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
