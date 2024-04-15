module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
   pure subroutine SearchFirstForAlph(Surnames, Initials, Dates, outSurname, outInitial, outDate)
      character(SURNAME_LEN, kind=CH_), intent(in)   :: Surnames(GROUP_COUNT)
      character(INITIALS_LEN, kind=CH_), intent(in)  :: Initials(GROUP_COUNT)
      integer, intent(in)                            :: Dates(GROUP_COUNT)
      character(SURNAME_LEN, kind=CH_), intent(out)  :: outSurname
      character(INITIALS_LEN, kind=CH_), intent(out) :: outInitial
      integer, intent(out)                           :: outDate
 
      integer :: searchIndex  

      searchIndex = FindLoc(Surnames, MinVal(Surnames, 1), 1)
      ! При MinLoc(Surnames,1) происходит неправильный поиск 
      
      outSurname = Surnames(searchIndex)  
      outInitial = Initials(searchIndex)
      outDate    = Dates(searchIndex)
 
   end subroutine SearchFirstForAlph 

   pure subroutine SearchYoungest(Surnames, Initials, Dates, outSurname, OutInitial, OutDate)
      character(SURNAME_LEN, kind=CH_), intent(in)   :: Surnames(GROUP_COUNT)
      character(INITIALS_LEN, kind=CH_), intent(in)  :: Initials(GROUP_COUNT)
      integer, intent(in)                            :: Dates(GROUP_COUNT)
      character(SURNAME_LEN, kind=CH_), intent(out)  :: outSurname
      character(INITIALS_LEN, kind=CH_), intent(out) :: outInitial
      integer, intent(out)                           :: outDate
 
      integer :: searchIndex  

      searchIndex = MaxLoc(Dates, 1)
      ! При MinLoc(Surnames,1) происходит неправильный поиск 
      
      outSurname = Surnames(searchIndex)  
      outInitial = Initials(searchIndex)
      outDate    = Dates(searchIndex)
 
   end subroutine SearchYoungest

end module SearchesGroup 
