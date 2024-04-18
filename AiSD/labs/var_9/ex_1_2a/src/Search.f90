module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
    subroutine SearchFirstForAlph(Surnames, Initials, Dates, outSurname, outInitial, outDate)
      character(kind=CH_), intent(in)  :: Surnames(GROUP_COUNT, SURNAME_LEN)
      character(kind=CH_), intent(in)  :: Initials(GROUP_COUNT, INITIALS_LEN)
      integer, intent(in)              :: Dates(GROUP_COUNT)
      character(kind=CH_), intent(out) :: outSurname(SURNAME_LEN) 
      character(kind=CH_), intent(out) :: outInitial(INITIALS_LEN) 
      integer, intent(out)             :: outDate
 
      integer :: searchIndex  
      searchIndex = FindLoc(Surnames(:,1), MinVal(Surnames(:,1), 1), 1) 
      ! При MinLoc(Surnames(1:),1) происходит неправильный поиск 

      outSurname = Surnames(searchIndex, :)  
      outInitial = Initials(searchIndex, :)
      outDate    = Dates(searchIndex)
 
   end subroutine SearchFirstForAlph 

   pure subroutine SearchYoungest(Surnames, Initials, Dates, outSurname, OutInitial, OutDate)
      character(kind=CH_), intent(in)  :: Surnames(GROUP_COUNT, SURNAME_LEN)
      character(kind=CH_), intent(in)  :: Initials(GROUP_COUNT, INITIALS_LEN)
      integer, intent(in)              :: Dates(GROUP_COUNT)
      character(kind=CH_), intent(out) :: outSurname(SURNAME_LEN)
      character(kind=CH_), intent(out) :: outInitial(INITIALS_LEN)
      integer, intent(out)             :: outDate
 
      integer :: searchIndex  

      searchIndex = MaxLoc(Dates, 1)
      ! При MinLoc(Surnames,1) происходит неправильный поиск 
      
      outSurname = Surnames(searchIndex, :)  
      outInitial = Initials(searchIndex, :)
      outDate    = Dates(searchIndex)
 
   end subroutine SearchYoungest

end module SearchesGroup 
