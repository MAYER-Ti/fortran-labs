
module mod_IO
   use Environment

   implicit none

   type node
      character(:, kind=CH_), allocatable :: string
      type(node), pointer                 :: next => Null()
   end type node

contains

   function ReadList(inputFile) result(List)
      character(*), intent(in) :: inputFile

      type(node), pointer      :: List
      integer :: In

      open (file=inputFile, encoding=E_, newunit=In)
        call ReadValue(In, List)
      close (In)
   end function ReadList

   recursive subroutine ReadValue(In, current)
      type(node), pointer :: current
      integer, intent(in) :: In

      integer, parameter      :: max_len = 1024
      character(max_len, CH_) :: string
      integer                 :: IO
      
      read(In, "(a)", iostat=IO) string
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
          allocate(current)
          current%string = Trim(string)
         call ReadValue(In, current%next)
      end if
   end subroutine ReadValue
  
   subroutine WriteList(outputFile, List, writeLetter, writePosition)
      character(*), intent(in) :: outputFile, writeLetter, writePosition
      type(node), pointer      :: List

      integer  :: Out
      
      open (file=outputFile, encoding=E_, position=writePosition, newunit=Out)
         write (out, '(a)') writeLetter
         call WriteValue(Out, List)
      close (Out)
   end subroutine WriteList

   recursive subroutine WriteValue(Out, Elem)
      integer, intent(in) :: Out
      type(node), pointer :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(a)', iostat=IO) Elem%string 
         call Handle_IO_status(IO, "writing list")
         call WriteValue(Out, Elem%next)
      end if
   end subroutine WriteValue 
end module mod_IO 
