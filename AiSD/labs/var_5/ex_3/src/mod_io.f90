
module mod_IO
   use Environment

   implicit none
   private

   public :: list, node 
   public :: ReadSortedList
   public :: WriteOrderedList
   public :: WriteUnorderedList

   type node
      character(:, kind=CH_), allocatable :: string
      type(node), pointer          :: next     => Null()
      type(node), pointer          :: next_len => Null()
   end type node
   type list
      type(node), pointer :: head
      type(node), pointer :: sorted
   end type

contains

   subroutine ReadSortedList(inputFile, List, SortedList)
      type(node), pointer        :: List, SortedList
      character(*), intent(in)   :: inputFile

      integer :: In

      open (file=inputFile, encoding=E_, newunit=In)
        call ReadSortedValue(In, List, SortedList)
      close (In)
   end subroutine ReadSortedList

   recursive subroutine ReadSortedValue(In, current, SortedList)
      type(node), pointer :: current, SortedList
      integer, intent(in) :: In

      integer, parameter      :: max_len = 1024
      character(max_len, CH_) :: string
      integer                 :: IO
      
      read(In, "(a)", iostat=IO) string
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
          allocate(current)
          current%string = Trim(string)
         call ReadSortedValue(In, current%next, SortedList)
      end if
   end subroutine ReadSortedValue
  
   subroutine WriteUnorderedList(outputFile, List, writeLetter, writePosition)
      character(*), intent(in) :: outputFile, writeLetter, writePosition
      type(node), pointer      :: List

      integer  :: Out
      
      open (file=outputFile, encoding=E_, position=writePosition, newunit=Out)
         write (out, '(a)') writeLetter
         call WriteUnorderedValue(Out, List)
      close (Out)
   end subroutine WriteUnorderedList

   recursive subroutine WriteUnorderedValue(Out, Elem)
      integer, intent(in) :: Out
      type(node), pointer :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(a)', iostat=IO) Elem%string 
         call Handle_IO_status(IO, "writing list")
         call WriteUnorderedValue(Out, Elem%next)
      end if
   end subroutine WriteUnorderedValue 
   
   subroutine WriteOrderedList(outputFile, SortedList, writeLetter, writePosition)
      character(*), intent(in) :: outputFile, writeLetter, writePosition
      type(node), pointer      :: SortedList

      integer  :: Out
      
      open (file=outputFile, encoding=E_, position=writePosition, newunit=Out)
         write (out, '(a)') writeLetter
         call WriteOrderedValue(Out, SortedList)
      close (Out)
   end subroutine WriteOrderedList  

   recursive subroutine WriteOrderedValue(Out, Elem)
      integer, intent(in) :: Out
      type(node), pointer :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(a)', iostat=IO) Elem%string 
         call Handle_IO_status(IO, "writing list")
         call WriteOrderedValue(Out, Elem%next_len)
      end if
   end subroutine WriteOrderedValue 
end module mod_IO 
