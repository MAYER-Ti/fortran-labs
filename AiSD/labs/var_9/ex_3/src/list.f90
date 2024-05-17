module mod_list
   use Environment

   implicit none
   private

   type, public :: node
      character(:, kind=CH_), allocatable :: string
      type(node), allocatable :: next    
   end type node

   type, public :: list
      type(node), allocatable :: head
      contains
          procedure, public :: ReadList
          procedure, public :: WriteList
          procedure, public :: Sort
          final             :: Delete
   end type

contains
   subroutine Sort(dList)
      class(list), intent(inout) :: dList

      type(node), allocatable :: SortedList

      call InsertionSort(dList%head, SortedList)
      call move_alloc(SortedList, dList%head)

   end subroutine Sort

   recursive subroutine InsertionSort(current, ListSort)
      type(node), allocatable, intent(inout) :: current
      type(node), allocatable, intent(inout) :: ListSort


      if(Allocated(current)) then 

         call SortStep(ListSort, current)

         call InsertionSort(current%next, ListSort)
      end if

   end subroutine InsertionSort

   pure recursive subroutine SortStep(curSorted, nodeToInsert)
      type(node), allocatable, intent(inout) :: nodeToInsert
      type(node), allocatable, intent(inout) :: curSorted

      type(node), allocatable :: tmp, tmp2

      if (.not. Allocated(curSorted)) then
         ! Либо голова не размещена,
         ! либо дошли до поледнего элемента.
         call move_alloc(nodeToInsert%next, tmp)
         call move_alloc(nodeToInsert, curSorted)
         call move_alloc(tmp, nodeToInsert)
      else if (nodeToInsert%string <= curSorted%string) then
         call SortStep(curSorted%next, nodeToInsert)
      else
         call move_alloc(nodeToInsert%next, tmp)
         call move_alloc(curSorted, tmp2)
         call move_alloc(nodeToInsert, curSorted)
         call move_alloc(tmp2, curSorted%next)
         call move_alloc(tmp, nodeToInsert)
      end if
   end subroutine SortStep

   pure subroutine Delete(dList)

      type(list), intent(inout) :: dList
      deallocate(dList%head)
   end subroutine Delete

   subroutine ReadList(dList, inputFile)
      character(*), intent(in) :: inputFile
      class(list)              :: dList

      integer :: In

      open (file=inputFile, encoding=E_, newunit=In)
        call ReadValue(In, dList%head)
      close (In)
   end subroutine ReadList

   recursive subroutine ReadValue(In, current)
      type(node), allocatable, target :: current
      integer, intent(in)             :: In

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
  
   subroutine WriteList(dList, outputFile, writeLetter, writePosition)
      character(*), intent(in) :: outputFile, writeLetter, writePosition
      class(list)              :: dList

      integer  :: Out
      
      open (file=outputFile, encoding=E_, position=writePosition, newunit=Out)
         write (out, '(/a)') writeLetter
         call WriteValue(Out, dList%head)
      close (Out)
   end subroutine WriteList

   recursive subroutine WriteValue(Out, Elem)
      integer, intent(in)     :: Out
      type(node), allocatable :: Elem
      
      integer  :: IO

      if (Allocated(Elem)) then 
         write (Out, '(a)', iostat=IO) Elem%string 
         call Handle_IO_status(IO, "writing list")
         call WriteValue(Out, Elem%next)
      end if
   end subroutine WriteValue 
   
end module mod_list 
