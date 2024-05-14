module Source_IO
   use Environment

   implicit none
   
   ! Структура данных для хранения строки исходного текста.
   type line
      character(:, CH_), allocatable :: string
      type(line), pointer      :: next
   end type line

contains
   ! Чтение исходного кода. 
   function ReadList(InputFile) result (List)
      character(*), intent(in) :: InputFile

      integer             :: In
      type(line), pointer :: List
      
      open (file=InputFile, encoding=E_, newunit=In)
         call ReadNode(in, List)
      close (In)
   end function ReadList

   ! Чтение строки исходного кода.
   recursive subroutine ReadNode(in, node)
      type(line), pointer :: node
      integer, intent(in)       :: In

      integer, parameter        :: max_len = 1024
      character(max_len, CH_)   :: string
      integer                   :: IO

      ! Чтение строки во временную строку бОльшей длины.
      read (In, "(a)", iostat=IO) string
      call Handle_IO_Status(IO, "reading line from source code")
      if (IO == 0) then
         allocate (node)
         ! Хранение в размещаемом поле символов без завершающих пробелов.
         node%string = Trim(string)
         call ReadNode(In, node%Next)
      end if
   end subroutine ReadNode
 
   ! Вывод исходного кода.
   subroutine WriteList(outputFile, List, writePosition, writeLetter)
      character(*), intent(in)              :: outputFile, writePosition, writeLetter 
      type(line), pointer, intent(in) :: List

      integer  :: Out
      
      open (file=outputFile, encoding=E_, position=writePosition, newunit=Out)
         write (Out, '(/,a)') writeLetter 
         call WriteNode(Out, List)
      close (Out)

   end subroutine WriteList

   ! Вывод строки исходного кода.
   recursive subroutine WriteNode(Out, node)
      integer, intent(in)                   :: Out
      type(line), pointer, intent(in) :: node

      integer  :: IO

      write (Out, "(a)", iostat=IO) node%String
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(node%next)) &
         call WriteNode(Out, node%next)

   end subroutine WriteNode

   subroutine ReadInput(input_file, F, L, M)
      character(*), intent(in) :: input_file
      integer, intent(out)     :: F, L, M 

      integer :: In, IO 

      open (file=input_file, encoding=E_, newunit=In)
         read (In, '(i2, 1x, i2, 1x, i2)', iostat=IO) F, L, M 
         call Handle_IO_Status(IO, 'reading indexes in file')
      close (In)

   end subroutine ReadInput
end module Source_IO 
