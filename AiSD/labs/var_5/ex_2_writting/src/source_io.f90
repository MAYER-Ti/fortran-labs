module Source_IO
   use Environment

   implicit none
   
   ! Структура данных для хранения строки исходного текста.
   type SourceLine
      character(:, CH_), allocatable :: String
      type(SourceLine), allocatable  :: Next
   end type SourceLine

contains
   ! Чтение исходного кода. 
   function ReadSourceCode(InputFile) result (Code)
      character(*), intent(in) :: InputFile

      integer                       :: In
      type(SourceLine), allocatable :: Code
      
      open (file=InputFile, encoding=E_, newunit=In)
         call ReadSourceLine(in, Code)
      close (In)
   end function ReadSourceCode

   ! Чтение строки исходного кода.
   recursive subroutine ReadSourceLine(in, line)
      type(SourceLine), allocatable, intent(inout) :: line
      integer, intent(in)                      :: In

      integer, parameter                       :: max_len = 1024
      character(max_len, CH_)                  :: string
      integer                                  :: IO

      ! Чтение строки во временную строку бОльшей длины.
      read (In, "(a)", iostat=IO) string
      call Handle_IO_Status(IO, "reading line from source code")
      if (IO == 0) then
         allocate (line)
         ! Хранение в размещаемом поле символов без завершающих пробелов.
         line%String = Trim(string)
         call ReadSourceLine(In, line%Next)
      end if
   end subroutine ReadSourceLine
 
   ! Вывод исходного кода.
   subroutine WriteCode(outputFile, Code, writePosition, writeLetter)
      character(*), intent(in)                  :: outputFile, writePosition, writeLetter 
      type(SourceLine), allocatable, intent(in) :: Code 

      integer  :: Out
      
      open (file=outputFile, encoding=E_, position=writePosition, newunit=Out)
         write (Out, '(/,a)') writeLetter 
         call WriteLine(Out, Code)
      close (Out)

   end subroutine WriteCode

   ! Вывод строки исходного кода.
   recursive subroutine WriteLine(Out, line)
      integer, intent(in)                       :: Out
      type(SourceLine), allocatable, intent(in) :: line

      integer  :: IO

      write (Out, "(a)", iostat=IO) line%String
      call Handle_IO_Status(IO, "writing line to file")
      if (Allocated(line%next)) &
         call WriteLine(Out, line%next)

   end subroutine WriteLine

   subroutine ReadInput(input_file, indexFirst, indexLast, indexPaste)
      character(*), intent(in) :: input_file
      integer, intent(out)     :: indexFirst, indexLast, indexPaste

      integer :: In, IO 

      open (file=input_file, encoding=E_, newunit=In)
         read (In, '(i2, 1x, i2, 1x, i2)', iostat=IO) indexFirst, indexLast, indexPaste
         call Handle_IO_Status(IO, 'reading indexes in file')
      close (In)

   end subroutine ReadInput
end module Source_IO 
