module IOGroup 
   use Environment
   implicit none

   integer, parameter ::  GROUP_COUNT = 7, SURNAME_LEN = 15, INITIALS_LEN = 5, DATE_LEN = 4

   type student
      character(SURNAME_LEN, kind=CH_)  :: sur(GROUP_COUNT)
      character(INITIALS_LEN, kind=CH_) :: init(GROUP_COUNT)
      integer                           :: date(GROUP_COUNT)
   end type student
contains
   subroutine CreateDataFile(input_file, data_file)
      character(*), intent(in) :: input_file, data_file

      type(student) :: stud 
      integer       :: In, Out, IO, i
      character(:), allocatable :: format

      open (file=input_file, encoding=E_, newunit=In)
      open (file=data_file, form='unformatted', newunit=Out, access='stream')
         format = '(:a, 1x, a, 1x, i'//DATE_LEN//')'
         do i = 1, GROUP_COUNT
            ! Чтение входного файла
            read (In, format, iostat=IO) stud%sur(i), stud%init(i), stud%date(i)
            call Handle_IO_status(IO, "Ошибка чтения из входного файла-"//i)
         end do
         ! Запись в бинарный файл
         write (Out, iostat=IO) stud%sur, stud%init, stud%date
         call Handle_IO_status(IO, "Ошибка записи в бинарный файл!")
      close (Out)
      close (In)
   end subroutine CreateDataFile

   function ReadGroup(data_file) result(group)
      character(*),intent(in) :: data_file
      type(student)           :: Group
      
      integer :: In, IO
      
      open (file=data_file, form='unformatted', newunit=In, access='stream')
      read (In, iostat=IO) Group%sur, Group%init, Group%date
      call Handle_IO_status(IO, "Ошибка чтения данных")
      close (In)
   end function ReadGroup 
   
   subroutine WriteGroup(output_file, Group, writeFilePostion, writeLetter)
      character(*), intent(in)  :: output_file, writeFilePostion, writeLetter
      type(student), intent(in) :: Group

      integer :: Out, IO, i
      character(:), allocatable :: format

      open (file=output_file, encoding=E_,position=writeFilePostion, newunit=Out)
         format = '(a, 1x, a, 1x, i'//DATE_LEN//')'
         write(Out, '(a)') writeLetter
         write(Out, format, iostat=IO) (Group%sur(i), Group%init(i), Group%date(i), i = 1, GROUP_COUNT)
         call Handle_IO_status(IO, "Ошибка вывода данных")
      close (Out)
   end subroutine WriteGroup

   subroutine WriteElement(output_file, sur, init, date, writeFilePostion, writeLetter)
      character(*), intent(in)          :: output_file, writeFilePostion, writeLetter
      character(SURNAME_LEN, kind=CH_)  :: sur
      character(INITIALS_LEN, kind=CH_) :: init
      integer                           :: date
      
      integer :: IO, Out

      open (file=output_file, encoding=E_, position=writeFilePostion, newunit=Out)
         write(Out, '(a)') writeLetter
         write(Out, '(a, 1x, a, 1x, i'//DATE_LEN//')', iostat=IO) sur, init, date 
      close (Out)

   end subroutine WriteElement

end module IOGroup
