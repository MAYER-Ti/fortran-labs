module IO
   use Environment
   implicit none

   type node 
       character(:, kind=CH_), allocatable :: string
       type(node), pointer                 :: next => Null()
       type(node), pointer                 :: next_len => Null()
   end type node 
   
 contains
   ! Чтение списка
   function ReadList(input_file) result(List) 
      character(*), intent(in) :: input_file 
      type(node), pointer  :: List 
      
      integer :: In
      
      open (file=Input_File, encoding=E_, newunit=In)
         call ReadValue(In, List)
      close (In)
   end function ReadList

   ! Чтение строки исходного кода.
   recursive subroutine ReadValue(in, elem)
      type(node), pointer, intent(inout) :: elem
      integer, intent(in)                      :: In

      integer, parameter                       :: max_len = 1024
      character(max_len, CH_)                  :: string
      integer                                  :: IO

      ! Чтение строки во временную строку бОльшей длины.
      read (In, "(a)", iostat=IO) string
      call Handle_IO_Status(IO, "reading line from source code")
      if (IO == 0) then
         allocate (elem)
         ! Хранение в размещаемом поле символов без завершающих пробелов.
         elem%string = Trim(string)
         call ReadValue(In, elem%Next)
      end if
   end subroutine ReadValue
 
   ! Вывод списка
   subroutine WriteList(output_file, List, isSort, writeFilePostion, writeLetter)
      character(*), intent(in)        :: output_file, writeFilePostion, writeLetter
      type(node), pointer, intent(in) :: List  
      logical, intent(in)             :: isSort

      integer :: Out

      open (file=output_file, encoding=E_, position=writeFilePostion, newunit=Out)
         write (out, '(/a)') writeLetter 
         if(isSort) then
            call WriteValue_Sort(Out, List)
        else
            call WriteValue(Out, List)
        end if
      close (Out)
   end subroutine WriteList
   ! вывод следующего значения
   recursive subroutine WriteValue(Out, Elem)
      integer, intent(in)                 :: Out
      type(node), pointer, intent(in) :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(a)', iostat=IO) Elem%string
         call Handle_IO_status(IO, "Некорректный вывод списка")
         call WriteValue(Out, Elem%next)
      end if
   end subroutine WriteValue 

   ! вывод следующего сортированного значения
   recursive subroutine WriteValue_Sort(Out, Elem)
      integer, intent(in)                 :: Out
      type(node), pointer, intent(in) :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
         write (Out, '(a)', iostat=IO) Elem%string
         call Handle_IO_status(IO, "Некорректный вывод списка")
         call WriteValue(Out, Elem%next_len)
      end if
   end subroutine WriteValue_Sort 

end module IO 
