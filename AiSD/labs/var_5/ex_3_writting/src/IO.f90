module IO
   use Environment
   implicit none

   type node 
       character(:, kind=CH_), allocatable :: string
       type(node), pointer                 :: next => Null()
       type(node), pointer                 :: next_len => Null(), prev_len => Null()
   end type node 
   
 contains
   ! Чтение списка
   subroutine ReadList(input_file, List, SortedList) 
      character(*), intent(in) :: input_file 
      type(node), pointer, intent(inout)  :: List, SortedList 
      
      integer :: In
      
      open (file=Input_File, encoding=E_, newunit=In)
         call ReadValue(In, List, SortedList)
      close (In)
   end subroutine ReadList

   ! Чтение строки исходного кода.
   recursive subroutine ReadValue(in, elem, SortedList)
      type(node), pointer, intent(inout) :: elem, sortedList
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
         !call Put(SortedList, elem)
         call ReadValue(In, elem%Next, SortedList)
      end if
   end subroutine ReadValue

   recursive subroutine Put(Sorted_current, current)
      type(node), pointer  :: Sorted_current
      type(node), target   :: current

      if (.not. Associated(Sorted_current)) then
         ! Либо голова пока никуда не ссылается,
         ! либо дошли до поледнего элемента.
         Sorted_current => current
         !Sorted_current%sorted_next => Null() 
      else if (LEN(current%string) < LEN(Sorted_current%string)) then
         call Put(Sorted_current%next_len, current)
      else
         !tmp => Sorted_current
         !Sorted_current => current
         !Sorted_current%sorted_next => tmp

         current%next_len => Sorted_current
         Sorted_current => current
      end if
   end subroutine Put
 
 
   ! Вывод списка
   subroutine WriteList(output_file, List, writeFilePostion, writeLetter)
      character(*), intent(in)        :: output_file, writeFilePostion, writeLetter
      type(node), pointer, intent(in) :: List  

      integer :: Out

      open (file=output_file, encoding=E_, position=writeFilePostion, newunit=Out)
         write (out, '(/a)') writeLetter 
         call WriteValue(Out, List)
      close (Out)
   end subroutine WriteList
   ! Вывод списка
   subroutine WriteSortedList(output_file, List, writeFilePostion, writeLetter)
      character(*), intent(in)        :: output_file, writeFilePostion, writeLetter
      type(node), pointer, intent(in) :: List  

      integer :: Out

      open (file=output_file, encoding=E_, position=writeFilePostion, newunit=Out)
         write (out, '(/a)') writeLetter 
         call WriteValue_Sort(Out, List)
      close (Out)
   end subroutine WriteSortedList

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
