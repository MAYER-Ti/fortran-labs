! Задание
! 5. Дан список сотрудников научно-исследовательской лаборатории в виде:
! ФАМИЛИЯ   ДОЛЖНОСТЬ
! 15 симв.  15 симв.
! Пример входного файла:
! Иванов       техник
! Определить число одинаковых должностей. пример выходного файла:
! ведущий инженер - 2
! старший инженер - 3
! инженер         - 8
! техник          - 2
! Указание
! Завести логический массив длинной N. При обработке должности
! очередного сотрудника сразу найти все такие должности и поместить в логическом массиве, где они встречаются
! Должность следующего сотрудника обрабатывать, только если она прежде не встречалась (смотреть логический массив).
! Использовать Count с маской, Any. См. упражнение 5.16, 7.25

program ex_1_1
   use Environment

   implicit none
   integer, parameter       :: BLOCK_LEN = 15, EMPLOYEE_COUNT  = 5
   character(*), parameter  :: input_file = "../data/class.txt" , output_file = "output.txt"
   character(:), allocatable   :: format
   
   ! Массивы фамилий и должностей 
   character(BLOCK_LEN, kind=CH_) :: surnames(EMPLOYEE_COUNT) = "", &
                                     positions(EMPLOYEE_COUNT) = ""
   logical                        :: locPosition(EMPLOYEE_COUNT)

   integer :: IO, i, In, Out
   integer, allocatable :: ind(:)
   ! Ввод данных
   open (file=input_file, encoding=E_, newunit=In)
      format = '(a, 1x, a)'
      read (In, format, iostat=IO) (surnames(i), positions(i), i = 1, EMPLOYEE_COUNT)
      call Handle_IO_status(IO, "reading employee list")
   close (In)
   ! Вывод исходных данных
   open (file=output_file, encoding=E_, newunit=Out)
      format = '(a, 1x, a)'
      write(Out, format, iostat=IO) (surnames(i), positions(i), i = 1, EMPLOYEE_COUNT)
      call Handle_IO_status(IO, "writing employee list")
   close (Out)
   Ind  = [(i, i = 1, EMPLOYEE_COUNT)]

   do i = 1,EMPLOYEE_COUNT
            


   end do




! Составление логической маски, соответствующей юношам.
!   Is_A_Boy       = Gender == MALE ! Gender == CH__"М" в некоторых компиляторах может пока не поддерживаться.
!   Boys_Amount    = Count(Is_A_Boy)
!   
!   ! Получение массивов, связынных с юношами.
!   ! 1-ый способ. Использование массива номеров юношей в списке.
!   Boys_Pos   = Pack(INDEXES, Is_A_Boy) ! == [1, 2, 3]
!   allocate (Boys_Surnames(Boys_Amount), Boys_Initials(Boys_Amount), &
!      Boys_Marks(Boys_Amount, MARKS_AMOUNT))
!   do concurrent (i = 1:Boys_Amount)
!      ! Получение списков юношей.
!      Boys_Surnames(i)  = Surnames(Boys_Pos(i))
!      Boys_Initials(i)  = Initials(Boys_Pos(i))
!      Boys_Marks(i, :)  = Marks(Boys_Pos(i), :)
!   end do
!   
!   ! 2-ой способ. Использование двумерной маски, накладываемой на массив оценок.
!   ! ! Получение списков юношей.
!   ! Boys_Surnames  = Pack(Surnames, Is_A_Boy)
!   ! Boys_Initials  = Pack(Initials, Is_A_Boy)
!   ! ! Получение двумерного списка оценок юношей:
!   ! ! 1. Для получение двумерного списка оценок необходима двумерная маска, захватывающая все оценки юношей:
!   ! ! Spread(Is_A_Boy, 2, MARKS_AMOUNT)
!   ! ! 2. По такой маске Pack вернёт одномерный массив со всеми оценками юношей, который
!   ! ! необходимо будет переформировать в двумерный массив размером: [Boys_amount, MARKS_AMOUNT].
!   ! Boys_Marks     = Reshape( Pack(Marks, Spread(Is_A_Boy, 2, MARKS_AMOUNT)), [Boys_amount, MARKS_AMOUNT])
!   
!   ! Вычисление средней оценки для юношей. Вне цикла для векторизации.
!   Boys_Aver_Marks   = Sum(Boys_Marks, dim=2) / Real(MARKS_AMOUNT, R_)
!
!   Is_A_Girl      = .not. Is_A_Boy
!   Girls_Amount   = STUD_AMOUNT - Boys_Amount
!   
!   ! Получение массивов, связынных с девушками.
!   Girls_Pos   = Pack(INDEXES, Is_A_Girl) ! == [4, 5]
!   allocate (Girls_Surnames(Girls_Amount), Girls_Initials(Girls_Amount), &
!      Girls_Marks(Girls_Amount, MARKS_AMOUNT))
!   do concurrent (i = 1:Girls_Amount)
!      ! Получение списков девушек.
!      Girls_Surnames(i)  = Surnames(Girls_Pos(i))
!      Girls_Initials(i)  = Initials(Girls_Pos(i))
!      Girls_Marks(i, :)  = Marks(Girls_Pos(i), :)
!   end do
!      
!   ! Вычисление средней оценки для девушек. Вне цикла для векторизации.
!   Girls_Aver_Marks = Sum(Girls_Marks, dim=2) / Real(MARKS_AMOUNT, R_)
!
!   ! Сортировка списка юношей по среднему баллу методом пузырька.
!   do i = Boys_amount, 2, -1
!      ! Просматриваем список с начала, ставя в конец менее успешного.
!      do j = 1, i-1
!         Swap = .false.
!         ! Проверка на то, стоит ли менять учащихся местами.
!         if (Boys_Aver_Marks(j) < Boys_Aver_Marks(j+1)) then
!            Swap = .true.
!         else if (Boys_Aver_Marks(j) == Boys_Aver_Marks(j+1)) then
!            if (Boys_Surnames(j) > Boys_Surnames(j+1)) then
!               Swap = .true.
!            else if (Boys_Surnames(j)==Boys_Surnames(j+1) .and. Boys_Initials(j)>Boys_Initials(j+1)) then
!               Swap = .true.
!            end if
!         end if
!         
!         if (Swap) then
!            tmpSurname           = Boys_Surnames(j+1)
!            Boys_Surnames(j+1)   = Boys_Surnames(j)
!            Boys_Surnames(j)     = tmpSurname
!			! Boys_Surnames(j+1:j:-1) = Boys_Surnames(j:j+1)
!			! Boys_Surnames([j, j+1]) = Boys_Surnames([j+1, j])
!
!            tmpInitials          = Boys_Initials(j+1)
!            Boys_Initials(j+1)   = Boys_Initials(j)
!            Boys_Initials(j)     = tmpInitials
!
!            tmpMarks             = Boys_Marks(j+1, :)
!            Boys_Marks(j+1, :)   = Boys_Marks(j, :)
!            Boys_Marks(j, :)     = tmpMarks
!
!            tmpAverMark          = Boys_Aver_Marks(j+1)
!            Boys_Aver_Marks(j+1) = Boys_Aver_Marks(j)
!            Boys_Aver_Marks(j)   = tmpAverMark
!         end if
!      end do
!   end do
!
!   ! Сортировка списка девушек по среднему баллу методом пузырька.
!   do i = Girls_Amount, 2, -1
!      ! Просматриваем список с начала, ставя в конец менее успешного.
!      do j = 1, i-1
!         Swap = .false.
!         ! Проверка на то, стоит ли менять учащихся местами.
!         if (Girls_Aver_Marks(j) < Girls_Aver_Marks(j+1)) then
!            Swap = .true.
!         else if (Girls_Aver_Marks(j) == Girls_Aver_Marks(j+1)) then
!            if (Girls_Surnames(j) > Girls_Surnames(j+1)) then
!               Swap = .true.
!            else if (Girls_Surnames(j)==Girls_Surnames(j+1) .and. Girls_Initials(j)>Girls_Initials(j+1)) then
!               Swap = .true.
!            end if
!         end if
!
!         if (Swap) then
!            tmpSurname           = Girls_Surnames(j+1)
!            Girls_Surnames(j+1)   = Girls_Surnames(j)
!            Girls_Surnames(j)     = tmpSurname
!
!            tmpInitials          = Girls_Initials(j+1)
!            Girls_Initials(j+1)   = Girls_Initials(j)
!            Girls_Initials(j)     = tmpInitials
!
!            tmpMarks             = Girls_Marks(j+1, :)
!            Girls_Marks(j+1, :)   = Girls_Marks(j, :)
!            Girls_Marks(j, :)     = tmpMarks
!
!            tmpAverMark          = Girls_Aver_Marks(j+1)
!            Girls_Aver_Marks(j+1) = Girls_Aver_Marks(j)
!            Girls_Aver_Marks(j)   = tmpAverMark
!         end if
!      end do
!   end do
!
!   ! Вывод отсортированного списка юношей со средним баллом.
!   open (file=output_file, encoding=E_, position='append', newunit=Out)
!      write (out, '(/a)') "Успеваемость юношей:"
!      write (Out, format, iostat=IO) &
!         (Boys_Surnames(i), Boys_Initials(i), "М", Boys_Marks(i, :), Boys_Aver_Marks(i), i = 1, Boys_Amount)
!   close (Out)
!   ! Обработка статуса записи.
!   Out = OUTPUT_UNIT
!   open (Out, encoding=E_)
!   select case(io)
!      case(0)
!      case(IOSTAT_END)
!         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
!      case(1:)
!         write (Out, '(a)') "Error while writing sorted boys list: ", io
!      case default
!         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
!   end select
!   
!      ! Вывод отсортированного списка девушек со средним баллом.
!   open (file=output_file, encoding=E_, position='append', newunit=Out)
!      write (out, '(/a)') "Успеваемость девушек:"
!      write (Out, format, iostat=IO) (Girls_Surnames(i), Girls_Initials(i), "Ж", Girls_Marks(i, :), &
!         Girls_Aver_Marks(i), i = 1, Girls_Amount)
!   close (Out)
!   ! Обработка статуса записи.
!   Out = OUTPUT_UNIT
!   open (Out, encoding=E_)
!   select case(io)
!      case(0)
!      case(IOSTAT_END)
!         write (Out, '(a)') "End of file has been reached while writing sorted girls list."
!      case(1:)
!         write (Out, '(a)') "Error while writing sorted girls list: ", io
!      case default
!         write (Out, '(a)') "Undetermined error has been reached while writing sorted girls list: ", io
!   end select
!
contains
   ! Чтение списка сотрудников: фамилия сотрудника, должность
   subroutine Read_employee_list(input_file, BLOCK_LEN, EMPLOYEE_COUNT, &
         surnamesEmployee, positionsEmployee)
      character(*), intent(in)         :: input_file
      integer, intent(in)              :: BLOCK_LEN, EMPLOYEE_COUNT
      character(kind=CH_), intent(out) :: surnamesEmployee(:), positionsEmployee(:)

      integer                   :: In, IO, i
      character(:), allocatable :: format

      open (file=input_file, newunit=In)
         format = '('// BLOCK_LEN // 'a1, 1x, ' // BLOCK_LEN // 'a1)'
         read (In, format, iostat=IO) (surnamesEmployee(i), positionsEmployee(i), &
            i = 1, EMPLOYEE_COUNT)
         call Handle_IO_status(IO, "reading employee list")
      close (In)
   end subroutine Read_employee_list

!   !Вывод списка сотрудников
!   subroutine Write_employee_list(output_file, BLOCK_LEN, EMPLOYEE_COUNT, &
!         surnamesEmployee, positionsEmployee, listName, positionToWrite)
!      character(*), intent(in)         :: output_file, listName, positionToWrite
!      integer, intent(in)              :: BLOCK_LEN, EMPLOYEE_COUNT
!      character(BLOCK_LEN, kind=CH_), intent(in) :: surnamesEmployee(:,:), positionsEmployee(:,:)
!
!      integer                   :: Out, IO, i
!      character(:), allocatable :: format
!
!   end subroutine Write_employee_list













end program ex_1_1
