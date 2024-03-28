! Задание
! В матрице A(50,50) найти значения индексов i и j такие, что сумма элементов aij, ai(j+1), a(i+1)j, a(i+1)(j+1) максимальна из всех
! аналогичных сумм. Если таких сумм несколько, то указать все пары индексов, удовлетворяющих заданному условию.
! Указания
! Сначала вычислить массив всех сумм S(N-1,N-1) по заданному условию.
! Для эффективно вектроризируемых вычислений массива S можно вначале к каждому столбцу прибавить следующий
! Для завершения вычисления массива S теперь достаточно к каждой строке прибавить следующую.

program exercise_7_23
   use Environment
   use Matrix_IO
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: Out = 0, N = 0, M, i = 0, j = 0
   real(R_), allocatable   :: A(:,:), S(:,:), SummStolbs(:,:)

   ! Ввод данных.
   call ReadMatrix(input_file, A, N, M) 

   ! Вывод данных.
   call WriteMatrix(output_file, A) 
   
   !Размещение данных
   allocate(S(N-1,N-1)) 
   allocate(SummStolbs(N-1,N))
   !Обработка данных 
   !Достижение оптимизации засчет сплошных данных и распараллеливания
   do concurrent(i = 1:N)
      do concurrent(j = 1:N-1)
         SummStolbs = A(j,i) + A(j+1,i)
      end do
   end do
  ! do concurrent(i=1:N-1)
  !    do concurrent (j=1:N-1)  
  !       S(i,j) = A(i,j) + A(i,j+1) + A(i+1,j) + A(i+1,j+1) 
  !    end do
  ! end do 
   ! Вывод данных.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
       write (Out, *) "Матрица сумм:"
     !  write (Out, *) SummStolbs
     !  write (Out, '(A, 2i2)') "Индекс максимального значения - ", MaxLoc(S)
     !  write (Out, '(A, f6.2)') "Максимальное значение - ", MaxVal(S)
   close (Out)

  call WriteMatrix(output_file, SummStolbs)

end program exercise_7_23
