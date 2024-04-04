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
   integer                 :: N = 0, M, i = 0
   real(R_), allocatable   :: A(:,:), S(:,:)

   ! Ввод данных.
   call ReadMatrix(input_file, A, N, M) 

   ! Вывод данных.
   call WriteMatrix(output_file, A, 'rewind', 'Входная матрица') 
   ! Размещение данных
   allocate(S(N-1,N-1))
   ! Сумма столбца со следующем
   A = A(:,:) + A(::2,:)  
  ! do concurrent(i = 1:N-1) 
  !    A(i,:) = A(i,:) + A(i+1,:)
  ! end do
   !S = A(1:N-1,:) + A(1:N-1,::2)
  ! do concurrent(i = 1:N-1)
  !    S(:,i) = A(1:N-1,i) + A(1:N-1,i+1)
  ! end do
   !Обработка данных 
    
  call WriteMatrix(output_file, A, 'append', 'Матрица сумм')

end program exercise_7_23
