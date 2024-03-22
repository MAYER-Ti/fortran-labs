! Задание
! Вычислить значение величины - квадратный корень из произведения сумм элементов , 
! Если матрица A=(aij) квадратная и содержит 900 элементов.
! Указания
! Вычисления провести, используя два подхода: 
! 1) Sum, Product, SqRt;
! 2) Norm2, Product
! Сравнить результат двух подходов.
! Использовать параметр dim в Sum и Norm2

! Norm2 - Евклидовая норма вектора
! Product - произведение элементов массива. Работает также как и Sum.
program exercise_7_19v
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0 
   real(R_), allocatable   :: A(:,:)
   real(R_)                :: S = 0
   ! i - строка, j - столбец
   ! Ввод данных.
   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (A(N, N))
      read (In, *) (A(:, i), i = 1, N)
   close (In)

   ! Вывод данных.
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(a,2i3)') "Размер матрицы:", N, N
      write (Out, '(a)') "Матрица:" 
      write (Out, '('//N//'f6.2)') (A(:, N), i = 1, N)
   close (Out)

   ! Размещение массивов в НАЧАЛЕ работы программы,
   ! а не внутри подпрограмм при КАЖДОМ их вызове.
   S = EvclidNormalOnMatrix(A, N)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '("S = ", f7.4)') S
   close (Out)

contains
   Real(R_) pure function EvclidNormalOnMatrix(A, N)
     real(R_), intent(in) :: A(:,:) 
     integer, intent(in)  :: N 

     integer :: i, j
     EvclidNormalOnMatrix = sqrt( product(A, 2) * sum(A, 2) * ((A(:,i), i = 1,N)**2) )

   end function EvclidNormalOnMatrix 
  end program exercise_7_19v
