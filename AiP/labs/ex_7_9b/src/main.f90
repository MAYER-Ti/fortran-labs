! Задание Вычислить нормы квадратной матрицы A, содержащей 100 элементов (N = 10):
! б) ||A|| = max(j) sum(i = 1,N) |a(i,j)|
! Указания
! MaxVal, Sum с параметром dim, Abs

! Найти сумму каждой строки матрицы по модулю
! Найти максимальное из этих сумм 

program exercise_7_9b
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: Z(:, :)
   real(R_)                :: maxZ = 0

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N, N))
      read (In, *) (Z(:, i), i = 1, N)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') (Z(:, i), i = 1, N)
   close (Out)

   maxZ = Norma(Z, N)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, T5, "= ", f9.2)') "Sum", maxZ
   close (Out)

contains
   Real(R_) pure function Norma(Z, N) result(maxZ)
      real(R_), intent(in) :: Z(:, :)
      integer, intent(in)  :: N 

      Real(R_) Sums(N)
      integer  i
      
      do concurrent (i = 1:N)
        Sums(i) = Sum(Abs(Z(:,i))) 
      end do

      maxZ = MaxVal(Sums, 1)

   end function Norma

end program exercise_7_9b
