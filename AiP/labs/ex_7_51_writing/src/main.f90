! Задание
! Матрицу С (30,30) преобразовать в одномерный массив, просматривая С по строкам и выбрасывая отрицательные элементы
! Указания
! Pack 

program exercise_7_28
   use Environment
   use Matrix_IO
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: C(:,:), C1(:)
   logical, allocatable    :: Mask(:,:)


   ! Ввод данных.
   call ReadMatrix(input_file, C, N, M) 

   ! Вывод данных.
   call WriteMatrix(output_file, C) 
   
   !Обработка данных 
   !Достижение оптимизации засчет распараллеливания
   allocate(C1(N*M)) 

   Mask = C >= 0 
   
   C1 = Reshape(Pack(C, Mask), [N*M])

   ! Вывод данных.
   open(file=output_file, newunit=Out, position='append')
      write(Out, *) "C1"
      write(Out, '('//N*M//'f6.2)') C1
   close(Out)


end program exercise_7_28
