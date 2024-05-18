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
! Использовать массивы символов.
! первым индексом использовать позицию символа
program ex_1_2b
   use Environment
   use IOEmployee
   use calcPositions
   use globalVars
   implicit none

   integer :: cpu_time = 0, start_time = 0, end_time = 0
   real(R_) :: elapsed_time = 0.0
   ! "../data/class.txt"
   character(*), parameter  :: input_file = "../../ex_1_1/data/class.txt" , output_file = "output.txt"
   ! Массивы фамилий и должностей 
   character(kind=CH_), allocatable :: surnames(:,:) ,positions(:,:) 
   ! Массивы где хранится  должности и количество сотрудников этой должности
   character(kind=CH_), allocatable :: Poss(:,:)
   integer, allocatable             :: Counts(:)
   ! Ввод данных
   call ReadEmployee(input_file, surnames, positions) 
   print *, Ubound(positions, 2)
   ! Вывод исходных данных
   !call WriteEmployee(output_file, surnames, positions, 'rewind', 'Входные данные')

   call system_clock(count_rate=cpu_time)
   call system_clock(count=start_time)
   ! Обработка данных
   call  CalcPos(positions, Ubound(positions, 2), Poss, Counts) 

   call system_clock(count=end_time)
   elapsed_time = (real(end_time-start_time)/real(cpu_time)) * 1000
   print *, start_time, end_time
   print *, 'Время выполнения', elapsed_time, 'миллисекунд'
   ! Вывод обработанных данных.
   call WriteCountPositions(output_file, Poss, Counts, 'rewind', 'Кол-во должностей')


end program ex_1_2b
