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
! Использовать или массив структур или структуру массивов
! Использовать хвостовую рекурсию
program ex_1_5
   use Environment
   use IOEmployee
   use calcPositions

   implicit none
   character(*), parameter  :: input_file = "../data/class.txt", &
                               output_file = "output.txt", &
                               data_file   = "employee.dat"
   ! Массивы фамилий и должностей 
   type(employees) :: empls 
   ! Массивы где хранится  должности и количество сотрудников этой должности
   type(ResPosAndCount) :: Res
  ! character(BLOCK_LEN, kind=CH_), allocatable :: Poss(:)
  ! integer, allocatable                        :: Counts(:)
   ! Количество должностей
  ! integer :: sizePosCounts = 0
   ! Создание файла данных
   call CreateDataFile(input_file, data_file)
   ! Ввод данных
   empls = ReadEmployees(data_file) 
   ! Вывод исходных данных
   call WriteEmployee(output_file, empls, 'rewind', 'Входные данные')
   ! Обработка данных
   call SearchPositions(empls%pos, Res) 
   ! Вывод обработанных данных.
   call WriteCountPositions(output_file, Res, 'append', 'Кол-во должностей')

end program ex_1_5