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
! первым индексом использовать позицию сотрудника 
program ex_1_2b
   use Environment
   use IOEmployee
   use calcPositions
   use globalVars

   implicit none
   character(*), parameter  :: input_file = "../data/class.txt" , output_file = "output.txt"
   ! Массивы фамилий и должностей 
   character(kind=CH_)      :: surnames(EMPLOYEE_COUNT, BLOCK_LEN) = "", &
                               positions(EMPLOYEE_COUNT, BLOCK_LEN) = ""
   ! Матрица длинной количества сотрудников на 2, где хранится индекс должности и количество сотрудников этой должности
   integer                  :: outPosAndCount(2, EMPLOYEE_COUNT) 
   ! Количество должностей
   integer :: countPositions = 0

   ! Ввод данных
   call ReadEmployee(input_file, surnames, positions) 
   ! Вывод исходных данных
   call WriteEmployee(output_file, surnames, positions, 'rewind', 'Входные данные')
   ! Обработка данных
   call  CalcPos(positions, outPosAndCount, countPositions) 
!   ! Вывод обработанных данных.
   call WriteCountPositions(output_file, positions, outPosAndCount, countPositions, 'append', 'Кол-во должностей')

end program ex_1_2b
