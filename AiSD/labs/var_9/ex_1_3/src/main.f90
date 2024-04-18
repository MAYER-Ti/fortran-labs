! Задание
! 9. Дан список сотрудников группы в виде:
! ФАМИЛИЯ   И. О.   Год рождения
! 15 симв.  5 симв. 4 симв. 
! Пример входного файла:
! Иванов И. И. 1994
! Петров Д. Т. 1992
! Выделить первого по алфавиту и самого малодого. пример выходного файла:
! Первый по алфавиту:
! Иванов И. И. 1994
! Самый молодой: 
! Петров Д. Т. 1992
! 
! Использовать массив структур 

program ex_1_3
   use Environment
   use SearchesGroup 
   use IOGroup

   implicit none
   !integer, parameter       :: BLOCK_LEN = 15, EMPLOYEE_COUNT  = 5
   character(*), parameter  :: input_file  = "../data/class.txt", &
                               output_file = "output.txt", &
                               data_file   = "group.dat" 

   ! Массивы фамилий, инициалов и года рождения 
   type(student) :: Group(GROUP_COUNT)
   integer       :: indexFirstForAlph = 0, indexYoungest = 0 
   ! Создание файла должностей
   call CreateDataFile(input_file, data_file)
   ! Ввод данных
   Group = ReadGroup(data_file) 
   ! Вывод исходных данных
   call WriteGroup(output_file, Group, 'rewind', 'Входные данные')
   ! Обработка данных
   ! Найти первого работника по алфавиту
   indexFirstForAlph = SearchFirstForAlph(Group) 
   ! Найти самого молодого
   indexYoungest = SearchYoungest(Group)
   ! Вывод обработанных данных.
   call WriteElement(output_file, Group(indexFirstForAlph), 'append', 'Первый по алфавиту:')
   call WriteElement(output_file, Group(indexYoungest), 'append', 'Самый молодой:')

end program ex_1_3
