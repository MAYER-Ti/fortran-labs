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
! Использовать рекурсивно размещаемые однонаправвленные списки
! Использовать хвостовую рекурсию

program ex_1_6b
   use Environment
   use SearchesGroup 
   use IOGroup

   implicit none
   character(*), parameter  :: input_file  = "../data/class.txt", &
                               output_file = "output.txt", &
                               data_file   = "group.dat" 
   ! Массивы фамилий, инициалов и года рождения 
   type(student), allocatable, target :: Group
   type(student), pointer          :: studFirstAlph
   type(student), pointer          :: studYougest
   ! Ввод данных
   Group = ReadGroup(input_file) 
   ! Вывод исходных данных
   call WriteGroup(output_file, Group, 'rewind', 'Входные данные')
   ! Обработка данных
   ! Найти первого работника по алфавиту
   studFirstAlph => Group
   call FirstForAlph(Group, studFirstAlph) 
   ! Найти самого молодого
   studYougest => Group
   call Youngest(Group, studYougest)
   ! Вывод обработанных данных.
   call WriteElement(output_file, studFirstAlph, 'append', 'Первый по алфавиту:')
   call WriteElement(output_file, studYougest, 'append', 'Самый молодой:')

end program ex_1_6b
