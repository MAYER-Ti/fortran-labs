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
! Использовать массив строк

program ex_1_1
   use Environment
   use SearchesGroup 
   use IOGroup

   implicit none
   !integer, parameter       :: BLOCK_LEN = 15, EMPLOYEE_COUNT  = 5
   character(*), parameter  :: input_file = "../data/class.txt" , output_file = "output.txt"
   ! Массивы фамилий, инициалов и года рождения 
   character(SURNAME_LEN, kind=CH_)  :: Surnames(GROUP_COUNT) = ""
   character(INITIALS_LEN, kind=CH_) :: Initials(GROUP_COUNT) = ""
   integer                           :: Dates(GROUP_COUNT) = 0
   ! Массивы где хранится  должности и количество сотрудников этой должности
   integer                           :: indexFirstForAlph = 0, indexYoungest = 0
   ! Ввод данных
   call ReadGroup(input_file, Surnames, Initials, Dates) 
   ! Вывод исходных данных
   call WriteGroup(output_file, Surnames, Initials, Dates, 'rewind', 'Входные данные')
   ! Обработка данных
   ! Найти первого работника по алфавиту
   indexFirstForAlph = SearchFirstForAlph(Surnames) 
   ! Найти самого молодого
   indexYoungest = SearchYoungest(Dates)
   ! Вывод обработанных данных.
   call WriteElement(output_file, Surnames(indexFirstForAlph), Initials(indexFirstForAlph), Dates(indexFirstForAlph),&
       'append', 'Первый по алфавиту:')
   call WriteElement(output_file, Surnames(indexYoungest), Initials(indexYoungest), Dates(indexYoungest),&
       'append', 'Самый молодой:')

end program ex_1_1