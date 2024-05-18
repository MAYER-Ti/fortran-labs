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
! Использовать массив символов 
! Первым индексом хранить номер студента 

program ex_1_2a
   use Environment
   use SearchesGroup 
   use IOGroup

   implicit none
   !integer, parameter       :: BLOCK_LEN = 15, EMPLOYEE_COUNT  = 5
   character(*), parameter  :: input_file = "../../ex_1_1/data/class.txt" , output_file = "output.txt"
   ! Массивы фамилий, инициалов и года рождения 
   character(kind=CH_), allocatable :: Surnames(:,:)
   character(kind=CH_), allocatable :: Initials(:,:)
   integer, allocatable             :: Dates(:)
   integer                          :: indexYoungest = 0, indexFirstForAlph = 0
   real(8)                          :: start_time = 0, end_time = 0

   ! Ввод данных
   call ReadGroup(input_file, Surnames, Initials, Dates) 
   ! Вывод исходных данных
   !call WriteGroup(output_file, Surnames, Initials, Dates, 'rewind', 'Входные данные')
   ! Обработка данных
   call cpu_time(start_time)
   ! Найти первого работника по алфавиту
   indexFirstForAlph = SearchFirstForAlph(Surnames, Ubound(Surnames, 1)) 
   ! Найти самого молодого
   indexYoungest = SearchYoungest(Dates)

   call cpu_time(end_time)
   print *, 'Время выполнения', (end_time-start_time) * 1000, 'миллисекунд'
   ! Вывод обработанных данных.
   call WriteElement(output_file, Surnames(indexFirstForAlph,:), Initials(indexFirstForAlph,:), Dates(indexFirstForAlph),&
       'rewind', 'Первый по алфавиту:')
   call WriteElement(output_file, Surnames(indexYoungest,:), Initials(indexYoungest,:), Dates(indexYoungest),&
       'append', 'Самый молодой:')

end program ex_1_2a
