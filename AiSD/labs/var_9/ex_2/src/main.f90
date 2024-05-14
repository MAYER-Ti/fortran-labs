! Задание 9
! Разработать чистую подпрограмму копирования группы
! строк данного текста от начальной строки First до конеч-
! ной Last и вставки их после M-ой строки. Правильному
! набору вводимых данных соответствует ситуация, когда
! 0≤First≤Last и M не входит в диапазон [First, Last].
! Указание. Элементом списка является строка.

program ex_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: source_file, input_file, output_file

   type(line), pointer :: List ! Первоначальный код.
   integer             :: F = 0, L = 0, M = 0

   source_file = "../data/source.f90"
   input_file  = "../data/input.txt"
   output_file = "Output.txt"
  
   ! Ввод данных  
   List => ReadList(source_file)
   call ReadInput(input_file, F, L, M)
   ! Вывод исходных данных
   call WriteList(output_file, List, "rewind", "Исходный файл ------------------------------------")
   ! Обработка данных
   call MovePartList(F, L, M, List)
   ! Вывод обработанных данных
   call WriteList(output_file, List, "append" , "Измененный файл ------------------------------------")

end program ex_2
