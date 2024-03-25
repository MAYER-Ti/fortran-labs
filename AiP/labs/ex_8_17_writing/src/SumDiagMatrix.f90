!   Сделать отображение ранга ссылки, взглянув на двумерную матрицу как на одномерный массив.
!   Просумировать в одномерном массиве каждый N+1-ый элемент (UBound).
!   Матрицу принимает как target, contigious, inent(inout)
module SumDiagMatrix 
   use Environment

   implicit none

contains
   real(R_) pure function SumDiag(Matrix, N)
      real(R_), intent(in)    :: Matrix
      integer, intent(in)     :: N

      integer :: i = 0, j = 0
      
         

   end function SumDiag

end module SumDiagMatrix 
