module Integral_IO
   use Environment

   implicit none
contains
   ! Чтение параметра p.
   subroutine ReadMatrix(input_file, Matrix, N, M)
      character(*), intent(in) :: input_file
      real(R_), intent(out)    :: Matrix 
      integer, intent(out)     :: N,M

      integer :: In = 0, i = 0
   
      open (file=input_file, newunit=In)
         read (In, *) N, M
         allocate (Matrix(N,M))
         read (In, *) (Matrix(:,i), i = 1, M)
      close (In)
   end subroutine ReadP
 
   subroutine WriteMatrix(output_file, Matrix, N, M)
      character(*), intent(in) :: output_file
      real(R_), intent(in)     :: Matrix
      integer, intent(in)      :: N, M

      open (file=output_file, newunit=Out, position='append')
         write(Out, '('//M//'f6.2)') (Matrix(:,i), i = 1, M) 
      close (Out)
   end subroutine WriteMatrix
end module Integral_IO
