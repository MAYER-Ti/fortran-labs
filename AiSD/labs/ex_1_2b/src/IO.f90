module IOEmployee 
   use Environment
   use globalVars
   implicit none
contains
   subroutine ReadEmployee(input_file, surnames, positions) 
      character(*),intent(in)         :: input_file
      character(kind=CH_), intent(out) :: surnames(BLOCK_LEN, EMPLOYEE_COUNT), positions(BLOCK_LEN, EMPLOYEE_COUNT)
      
      integer :: i, In, IO 
      character(:), allocatable :: format

      open (file=input_file, encoding=E_, newunit=In)
         format = '('//BLOCK_LEN//'a1, 1x, '//BLOCK_LEN//'a1)'
         read (In, format, iostat=IO) (surnames(:,i), positions(:,i), i = 1, EMPLOYEE_COUNT)
         call Handle_IO_status(IO, "reading employee list")
      close (In)

   end subroutine ReadEmployee 
   
   subroutine WriteEmployee(output_file, surnames, positions, writeFilePostion, writeLetter)
      character(*), intent(in)        :: output_file, writeFilePostion, writeLetter
      character(kind=CH_), intent(in) :: surnames(BLOCK_LEN, EMPLOYEE_COUNT), positions(BLOCK_LEN, EMPLOYEE_COUNT)

      integer :: i = 0, Out = 0, IO = 0
      character(:), allocatable :: format

      open (file=output_file, encoding=E_,position=writeFilePostion, newunit=Out)
         format = '('//BLOCK_LEN//'a1, 1x, '//BLOCK_LEN//'a1)'
         write(Out, '(a)') writeLetter
         write(Out, format, iostat=IO) (surnames(:,i), positions(:,i), i = 1, EMPLOYEE_COUNT)
         call Handle_IO_status(IO, "writing employee list")
      close (Out)
    end subroutine WriteEmployee

   subroutine WriteCountPositions(output_file, positions, posAndCount, countPositions, writeFilePostion, writeLetter)
      character(*), intent(in)        :: output_file, writeFilePostion, writeLetter
      character(kind=CH_), intent(in) :: positions(BLOCK_LEN, EMPLOYEE_COUNT)      
      integer, intent(in)             :: posAndCount(2, EMPLOYEE_COUNT), countPositions

      integer :: i = 0, Out = 0, IO = 0

      open (file=output_file, encoding=E_, position=writeFilePostion, newunit=Out)
            write (Out, '(a)') writeLetter
            write (Out, '('//countPositions//'('//BLOCK_LEN//'a1, 1x, i3,/))', iostat=IO) &
                (positions(:, posAndCount(1,i)), posAndCount(2,i), i = 1, countPositions) 
            call Handle_IO_status(IO, "write employee positions")
      close (Out)     

   end subroutine WriteCountPositions

end module IOEmployee 
