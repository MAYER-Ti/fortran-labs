module IOEmployee 
   use Environment
   use globalVars
   implicit none
contains
   subroutine ReadEmployee(input_file, surnames, positions) 
      character(*),intent(in)         :: input_file
      character(kind=CH_), intent(out) :: surnames(EMPLOYEE_COUNT, BLOCK_LEN), positions(EMPLOYEE_COUNT, BLOCK_LEN)
      
      integer :: i, In, IO 
      character(:), allocatable :: format

      open (file=input_file, encoding=E_, newunit=In)
         format = '('//BLOCK_LEN//'a1, 1x, '//BLOCK_LEN//'a1)'
         read (In, format, iostat=IO) (surnames(i,:), positions(i,:), i = 1, EMPLOYEE_COUNT)
         call Handle_IO_status(IO, "reading employee list")
      close (In)

   end subroutine ReadEmployee 
   
   subroutine WriteEmployee(output_file, surnames, positions, writeFilePostion, writeLetter)
      character(*), intent(in)        :: output_file, writeFilePostion, writeLetter
      character(kind=CH_), intent(in) :: surnames(EMPLOYEE_COUNT, BLOCK_LEN), positions(EMPLOYEE_COUNT, BLOCK_LEN)

      integer :: i = 0, Out = 0, IO = 0
      character(:), allocatable :: format

      open (file=output_file, encoding=E_,position=writeFilePostion, newunit=Out)
         format = '('//BLOCK_LEN//'a1, 1x, '//BLOCK_LEN//'a1)'
         write(Out, '(a)') writeLetter
         write(Out, format, iostat=IO) (surnames(i,:), positions(i,:), i = 1, EMPLOYEE_COUNT)
         call Handle_IO_status(IO, "writing employee list")
      close (Out)
    end subroutine WriteEmployee

   subroutine WriteCountPositions(output_file, pos, counts, countPositions, writeFilePostion, writeLetter)
      character(*), intent(in)        :: output_file, writeFilePostion, writeLetter
      character(kind=CH_), allocatable, intent(in) :: pos(:, :)      
      integer, allocatable                         :: counts(:)
      integer                                      :: countPositions

      integer :: i = 0, Out = 0, IO = 0

      open (file=output_file, encoding=E_, position=writeFilePostion, newunit=Out)
            write (Out, '(a)') writeLetter
            write (Out, '('//countPositions//'('//BLOCK_LEN//'a1, 1x, i3,/))', iostat=IO) &
                (pos(i,:), counts(i), i = 1, countPositions) 
            call Handle_IO_status(IO, "write employee positions")
      close (Out)     

   end subroutine WriteCountPositions

end module IOEmployee 
