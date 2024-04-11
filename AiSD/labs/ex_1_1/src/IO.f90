module IOEmployee 
   use Environment
   use globalVars
   implicit none

contains
   subroutine ReadEmployee(input_file, surnames, positions) 
      character(*),intent(in)                     :: input_file
      character(BLOCK_LEN, kind=CH_), intent(out) :: surnames(EMPLOYEE_COUNT), positions(EMPLOYEE_COUNT)
      
      integer :: i = 0, In = 0, IO = 0 

      open (file=input_file, encoding=E_, newunit=In)
         read (In, '(a, 1x, a)', iostat=IO) (surnames(i), positions(i), i = 1, EMPLOYEE_COUNT)
         call Handle_IO_status(IO, "reading employee list")
      close (In)

   end subroutine ReadEmployee 
   
   subroutine WriteEmployee(output_file, surnames, positions, writeFilePostion, writeLetter)
      character(*), intent(in)                   :: output_file, writeFilePostion, writeLetter
      character(BLOCK_LEN, kind=CH_), intent(in) :: surnames(EMPLOYEE_COUNT), positions(EMPLOYEE_COUNT)

      integer :: i = 0, Out = 0, IO = 0

      open (file=output_file, encoding=E_,position=writeFilePostion, newunit=Out)
         write(Out, '(a)') writeLetter
         write(Out, '(a, 1x, a)', iostat=IO) (surnames(i), positions(i), i = 1, EMPLOYEE_COUNT)
         call Handle_IO_status(IO, "writing employee list")
      close (Out)
   end subroutine WriteEmployee

   subroutine WriteCountPositions(output_file, pos, counts, countPositions, writeFilePostion, writeLetter)
      character(*), intent(in)                   :: output_file, writeFilePostion, writeLetter
      character(BLOCK_LEN, kind=CH_), allocatable, intent(in) :: pos(:)
      integer, allocatable, intent(in)           :: counts(:)     
      integer, intent(in)                        :: countPositions

      integer :: i = 0, Out = 0, IO = 0

      open (file=output_file, encoding=E_, position=writeFilePostion, newunit=Out)
            write (Out, '(a)') writeLetter
            write (Out, '('//countPositions//'(a, 1x, i3,/))', iostat=IO) &
                (pos(i), counts(i), i = 1, countPositions) 
            call Handle_IO_status(IO, "write employee positions")
      close (Out)     

   end subroutine WriteCountPositions

end module IOEmployee 
