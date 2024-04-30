module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

    pure subroutine MovePartList(indexFirstLine, indexLastLine, indexToPaste, Code)
       integer, intent(in) :: indexFirstLine, indexLastLine, indexToPaste
       type(SourceLine), pointer, intent(inout) :: Code

       type(SourceLine), pointer :: firstLine
       type(SourceLine), pointer :: lastLine
       type(SourceLine), pointer :: toPasteLine
       type(SourceLine), pointer :: tmp1
       type(SourceLine), pointer :: tmp2

       call GetLineOnIndex(Code, indexFirstLine, 1, firstLine)
       call GetLineOnIndex(firstLine, indexLastLine, indexFirstLine, lastLine)
       call GetLineOnIndex(Code, indexToPaste, 1, toPasteLine)

       tmp1 => lastLine%next

       tmp2 => toPasteLine%next 
       toPasteLine%next => firstLine
       lastLine%next => tmp2 

       firstLine => tmp1

    end subroutine MovePartList 

    pure recursive subroutine GetLineOnIndex(line, searchIndex, indexCur, searchLine)
       integer, intent(in) :: searchIndex, indexCur
       type(SourceLine), target, intent(inout) :: line
       type(SourceLine), pointer, intent(inout) :: searchLine

       if(indexCur == searchIndex) then
          searchLine => line
       end if

       if (Associated(line%next) .and. (indexCur < searchIndex)) &
          call GetLineOnIndex(line%next, searchIndex, indexCur+1, searchLine) 
    end subroutine GetLineOnIndex
   
!   ! Формирование разницы двух кодов в виде новых строк.
!   pure recursive function Diff_Codes(InitialCode, ModdedCode) result(DiffCode)
!      type(SourceLine), pointer     :: DiffCode
!      type(SourceLine), intent(in)  :: InitialCode
!      type(SourceLine), intent(in)  :: ModdedCode
!
!      ! Поиск и запись отличных строк в рамках исходного файла InitialCode.
!      ! Если строки равны:
!      if (InitialCode%String == ModdedCode%String) then
!         ! Если остались ещё строки, то переход к следующей.
!         if (Associated(InitialCode%next)) then
!            DiffCode => Diff_Codes(InitialCode%next, ModdedCode%Next)
!         ! В противном случае если остались строки в модифицированном файле, то добавление их в список.
!         else if (Associated(ModdedCode%next)) then
!            ! Запись всех строк оставшейся части ModdedCode.
!            DiffCode => Add_Recent_Source_Lines(ModdedCode%next)
!         end if !  ELSE DiffCode => Null()
!      ! Если строки не равны, то добавление её в список.
!      else
!         allocate (DiffCode)
!         DiffCode%String = CH__"++ " // ModdedCode%String
!         DiffCode%next => Diff_Codes(InitialCode, ModdedCode%Next)
!      end if
!   end function Diff_Codes
!
!   pure recursive function Add_Recent_Source_Lines(ModdedCode) result(DiffCode)
!      type(SourceLine), pointer     :: DiffCode
!      type(SourceLine), intent(in)  :: ModdedCode
!
!      allocate (DiffCode)
!      DiffCode%String = CH__"++ " // ModdedCode%String
!      if (Associated(ModdedCode%next)) &
!         DiffCode%next => Add_Recent_Source_Lines(ModdedCode%Next)
!   end function Add_Recent_Source_Lines
!
end module Source_process
