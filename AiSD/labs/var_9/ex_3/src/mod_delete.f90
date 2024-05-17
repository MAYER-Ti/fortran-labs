module mod_Delete
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use mod_IO

   implicit none
   private
   
   public :: Delete

contains
    pure subroutine Delete(dList) !, SortedList)
       type(list), intent(inout) :: dList

       Nullify(dList%sorted)
       call DelNode(dList%head)
       
    end subroutine Delete

    pure recursive subroutine DelNode(curNode)
       type(node), pointer :: curNode

       type(node), pointer :: nextNode

       nextNode => curNode%next
       deallocate(curNode)

       if(Associated(nextNode)) &
          call DelNode(nextNode)
    end subroutine DelNode

end module mod_Delete
