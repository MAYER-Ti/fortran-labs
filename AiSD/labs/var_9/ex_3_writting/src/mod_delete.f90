module mod_Delete
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use mod_IO

   implicit none

contains

    pure recursive subroutine Delete(curNode)
       type(node), pointer :: curNode

       type(node), pointer :: nextNode

       nextNode => curNode%next
       deallocate(curNode)

       if(Associated(nextNode)) &
          call Delete(nextNode)
    end subroutine Delete

end module mod_Delete
