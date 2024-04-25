module SearchesGroup 
   use Environment
   use IOGroup

   implicit none

contains
   
pure recursive function FirstForAlphIndex(surnames, minindex, i) result(searchIndex)
    character(SURNAME_LEN, kind=CH_), intent(in) :: surnames(GROUP_COUNT)
    integer, intent(in)                          :: minindex, i
    integer                                      :: searchIndex

    if (i < GROUP_COUNT) then
        if (surnames(minindex) > surnames(i)) then
            searchIndex = i
            searchIndex = min(searchIndex, FirstForAlphIndex(surnames, i, i+1))
        else
            searchIndex = FirstForAlphIndex(surnames, minindex, i+1)
        end if
    else
        searchIndex = minindex
    end if

end function FirstForAlphIndex

end module SearchesGroup 
