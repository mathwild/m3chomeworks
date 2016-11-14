program basic
    implicit none
    integer, dimension(5) :: L,P
    integer,dimension(4) :: q
    integer :: i,j
    L = (/1,2,3,4,5/)
    do i = 1,size(L)
        P(i) = sum(L(1:i))
    end do
    print *,P
end program basic