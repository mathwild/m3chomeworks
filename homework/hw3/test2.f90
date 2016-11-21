program test2
    use network
    implicit none
    integer :: N0,Lmax,Nt
    real(kind=8), dimension(:),allocatable :: carray
    integer :: L,qmax
    N0 = 8
    Lmax = 6
    Nt = 4000
    allocate( carray(Lmax) )
    call vary_connectivity(N0,Lmax,Nt,carray)
    print *, carray
end program
    