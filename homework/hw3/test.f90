program test
        implicit none
	integer :: N0,L,Nt
	integer, dimension(2) :: L1
	!integer, intent(out) :: qmax
	integer, dimension(:), allocatable :: qnet
        integer, dimension(:,:), allocatable :: enet
        real(kind=8), dimension(:), allocatable :: PQ,P
	integer :: i,i1,t,j1,m,m1,u
	!initialise model
	N0 = 5
	L = 2
	L1 = (/1,3/)
	Nt = 3
	allocate ( qnet(N0+Nt) )
	allocate( enet(2,N0+Nt*L) ) 
	do i = 1,N0-1
	   qnet(i) = 2
	   enet(1,i) = i
	   enet(2,i) = i+1
	end do
        qnet(N0) = 2
        enet(1,N0) = N0
        enet(2,N0) = 1
        !run model
        print *,qnet
        !allocate( PQ(N0+t-1) )
        !do u =1,N0+t-1
        !    PQ(u) = qnet(u)/sum(qnet)
        !end do
        !do t = 1,Nt
            !generate the cmf vector P
          !  allocate( PQ(N0+t-1) )
           ! do u =1,N0+t-1
            !    PQ(u) = qnet(u)/sum(qnet)
            !end do
            !do j1 = 1,L
             !   i1 = L1(j1)
              !  qnet(i1) = qnet(i1) +1
            !end do 
            !qnet(N0 + t) = L
            !do m = 1,L
             !   enet(1,N0+(t-1)*L+m) = N0+t 
              !  m1 = L1(m)
               ! enet(2,N0+(t-1)*L+m) = m1
            !end do
        !end do
        !print *, PQ
end program test