program test
        implicit none
	integer :: N0,L,Nt
	!integer, intent(out) :: qmax
	integer, dimension(:), allocatable :: qnet
        integer, dimension(:,:), allocatable :: enet,anet,D
        real(kind=8), dimension(:,:), allocatable :: M
        real(kind=8), dimension(:), allocatable :: PQ,P,L1
        integer, dimension(2) :: S
	integer :: i,j,t,qmax
	EXTERNAL         DSYEV
        EXTERNAL         PRINT_MATRIX
        INTEGER          LWMAX
        PARAMETER        ( LWMAX = 1000 )
        INTRINSIC        INT, MIN
        INTEGER          INFO, LWORK
        DOUBLE PRECISION WORK( LWMAX )
        double precision, dimension(:), allocatable :: W
	!initialise model
	N0 = 5
	L = 2
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
        do i = N0+1,N0+Nt
            qnet(i) = 0
        end do
        !run model
        do t = 1,Nt
            !generate the cmf vector P
            allocate( PQ(N0+t-1) )
            do i =1,N0+t-1
                PQ(i) = qnet(i)/dble(sum(qnet))
            end do
            allocate( P(N0+t-1) )
            do i=1,N0+t-1
                P(i) = sum(PQ(1:i))
            end do
            allocate( L1(L) )
            call random_number(L1)
            do i =1,L
                do j = 1,size(P)
                    if (L1(i)<=P(j)) then
                        L1(i) = j
                        exit  
                    end if
                end do
            end do
            do i = 1,L
                j = L1(i)
                qnet(j) = qnet(j) +1
            end do 
            qnet(N0 + t) = L
            do i = 1,L
                enet(1,N0+(t-1)*L+i) = N0+t 
                j = L1(i)
                enet(2,N0+(t-1)*L+i) = j
            end do
            deallocate( PQ )
            deallocate( P )
            deallocate( L1 ) 
        end do
        qmax = maxval(qnet)
        
        
        !adjacency_matrix
        allocate( anet(N0+Nt,N0+Nt) )
        do i =1,size(enet)/2
            anet(enet(1,i),enet(2,i))=1
            anet(enet(2,i),enet(1,i))=1
        end do
        
        !connectivity 
        S = shape(anet)
        allocate( M(S(1),S(2)))
        allocate( D(S(1),S(2)))
        allocate( W(S(1)) )
        do i =1,size(qnet)
            D(i,i) = qnet(i)
        end do
        M = D - anet
        
        LWORK = -1
        CALL DSYEV( 'N', 'Upper', S(1), M, S(1), W, WORK, LWORK, INFO )
        LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )

        CALL DSYEV( 'N', 'Upper', S(1), M, S(1), W, WORK, LWORK, INFO )
end program test