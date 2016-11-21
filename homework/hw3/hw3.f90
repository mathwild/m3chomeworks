!Code for M3C 2016 Homework 3 Mathilde Duverger CID:00978498
module network
    implicit none
    integer, dimension(:), allocatable :: qnet
    integer, dimension(:,:), allocatable :: anet,enet 
    save 
    contains 

subroutine generate(N0,L,Nt,qmax)
	!Generate recursive matrix corresponding
	!to model parameters provided as input
	implicit none
	integer, intent(in) :: N0,L,Nt
	integer, intent(out) :: qmax
        real(kind=8), dimension(:), allocatable :: PQ,P,L1
	integer :: i,j,t
	!initialise model with N0 nodes 
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
        !run model for t from 1 to Nt
        do t = 1,Nt
            !generate the probabilities vector PQ
            allocate( PQ(N0+t-1) )
            do i =1,N0+t-1
                PQ(i) = qnet(i)/dble(sum(qnet))
            end do
            !generate the cmf vector P
            allocate( P(N0+t-1) )
            do i=1,N0+t-1
                P(i) = sum(PQ(1:i))
            end do
            !generate a random sample of size L from 
            !the uniform distribution
            allocate( L1(L) )
            call random_number(L1)
            !for each element in L1 assign it to
            !a certain node if the probability at 
            !L1(j) is smaller than the cmf for 
            !that node
            do i =1,L
                do j = 1,size(P)
                    if (L1(i)<=P(j)) then
                        L1(i) = j
                        exit  
                    end if
                end do
            end do
            !add the new links created to the degrees of each node
            do i = 1,L
                j = L1(i)
                qnet(j) = qnet(j) +1
            end do 
            qnet(N0 + t) = L
            !add the new links created to our edge list
            do i = 1,L
                enet(1,N0+(t-1)*L+i) = N0+t 
                j = L1(i)
                enet(2,N0+(t-1)*L+i) = j
            end do
            deallocate( PQ )
            deallocate( P )
            deallocate( L1 ) 
        end do
        !find degree of largest hub in network
        qmax = maxval(qnet)
end subroutine generate


subroutine adjacency_matrix()
	!Create adjacency matrix corresponding 
	!to pre-existing edge list 
	implicit none
	integer :: i
	allocate( anet(size(qnet),size(qnet)) )
        do i =1,size(enet)/2
            anet(enet(1,i),enet(2,i))=1
            anet(enet(2,i),enet(1,i))=1
        end do


end subroutine adjacency_matrix

subroutine connectivity(c)
	!Compute connectivity of pre-computed network
	implicit none
	real(kind=8), intent(out) :: c
	integer, dimension(:,:), allocatable :: M,D
	integer, dimension(2) :: S
	integer :: i,LWORK,INFO
	external :: DSYEV
        intrinsic :: INT, MIN
        double precision, dimension(:,:),allocatable :: Mtemp
        double precision, dimension(:),allocatable :: WORK
        double precision, dimension(:), allocatable :: W
        S = shape(anet)
        allocate( M(S(1),S(2)))
        allocate( D(S(1),S(2)))
        !compute M the Laplacian matrix
        do i =1,size(qnet)
            D(i,i) = qnet(i)
        end do
        M = D - anet
        
        !run DSYEV
        allocate( Mtemp(S(1),S(2)))
        Mtemp = dble(M)
        allocate( WORK(1) )
        allocate( W(S(1)) )
        !query the optimal workspace
        LWORK = -1
        CALL DSYEV( 'N', 'Upper', S(1), Mtemp, S(1), W, WORK, LWORK, INFO )
        LWORK = WORK(1)
        deallocate( WORK )
        !solve eigenproblem 
        allocate( WORK(MAX(1,LWORK)) )
        CALL DSYEV( 'N', 'Upper', S(1), Mtemp, S(1), W, WORK, LWORK, INFO )
        !W is the array of eigenvalues in ascending order, 
        !c is the second smallest eigenvalue
        c = W(2)
end subroutine connectivity


subroutine vary_connectivity(N0,Lmax,Nt,carray)
	!Compute connectivity for networks with L varying
	!between 1 and Lmax
	!As the number of links added L increases in every loop
	!the connectivity decreases. There is a linear correspondence 
	!between L and c.
	implicit none
	integer, intent(in) :: N0,Lmax,Nt
	real(kind=8), dimension(Lmax),intent(out) :: carray
	integer :: L,qmax
	real(kind=8) :: c
        do L =1,Lmax
            call generate(N0,L,Nt,qmax)
            call adjacency_matrix() 
            call connectivity(c)
            carray(L) = c
            deallocate( qnet )
            deallocate( enet )
            deallocate( anet )
        end do
end subroutine vary_connectivity

end module network
