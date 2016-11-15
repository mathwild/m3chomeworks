!Template code for Homework 3
module network
    implicit none
    integer, dimension(:), allocatable :: qnet
    integer, dimension(:,:), allocatable :: anet,enet  

subroutine generate(N0,L,Nt,qmax)
	!Generate recursive matrix corresponding
	!to model parameters provided as input
	implicit none
	integer, intent(in) :: N0,L,Nt
	integer, intent(out) :: qmax
        real(kind=8), dimension(:), allocatable :: PQ,P,L1
	integer :: i,j,t
	!initialise model
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
                    if (L1(i)<P(j)) then
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
	allocate( anet(N0+Nt,N0+Nt) )
        do i =1,size(enet)/2
            anet(enet(1,i),enet(2,i))=1
            anet(enet(2,i),enet(1,i))=1
        end do


end subroutine adjacency_matrix

!subroutine connectivity(c)
!	!Compute connectivity of pre-computed network
!	implicit none
!	real(kind=8), intent(out) :: c
!
!end subroutine connectivity


!subroutine vary_connectivity(N0,Lmax,Nt,carray)
!	!Compute connectivity for networks with L varying
!	!between 1 and Lmax
!	implicit none
!	integer, intent(in) :: N0,Lmax,Nt
!	real(kind=8), dimension(Lmax),intent(out) :: carray


!end subroutine vary_connectivity()
end module network