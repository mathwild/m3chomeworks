!Template code for hw4
!Should be compiled with network.f90
module netstats
	use network
	use omp_lib

	contains

subroutine stats(n0,l,nt,m,qnetm,qmaxm,qvarm)
	!Input: 
	!n0,nl,nt: recursive network model parameters
	!m: number of network realizations to compute
	!Output:
	!qnetm: node lists for all m networks
	!qvarm: ensemble averages of var(q)
	!qmaxm: maximum qmax from m network realizations
	implicit none
	integer, intent(in) :: n0,l,nt,m
	integer, dimension(n0+nt,m), intent(out) :: qnetm
	integer, intent(out) :: qmaxm
	real(kind=8), intent(out) :: qvarm
	integer, dimension(n0+nt) :: qnet
	integer, dimension(n0+l*nt,2) :: enet
	real(kind=8) :: qmean
	real(kind=8), dimension(m) :: qvar
	integer :: i, qmax
	qmaxm = 0
        do i = 1,m
            call generate(n0,l,nt,qmax,qnet,enet)
            qnetm(:,i) =  qnet
            qmean = sum(qnet)/(n0+nt)
            qvar(i) = sum((qnet-qmean)**2)/(n0+nt)
            qmaxm = max(qmaxm,qmax)
        end do 
        qvarm = sum(qvar)/m
        
end subroutine stats


subroutine stats_omp(n0,l,nt,m,numThreads,qnetm,qmaxm,qvarm)
	!Input: 
	!n0,nl,nt: recursive network model parameters
	!m: number of network realizations to compute
	!numThreads: number of threads for parallel computation
	!Output:
	!qnetm: node lists for all m networks
	!qvarm: ensemble averages of var(q)
	!qmaxm: maximum qmax from m network realizations
	implicit none
	integer, intent(in) :: n0,l,nt,m,numThreads
	integer, dimension(n0+nt,m), intent(out) :: qnetm
	integer, intent(out) :: qmaxm
	real(kind=8), intent(out) :: qvarm
	real(kind=8), dimension(m) :: qvar
	real(kind=8) :: qmean
	integer, dimension(n0+nt) :: qnet
	integer, dimension(n0+l*nt,2) :: enet
	integer :: i, qmax
	!$call omp_set_num_threads(numThreads)
	qmaxm = 0
	!$OMP parallel do private(qmax,qmean)
        do i = 1,m
            call generate(n0,l,nt,qmax,qnetm(:,i),enet)
            qmean = sum(qnetm(:,i))/(n0+nt)
            qvar(i) = sum((qnetm(:,i)-qmean)**2)/(n0+nt)
            qmaxm = max(qmaxm,qmax)
        end do 
        !$OMP end parallel do 
        qvarm = sum(qvar)/m


end subroutine stats_omp


subroutine test_stats_omp(n0,l,nt,m,numThreads,walltime)
	!Input: same as stats_omp
	!Output: walltime: time for 100 calls to stats_par
	implicit none
	integer, intent(in) :: n0,l,nt,m,numThreads
	real(kind=8), intent(out) :: walltime
	integer, dimension(n0+nt,m) :: qnetm
	integer :: qmaxm,i
	real(kind=8), dimension(100) :: wtime
	real(kind=8) :: qvarm
	integer :: start,finish,clockrate
        if (numThreads>1) then 
            print *, 'parallel'
            do i=1,100
                call system_clock(start)
                call stats_omp(n0,l,nt,m,numThreads,qnetm,qmaxm,qvarm)
                call system_clock(finish,clockrate)
                wtime(i) = float(finish-start)/float(clockrate)
            end do
            walltime = sum(wtime)/100
        else 
            print *, 'normal'
            do i=1,100 
                call system_clock(start)
                call stats(n0,l,nt,m,qnetm,qmaxm,qvarm)
                call system_clock(finish,clockrate)
                wtime(i) = float(finish-start)/float(clockrate)
            end do
            walltime = sum(wtime)/100
        end if 

end subroutine test_stats_omp


end module netstats
