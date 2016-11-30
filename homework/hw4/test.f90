program test
	use network
	use omp_lib
	use netstats
        implicit none
	integer :: n0,l,nt,m
	integer, dimension(:,:), allocatable :: qnetm
	integer :: qmaxm, numThreads
	real(kind=8) :: qvarm, walltime
	n0 = 5
	l = 2
	nt = 400
	m = 1000
	allocate( qnetm(n0+nt,m) )
	!call stats(n0,l,nt,m,qnetm,qmaxm,qvarm)
	!print *, qmaxm
	!call stats_omp(n0,l,nt,m,numThreads,qnetm,qmaxm,qvarm)
	!print *, qmaxm
	call test_stats_omp(n0,l,nt,m,1,walltime)
        print *, walltime
        call test_stats_omp(n0,l,nt,m,2,walltime)
        print *, walltime
end program