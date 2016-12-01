"""Code for M3C 2016 Homework 4 Mathilde Duverger CID:00978498
"""

import numpy as np
import matplotlib.pyplot as plt
from ns import netstats as ns #assumes netstats module has been compiled as ns.so with f2py

def convergence(n0,l,nt,m,numthreads,display=False):
    """test convergence of qmax_ave"""
    qmax_ave = np.zeros(m)
    M = range(1,m+1)
    #look at how qmax_ave varies with m
    for i in M:
        qnetm,qmaxm,qvarm = ns.stats(n0,l,nt,i)
        qmax = qnetm.max(axis=0)
        qmax_ave[i-1]= sum(qmax)/float(len(qmax))
    #fit a polynomial to the log of qmax_ave to estimate 
    #its rate of convergence
    a,b = np.polyfit(np.log(M),np.log(qmax_ave),1)
    fit = a*np.log(M) + b
    k = -a
    if (display==True):
        plt.figure()
        plt.loglog(M,qmax_ave,label='qmax_ave')
	plt.loglog(M,np.exp(fit),label='fit with convergece rate k='+str(k))
	plt.xlabel('m')
	plt.legend(loc='best')
	plt.axis([0, 1000, 5, 10.5])
	plt.title('convergence Mathilde Duverger')
	plt.show()
    return k
	     
	
def speedup(n0,l,ntarray,marray):
    """test speedup of stats_par"""
    speedup = np.zeros((np.size(ntarray),np.size(marray)))
    #compute the speedup for every nt and m
    for i in range(np.size(ntarray)):
        for j in range(np.size(marray)):
            nt = ntarray[i]
            m = marray[j]
            t1 = ns.test_stats_omp(n0,l,nt,m,1)
            t2 = ns.test_stats_omp(n0,l,nt,m,2)
            speedup[i,j] = t1/t2
    #plot the speedup depending on m and nt
    I = np.ones(np.shape(ntarray)[0])
    plt.figure()
    for i in range(np.shape(marray)[0]):
        plt.plot(ntarray,speedup[:,i],label='m='+str(marray[i]))
    plt.plot(ntarray,I)
    plt.xlabel('nt')
    plt.ylabel('speedup')
    plt.legend(loc='best')
    plt.title('speedup Mathilde Duverger')
    I = np.ones(np.shape(marray)[0])
    plt.figure()
    for i in range(np.shape(ntarray)[0]):
        plt.plot(marray,speedup[i,:],label='nt='+str(ntarray[i]))
    plt.plot(marray,I)
    plt.xlabel('m')
    plt.ylabel('speedup')
    plt.legend(loc='best')
    plt.title('speedup Mathilde Duverger')
    plt.show()
   
        
def degree_average(n0,l,nt,m,display=False):
	"""compute ensemble-averaged degree distribution"""
	"""In the plots hw421.png and hw422.png I have plotted
	the speedup depending on the values of m and nt and I have
	also plotted the line y=1 for comparison. Indeed, for the
	parallelized version to be faster than the serial version
	our lines need to be above 1, indicating t1>t2. We thus notice 
	in hw421.png that for low values of nt the parallelized version
	is slower, which is explained by the fact that the size of the problem 
	being small, the setting up of the parallelization adds time to
	the execution. For each value of nt, the speedup oscillates greatly
	but we could say that it has an overall increasing trend. Similarly, 
	we notice in hw422.png that for small nt and m the parallelization
	is not helpful. For primal use, I would recommend using the
	parallelized code for values of nt and m both above 50.
	"""
	#generate network and get max degree over all m iterations
	qnetm,qmaxm,qvarm = ns.stats_omp(n0,l,nt,m,2)
	P = np.zeros([qmaxm+1,m])
	#compute P(d) for each m
	for i in range(m):
	    bins = np.zeros(qmaxm+1)
            bins[:max(qnetm[:,i])+1] = np.bincount(qnetm[:,i])
            d = np.arange(bins.size)
            P[:,i] = (1.0*bins)/(n0+nt)
        #compute average Pa of m distributions P
        Pave = np.zeros(qmaxm+1)
        for i in range(qmaxm+1):
            Pave[i] = sum(P[i,:])/m
        #Take the non-zero values of Pa
        Pn = Pave[Pave>0]
        dn = d[Pave>0]
        #Compute fit
        #If we look at a semilog plot, we don't see a straight line and can rule out
        #an exponential form. A loglog plot does show a straight line (when q is not 'too' large)
        #which suggests a power law form, P ~ q^k or log(P) = k log(q) + const and the following fit:
        k,l = np.polyfit(np.log(dn),np.log(Pn),1)	
        if (display==True):
            plt.figure()
            plt.loglog(d,Pave,'x')
            plt.loglog(d,np.exp(l)*d**k,'k--')
            plt.xlabel('degree,d')
            plt.ylabel('Pave(d)')
            plt.title('degree_average Mathilde Duverger')
            plt.legend(('Computed','Fit, P ~ d^k with k=%f' %(k)))
            plt.show()



if __name__ == '__main__':
    convergence(5,2,10,800,1,display=True)
    ntarray = range(1,201)
    marray = np.array([5,50,100])
    speedup(5,2,ntarray,marray)
    ntarray = np.array([5,50,100])
    marray = range(1,201)
    speedup(5,2,ntarray,marray)