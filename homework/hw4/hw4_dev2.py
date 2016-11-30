"""template code for hw4
"""
import numpy as np
import matplotlib.pyplot as plt
from ns import netstats as ns #assumes netstats module has been compiled as ns.so with f2py
import time

def convergence(n0,l,nt,m,numthreads,display=False):
    """test convergence of qmax_ave"""
    qmax_ave = np.zeros(m)
    M = range(1,m+1)
    for i in M:
        qnetm,qmaxm,qvarm = ns.stats(n0,l,nt,i)
        qmax = qnetm.max(axis=0)
        qmax_ave[i-1]= sum(qmax)/float(len(qmax))
    a,b = np.polyfit(np.log(M),np.log(qmax_ave),1)
    fit = a*np.log(M) + b
    k = -a
    if (display==True):
        plt.figure()
        plt.loglog(M,qmax_ave)
	plt.loglog(M,np.exp(fit))
    return k
	     
	
def speedup(n0,l,ntarray,marray):
    """test speedup of stats_par"""
    speedup = np.zeros((np.size(ntarray),np.size(marray)))
    for i in range(np.size(ntarray)):
        for j in range(np.size(marray)):
            nt = ntarray[i]
            m = marray[j]
            t1 = ns.test_stats_omp(n0,l,nt,m,1)
            t2 = ns.test_stats_omp(n0,l,nt,m,2)
            speedup[i,j] = t1/t2
    plt.figure()
    plt.plot(ntarray,speedup[:,0])
    plt.figure()
    plt.plot(marray,speedup[0,:])
        
	


def degree_average(n0,l,nt,m,display=False):
	"""compute ensemble-averaged degree distribution"""	
        



#if __name__ == '__main__':
    #ntarray = np.array([5,10,50,100,500,1000])
    #marray = np.array([5,10,50,100,500,1000])