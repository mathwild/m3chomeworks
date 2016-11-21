"""Code for M3C 2016 Homework 3 Mathilde Duverger CID:00978498
"""
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from n1 import network as net 

def visualize(e):    
    """Visualize a network represented by the edge list
    contained in e
    """
    n = max(np.unique(e))
    fig, ax = plt.subplots()
    X = []
    Y = []
    #plots the nodes
    for i in range(n):
        x = np.random.uniform()
        y = np.random.uniform()
        X.extend([x])
        Y.extend([y])
        circle = plt.Circle((x,y),0.01) 
        ax.add_artist(circle)
    #plots the lines between the nodes
    for i in range(np.shape(e)[0]):
        line = matplotlib.lines.Line2D((X[e[i,0]-1],X[e[i,1]-1]),(Y[e[i,0]-1],Y[e[i,1]-1]),linewidth=0.2,color='black')
        ax.add_line(line)
    plt.show()
 
def degree(N0,L,Nt,display=False):
    """Compute and analyze degree distribution based on degree list, q,
    corresponding to a recursive network (with model parameters specified
    as input.
    The functional form seen in the saved figure is the exponential of 
    the polyfit of degree 3 to the log of the first Np/2 values of P that
    are non zero.
    """
    qmax = net.generate(N0,L,Nt)
    q = net.qnet.tolist()
    P = np.zeros(qmax)
    d = np.array(range(1,qmax+1))
    #compute degree distribution
    for i in d:
        P[i-1] = float(q.count(i))/float(np.size(q))
    Np = np.size(P)
    #if Nt>1000 compute a functional form for P(d)
    if (Nt>1000):
        P1 = (P[:Np/2 +1])
        P1 = P1[np.nonzero(P1)]
        lP1 = np.log(P1)
        a1,a2,a3,a4 = np.polyfit(d[np.nonzero(P1)],lP1,3)
        f = np.exp(a1*np.power(d,3)+a2*np.power(d,2)+a3*np.power(d,1)+a4*np.power(d,0))
    #if display is True then plot the distribution and fit if Nt>1000
    if (display==True):
        width = 1/1.5
        plt.figure()
        plt.bar(d,P,width,align='center')
        if (Nt>1000):
            plt.plot(d,f,'r',label='Fit to P(d)')
        plt.xticks(np.arange(min(d), max(d)+1, 10))
        plt.title('Mathilde Duverger degree')
        plt.xlabel('d')
        plt.ylabel('P(d)')
        plt.legend(loc='best')
        plt.show()
    return d,P
    
#if __name__ == "__main__":