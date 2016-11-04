"""Homework 2, part 2"""

"""Name and CID"""
Output = ["Mathilde Duverger","00978498"]
print "M3C 2016 Homework 2 by", Output[0]
print "CID:", Output[1]

import numpy as np
import matplotlib.pyplot as plt

def solveFlu(T,Nt,a,b0,b1,g,k,y0):
    """Obtains solution to Flu model from t=0 to t=T with
    initial conditions, y0, and model parameters, a,b0,b1,g,k.
    """
    def Flu(y,t,a,b0,b1,g,k):
        """Implements the flu ode"""
        S,E,C,R = y
        b = b0 + b1*(1+np.cos(2*np.pi*t))
        dydt = [k*(1-S)-b*C*S,b*C*S-(k+a)*E,a*E-(g+k)*C,g*C-k*R]
        return dydt
    t = np.linspace(0,T,Nt)
    from scipy.integrate import odeint
    """Find solution for flu ode"""
    sol = odeint(Flu,y0,t,args=(a,b0,b1,g,k))
    S = sol[:,0]
    E = sol[:,1]
    C = sol[:,2]
    R = sol[:,3]
    return t,sol,S,E,C,R

def displaySolutions(t,y):
    """Plot time series and phase plane for the numerical solution;
    input variable y contains S(t),E(t),C(t),R(t)
    """     
    plt.figure()
    plt.plot(t,y[:,2],'b',label='C(t)')
    plt.legend(loc='best')
    plt.xlabel('t')
    plt.grid()
    plt.title('Mathilde Duverger displaySolutions')
    plt.figure()
    plt.plot(y[:,2],y[:,1],'r',label='phase plane')
    plt.legend(loc='best')
    plt.xlabel('C')
    plt.ylabel('E')
    plt.grid()
    plt.title('Mathilde Duverger displaySolutions')
    plt.show()
    
def linearFit(b0=5):
    """Estimate initial exponential growth rate for C(t) and 
    display actual and estimated growth vs. time
    """
    y0 = [1-0.003,0.001,0.001,0.001]  
    T = 10
    Nt = 10000      
    t,y,S,E,C,R = solveFlu(T,Nt,1.0,b0,0.2,0.2,0.1,y0)
    """Extract C1 where C<=0.1"""  
    i = np.where(C<=0.1)
    C1 = C[i] 
    t1 = t[i]   
    lC1 = np.log(C1)
    mu,c = np.polyfit(t1,lC1,1)
    lC = np.log(C)
    poly = c + mu*t
    """Semilog plot full solution C(t) and estimated growth"""
    plt.figure()
    plt.plot(t,lC,'b',label='log(C(t))')
    plt.plot(t,poly,'r',label='Fitted polynomial')
    plt.legend(loc='best')
    plt.xlabel('t')
    plt.grid()
    plt.show()
    print mu
    
def fluStats(t,y,i1,i2):
    """Compute mean and variance for data within the range, t[i1]<=t<=t[i2]
    provided the size Nt array, t and size Nt x 4 array y which contains 
    S(t),E(t),C(t),R(t)
    """
    s = np.shape(t)
    assert i1>=0, "error, i1 should be bigger or equal to 0"
    assert i2<=s[0], "error, i2 should be smaller or equal than the length of t"
    assert i1<i2, "error, i2 should be bigger than i1"
    Si = y[i1:i2,0]
    Ei = y[i1:i2,1]
    Ci = y[i1:i2,2]
    Ri = y[i1:i2,3]
    mean = [np.mean(Si),np.mean(Ei),np.mean(Ci),np.mean(Ri)]
    var = [np.var(Si),np.var(Ei),np.var(Ci),np.var(Ri)]
    return mean,var
  
if __name__ == '__main__':  
    y0 = [0.1,0.05,0.05,0.8]  
    T = 5
    Nt = 10000     
    t,y,S,E,C,R = solveFlu(T,Nt,45.6,750.0,1000.0,73.0,1.0,y0)
    """To have t>1 we use Nt/T since we know that the vector 
    t is equispaced"""
    displaySolutions(t[Nt/T:],y[Nt/T:])
    mean,var = fluStats(t,y,Nt/2,Nt)