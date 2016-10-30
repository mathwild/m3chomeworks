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
    plt.show()
    plt.figure()
    plt.plot(y[:,1],y[:,2],'r',label='phase plane')
    plt.legend(loc='best')
    plt.xlabel('E')
    plt.ylabel('C')
    plt.grid()
    plt.show()
    
def linearFit(b0=5):
    """Estimate initial exponential growth rate for C(t) and 
    display actual and estimated growth vs. time
    """           

def fluStats(t,y,i1,i2):
    """Compute mean and variance for data within the range, t[i1]<=t<=t[i2]
    provided the size Nt array, t and size Nt x 4 array y which contains 
    S(t),E(t),C(t),R(t)
    """
    

        
if __name__ == '__main__':  
    y0 = [0.1,0.05,0.05,0.8]        
    t,y,S,E,C,R = solveFlu(5,100,45.6,750.0,1000.0,73.0,1.0,y0)
