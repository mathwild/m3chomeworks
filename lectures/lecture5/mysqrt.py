"""module to compute sqrt with Newton's method"""
import numpy as np

def sqrt2(a,tol=1.0e-14,debug=False):
    """function to compute sqrt(a) with Newton's method"
    optional inputs:
        tol: convergence criteria
        debug: iteration details are output to screen if True
    """

    x0 = 1.0 #initial guess
    imax = 100000 #maximum number of iterations

    assert type(a) is int or type(a) is float, "error, input must be a number"     
    assert a>=0, "error, input must be non-negative"   
        
    for i in range(imax):
        x1 = x0/2.0 + a/(2.0*x0)
        deltax = np.abs(x1-x0)
        if debug:
            print "after iteration %d, x= %20.15f, delta x = %20.15f" %(i+1,x1,deltax)    
        if deltax < tol:
            if debug:
                print "converged"
            break    
        x0 = x1
        
    return x1

def test_sqrt2():
    """test function for sqrt2"""
    avalues = [0.1, 5, 30, 169]
    for a in avalues:
        s1 = np.sqrt(a)
        s2 = sqrt2(a)
        deltas = np.abs(s1-s2)
        print "s1=%f,s2=%f,deltas=%f" %(s1,s2,deltas)
        assert deltas<1.0e-12, "error, test failed"

                

if __name__ == '__main__':
    test_sqrt2()        
        
        
"""Note: There are three ways to 'run' the code. One is to import the module in the
ipython terminal and call the individual functions. You can also use "run mysqrt"
in the ipython terminal or "python mysqrt.py" in the general unix terminal. In these
latter two cases, __name__ == "__main__" and the if statement above is true and the
test function is called. Flags can be used with the run command to obtain timming and
profiling information: run -t, run -p
"timeit" can be used in the ipython terminal to time any available function 
"""
        
    
    
    
    
    
    






    
    
    
    
    