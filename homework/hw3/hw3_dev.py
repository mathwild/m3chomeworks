"""Template code for M3C 2016 Homework 3
"""
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

 #A = np.array([[1, 2], [1, 5],[2, 3],[2, 5],[3, 4],[4, 5],[4, 6]])

def visualize(e):    
    """Visualize a network represented by the edge list
    contained in e
    """
    n = np.shape(np.unique(e))[0]
    fig, ax = plt.subplots()
    X = []
    Y = []
    for i in range(n):
        x = np.random.uniform()
        y = np.random.uniform()
        X.extend([x])
        Y.extend([y])
        circle = plt.Circle((x,y),0.01) 
        ax.add_artist(circle)
    for i in range(np.shape(e)[0]):
        line = matplotlib.lines.Line2D((X[e[i,0]-1],X[e[i,1]-1]),(Y[e[i,0]-1],Y[e[i,1]-1]),linewidth=0.2,color='black')
        ax.add_line(line)
    plt.show()
 
def degree(N0,L,Nt,display=False):
    """Compute and analyze degree distribution based on degree list, q,
    corresponding to a recursive network (with model parameters specified
    as input.
    """
    
#if __name__ == "__main__":