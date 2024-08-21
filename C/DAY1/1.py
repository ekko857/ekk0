from ipywidgets import interact
import matplotlib.pyplot as plt
import numpy as np

def func(x):
    return x**3

def htrans(c):
    x = np.linspace(-3,3)
    
    y = func(x+c) #constant c in the argument of the function
    
    plt.plot(x,y, label='$f(x+c)$')
    plt.title('Horizontal translation')
    plt.legend(fontsize=12)
    
    plt.ylim(-8, 8) #remember to change the limits for better visualisation if necessary
    
    plt.axhline(0, color='black', lw=1)
    plt.axvline(0, color='black', lw=1)
    
    

    plt.show()

def vtrans(c):
    x = np.linspace(-3,3)
    
    y = func(x)+c #constant c summing the whole function
    
    plt.plot(x,y,  label='$f(x)+c$')
    plt.title('Vertical translation')
    
    plt.xlabel('x')
    plt.ylabel('y')
    plt.legend(fontsize=12)
    
    plt.ylim(-8, 8) #remember to change the limits for better visualisation if necessary
        
    plt.axhline(0, color='black', lw=1)
    plt.axvline(0, color='black', lw=1)
    
    plt.show()
    
# create a slider

interact(htrans, c=(-2.0,2.0))
interact(vtrans, c=(-2.0,2.0))