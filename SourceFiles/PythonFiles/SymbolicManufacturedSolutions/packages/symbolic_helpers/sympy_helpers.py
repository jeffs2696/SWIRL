import matplotlib.pyplot as mpl
import sympy as sp
import numpy as np 
def plotSymbolicEquation(
        title,
        x_data,
        x_title,
        y_data,
        y_title,
        x_min,
        x_max):
    '''
    '''
    # convert symbolic expressions using numpy
    lam_x  = sp.lambdify(x_data,y_data , modules=['numpy'])
    x_vals = np.linspace((x_min), (x_max), 100)
    y_vals = lam_x(x_vals)

    fig = mpl.figure(figsize=(10,6))
    ax = mpl.axes()

    ax.grid(True,which='major',axis='both',alpha=0.3)
    mpl.title(title)
    mpl.xlabel(x_title)
    mpl.ylabel(y_title)
    mpl.plot(x_vals, y_vals)
    mpl.show()

    return
