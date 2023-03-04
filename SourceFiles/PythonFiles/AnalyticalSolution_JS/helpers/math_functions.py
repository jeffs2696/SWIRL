#!/usr/bin/python3
import numpy as np

def getL2norm(dataset1,dataset2):

    error = abs(dataset1-dataset2)
    error_sq = error**2
    errorSum = sum(error_sq)
    print('errorSum',errorSum)
    L2 = np.sqrt((1/len(error)*errorSum) )
    return L2

