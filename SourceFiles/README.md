# How to run

make MMS
make all 

# Notes 

Important flags:

-Wconversion-extra is not implied by -Wconversion but -Wconversion is implied by
-Wall

Use -Wno-error= foo to control which errors -Wextra is catching

see gcc.gnu.org/onlinedocs/gcc/invoking-GCC.html#Invoking-GCC


To Do/Problems:

*Refactor 
    -main.f90 is messy use include statements
    -clean directories and solidify file naming convention and content
    -format numbers for post processing

*Problem Statement:
*1.(context) Calculate the rate of convergence for the linearized euler 
equations (LEE) using MMS.
*2.(issue) we don't know if matrices were defined correctly in globalModule.f90
*3.(why I care) The code needs verification

*Steps for creating actual source terms for LEE
    -1. hack into swirlClassObject and extract aa,bb
    -2. decide which individual element is needed,and zero out the rest so that 
    the sizes of the matrix product doesnt change 
    -2. multiply aa and bb by x, where x is the manufactured perturbation solutions
      - input: index of desired element in 4x4 matrix
      - output: Residual array data, S, where S=aa*x-lambda*bb*x  

Questions:
*How do we compare the actual terms to the manufactured terms?  


