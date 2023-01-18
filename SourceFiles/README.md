# How to run

The first step is to ensure that the method of manufactured solutions is working.
To generate the symbolic expressions used in the method of manufactured solutions

1. make MMS
Then, to run SWIRL...
2. make all 




## FORTRAN
### Naming Convention

*Strong Suggestions
    - All FORTRAN commands are capitalized. It is not necessary but it is used to be 
    - all SUBROUTINE calls start with a verb (create,destroy,calculate etc) 
    - The variables should be descriptive and upper case letters and underscore use should be consistent

### Compiling Notes 

Important flags:

-Wconversion-extra is not implied by -Wconversion but -Wconversion is implied by
-Wall

Use -Wno-error= () to control which errors -Wextra is catching

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


Done 
    - [ ]   

*Steps for creating actual source terms for LEE
    -1. hack into swirlClassObject and extract aa,bb
    -2. decide which individual element is needed,and zero out the rest so that 
    the sizes of the matrix product doesnt change 
    -2. multiply aa and bb by x, where x is the manufactured perturbation solutions
      - input: index of desired element in 4x4 matrix
      - output: Residual array data, S, where S=aa*x-lambda*bb*x  

