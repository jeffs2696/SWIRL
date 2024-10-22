# SWIRL 

## A frequency domain linearized Euler equation code for ducted sound propagation 

SWIRL analyzes axial flows with mean shear and swirl in lined and unlined ducts.

### Table of Contents

- [Description](#description)
- [How To Use](#how-to-use)
- [References](#references)
- [Contributors](#contributors)

## Description
The code was originally written in Fortran 77 and is accompanied with the paper
"Kousen, Kenneth A. "Eigenmodes of ducted flows with radially-dependent axial 
and swirl velocity components." (1999)." [Here is the paper](https://core.ac.uk/download/pdf/10475014.pdf)

### File Directory Structure
* SourceFiles
  * PythonFiles
  * FortranFiles
* CodeRun
  * 01-mean-flow/
    * MMS1 
  * 02-method-of-manufactured-solutions/
    * MMS1 
  * 03-EVanalysis/
  * 04-plotReport/
    * MMS1
* LibraryFiles
* ModuleFiles
* ObjectFiles
* Documentation
* PostProcessing
* TestCaseFiles 
 
## How To Use
The main Makefile is in the SourceFiles folder. The Makefile compiles SWIRL,
runs the executable. There is an option that allows the user to display a latex
report with the results plotted

```
make all
```
## Installation
* clone this project 
* type ```make```

This code was developed using Linux and has been compiled using gfortran. 
Post processing was done using python. 

Depending on your system, your ploting windows may or may not appear. In
particular, if you are using WSL 1 or 2 make sure that you have xLaunch or 
xming enabled with the right display window. 

### Fortran Prerequisites 
gfortran

### Python Prerequisites 
cycler
virtualenv
Numpy
Sympy
Scipy
### Known Issues

### To - Do 
[ ] - Normalize Mode Shapes (in FORTRAN)
[ ] - Take out Kousen's table mentions in the captions
[ ] - Include Analytical Modes with Bessel functions
[ ] - mention Kink in axial Mach number for turbulent profile (Table 4.6) 
[ ] - make example plots to discuss cut on and cut off modes

[Back To The Top](#SWIRL)
## Contributors
- Jeffrey Severino <jseveri@rockets.utoledo.edu>
- Zaid Sabri
- Dr. Ray Hixon 
