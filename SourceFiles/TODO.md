# Necessary updates to SWIRL

### Todo
 [ ]
### In Progress

- [x] fix symbolic solver when r = 0
    - [ x] Include L'Hopital's Defintion
    - [  ] Change the way source terms are being calculated in Fortran by sending 
    the entire array of data instead of sending in array entries

- [ ] fix isinitialized variable in swirlClassObject
- [ ] Fix headers in exported data
- [ ] Add tables to the plotReport 

- [ ] Clean up Python code
    - [ ] refactor symbolic solver
        - [ ] convert .ipynb to .py (notebook to script) and add to Makefile
        - [ ] fix data type conflict

- [ ] Clean up FORTRAN code
  - [ ] Add a method of reporting the ROC once the L2 gets low enough (10^-11)
  - [ ] Make a debug include file for FORTRAN files
  - [x] make an MMS test case 

- [ ] Clean up Documentation
  - [ ] Address duplicate derivations
  - [ ] Complete SWIRLDocumentationJS
  - [ ] Write meanFlowMMS report with a test case table

### Notes 
Abs cannot be converted using fcode...
### Done ✓

- [x] Turn on analysisModule
- [x] extract complex admittances from fortran into Python
- [x] Complete Fairing Function Script

- [x] Clean up Python code 
  - [x] Combine the tanh summation(TS) code with the fairing function(FF) Code
  to impose desired boundary conditions
  - [x] Incorporate the TS/FF code with the existing symbolic solver

- [x] Integrate the fairing function with the existing symbolic solver
- [x] Figure out a way to have python and FORTRAN talk to each other
