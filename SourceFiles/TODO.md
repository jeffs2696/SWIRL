# Necessary updates to SWIRL

### Todo
 [ ]
### In Progress

- [ ] fix solver when r = 0
- [ ] fix isinitialized variable in swirlClassObject
- [ ] Fix headers in exported data
- [ ] Add tables to the plotReport 

- [ ] Clean up Python code
    - [ ] refactor symbolic solver
        - [ ] convert .ipynb to .py (notebook to script)
        - [ ] 

- [ ] Clean up FORTRAN code
  - [ ] Make sure all the data types are the same in SWIRL 
  - [ ] Make a debug include file for FORTRAN files
  - [ ] Write meanFlowMMS report with a test case table
  - [x] make an MMS test case 

- [ ] Clean up Documentation
  - [ ] Address duplicate derivations
  - [ ] Complete SWIRLDocumentationJS

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
