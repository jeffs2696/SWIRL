# Necessary updates to SWIRL

### Todo

- [x] Clean up Python code 
  - [x] Combine the tanh summation(TS) code with the fairing function(FF) Code
  to impose desired boundary conditions
  - [x] Incorporate the TS/FF code with the existing symbolic solver
  - [x] BUG - FF are not imposing the boundary conditions for some reason  
  - Note: A_min = A_max - 1 not 1 - A_max

- [ ] Clean up Fortran code
  - [ ] Make sure all the data types are the same in SWIRL 
  - [ ] Make a debug include file for FORTRAN files
  - [ ] Write meanFlowMMS report with a test case table
  - [ ] make an MMS test case 
- [ ] Clean up Documentation
  - [ ] Address duplicate derivation 
### In Progress

- [ ] Integrate the fairing function with the existing symbolic solver
- [ ] Clean up FORTRAN codes by incorporating several derived data types 


### Done ✓

- [x] Complete Fairing Function Script

