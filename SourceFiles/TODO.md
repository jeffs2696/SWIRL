# Necessary updates to SWIRL

### Todo

- [ ] Clean up MMS 
  - [ ] remove magic numbers 
  - [ ] fix up fortran code in SoundSpeedMMS.f90
  - [ ] Debug flag to plot symbolic funcitons in python
    - Note: it requires the user to define values for any variables other than r
  - [ ] Use FF to define the speed of sound (or maybe just a 2nd order polynomial)


- [ ] Clean up Fortran code
  - [ ] Make sure all the data types are the same in SWIRL 
  - [ ] Make a debug include file for FORTRAN files
  - [ ] Write meanFlowMMS report with a test case table
  - [ ] make an MMS test case 

- [ ] Clean up Documentation
  - [ ] Address duplicate derivation 
### In Progress

- [ ] Clean up FORTRAN codes by incorporating several derived data types 


### Done ✓

- [x] Complete Fairing Function Script

- [x] Clean up Python code 
  - [x] Combine the tanh summation(TS) code with the fairing function(FF) Code
  to impose desired boundary conditions
  - [x] Incorporate the TS/FF code with the existing symbolic solver

- [x] Integrate the fairing function with the existing symbolic solver
