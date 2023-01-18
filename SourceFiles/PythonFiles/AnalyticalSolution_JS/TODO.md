# Priority 
- [ ] ensure that data that is being overwritten each loop is saved from within
the code. Objects for a given radial mode number and grid could be easier 
to set up in a class and loop through

- [x] use Numpy arrays instead of lists
- [x] loop through radial mode number from 0 to number of zeros i.e. number of
radial modes
- [x] compute the L2 for each identified mode for both radial mode and axial wavenumber
- [x] compute the Lmax and index location 
- [x] compute ROC
- [x] store for every identified mode per grid per radial mode number
- [ ] compute velocities from pressure using known expressions and relevant errors
- [ ] Once done, document, document, document

# Secondary
-[ ] Make modules
 - [x] output radial wavenumber, radial mode for the same wavenumber.
 - [x] account for m = 0 in axial_wave_number_quadratic_function
 - [x] figure out a way to import modes where the input is an acceptable decay
 rate

 - [ ] account for annular ducts in the axial_wavenumber_quadratic function
# Refactor Goals
-[ ] Make Classes for objects 
-[x] Make unique index variable names 
