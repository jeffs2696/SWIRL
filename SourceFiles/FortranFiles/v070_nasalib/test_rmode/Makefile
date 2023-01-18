# Notes:
# Theres a way to append filetypes easily.

COMPILER = gfortran
COMPILER_FLAGS = -o
EXECUTABLE = testing_analytic_solution.exe

SUBROUTINES = \
			  ../besy.f \
			  ../besj.f \
			  ../rmode.f \
			  ../anrt.f \
			  ../anfu.f \
			  ../isig.f \
			  ../eigen.f 

MAIN_CODE = BesselFunctionCode.f90

all:
	($(COMPILER) $(MAIN_CODE) $(SUBROUTINES) -o $(EXECUTABLE); ./$(EXECUTABLE));
	python3 plotResult.py

plot: 
	python3 plotResult.py
clean:
	rm *.exe fort.*

