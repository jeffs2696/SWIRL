F90 = gfortran
F90FLAGS = -O
SRC_EXT = .f90
OBJ_EXT = .o
SRC = CompareL2.f90 L2NormModule.f90
OBJ = $(SRC:.f90=.o)

all: $(OBJ)
	$(F90) -o outt $(OBJ) 

CompareL2.o: CompareL2.f90 \
             L2NormModule.o
	$(F90) $(F90FLAGS) CompareL2.f90	

L2NormModule.o: L2NormModule.f90
	$(F90) $(F90FLAGS) L2NormModule.f90
