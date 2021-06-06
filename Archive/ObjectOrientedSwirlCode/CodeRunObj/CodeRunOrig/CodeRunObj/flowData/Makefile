F90 = gfortran
EXE = getFlowData

OBJ_EXT = .o
F90_EXT = .f90


# Defining Source Code and Defining their object files after
SRC = GenerateFlowData$(F90_EXT)
OBJ = $(SRC:$(F90_EXT)=$(OBJ_EXT))

SRC_LOCATION = SourceCode
OBJ_LOCATION = ObjectFiles

all: $(OBJ)
	$(F90) -o $(EXE) $(OBJ); \
mv *.o ObjectFiles

clean:
	rm -f $(OBJ_LOCATION)/*.o *.input 
	    
GenerateFlowData.o: $(SRC_LOCATION)/GenerateFlowData.f90
	$(F90) -c -O $(SRC_LOCATION)/GenerateFlowData.f90

