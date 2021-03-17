F90 = gfortran
F90FLAGS  = -g -c -Og -std=f2008 -fbounds-check -fcheck-array-temporaries \
			-Wconversion -Wsurprising -Wall -ffpe-trap=invalid
EXE = getFlowData

OBJ_EXT = .o
F90_EXT = .f90


# Defining Source Code and Defining their object files after
SRC = SoundSpeedMMS$(F90_EXT)
OBJ = $(SRC:$(F90_EXT)=$(OBJ_EXT))

SRC_LOCATION = SourceCode
OBJ_LOCATION = ObjectFiles

all: $(OBJ)
	$(F90) -o $(EXE) $(OBJ); \
mv *.o ObjectFiles; \
./$(EXE)

clean:
	rm -f $(OBJ_LOCATION)/*.o *.input 
	    
SoundSpeedMMS.o: $(SRC_LOCATION)/SoundSpeedMMS.f90
	$(F90) $(F90FLAGS) $(SRC_LOCATION)/SoundSpeedMMS.f90

