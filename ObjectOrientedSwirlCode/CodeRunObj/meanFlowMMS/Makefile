F90 = gfortran
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
	$(F90) -c -O $(SRC_LOCATION)/SoundSpeedMMS.f90

