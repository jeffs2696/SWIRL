
EXECUTABLE = runTest
TEST_EXECUTABLE = runTest1
MAIN_FILE = TestGetDissipationModule.f90
TEST_FILE = TestGetDissipationModule1.f90
MODULE_FILE = GetDissipationModule.f90

COMPILER = gfortran
OBJECTS = $(MODULE_FILE:.f90=.o)

F90FLAGS  =	\
			-o \
			-O0 \
			-std=f2008 \
			-fbacktrace -fcheck=all -fbounds-check -fcheck-array-temporaries \
			-fimplicit-none \
			-Wall -Wline-truncation -Wcharacter-truncation -Wuninitialized  \
			-Wsurprising -Warray-temporaries \
			-Wextra \
			-Werror# \
			-Wno-compare-reals \
			-Wno-unused-parameter \
			-Wno-unused-dummy-argument \
			-Wno-unused-variable\
			-Wno-array-temporaries \
			-Wno-unused-function #\
			-fdump-fortran-global \
			-Wno-error=unused-parameter \
			-Wno-error=conversion-extra \
			-Wno-error=conversion \
			-Wno-error=compare-reals \
			-Wno-error=character-truncation \
			-Wno-error=do-subscript -ffpe-trap=denormal,invalid,zero,overflow,underflow \

all: $(EXECUTABLE)

$(EXECUTABLE): $(MAIN_FILE) $(MODULE_FILE) 
	$(COMPILER) $(MAIN_FILE) $(OBJECTS) -o $(EXECUTABLE);
	./$(EXECUTABLE)

$(OBJECTS): $(MODULE_FILE)
	$(COMPILER) $(F90FLAGS) $(MODULE_FILE)
test: $(TEST_EXECUTABLE)

$(TEST_EXECUTABLE): $(TEST_FILE) $(MODULE_FILE) 
	$(COMPILER) $(TEST_FILE) $(OBJECTS) -o $(TEST_EXECUTABLE);
	./$(TEST_EXECUTABLE)

plot:
	python3 plotResult.py
plot_test:
	python3 plotTestResult.py
.PHONY: clean

clean:
		rm $(EXECUTABLE);
		rm SWIRL_Dissipation_nD*;
clean_test:
		rm $(TEST_EXECUTABLE);


