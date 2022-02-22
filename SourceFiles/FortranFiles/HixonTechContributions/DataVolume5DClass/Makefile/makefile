### MAKEFILE DEFINITIONS ###

F95_IFC      = ifort
F95FLAGS_IFC = -c  -O0 -CU -CB
#F95FLAGS_IFC = -c  -O3 -pad -align -prec_div
LDFLAGS_IFC  = -static

F95_GFORTRAN      = mpif90
#F95FLAGS_GFORTRAN = -c -O3
F95FLAGS_GFORTRAN = -c -g -O0 -Wall -Werror -Wline-truncation -Wuninitialized -std=f2008 -fbounds-check
F95FLAGS2_GFORTRAN = -c -g -O0 -Wall -Werror -Wline-truncation -Wuninitialized -std=f2008 -fbounds-check
LDFLAGS_GFORTRAN  =

F95_G95      = g95
#F95FLAGS_G95 = -c -O3
F95FLAGS_G95 = -c -O0 -Wall -Werror -Wline-truncation -Wmissing-intent -Wuninitialized -Wprecision-loss -std=f2008 -fbounds-check -ftrace=full
LDFLAGS_G95  =

F95_SICOR      = mpif90
F95FLAGS_SICOR = -c -O3 -64
F95FLAGS2_SICOR = -c -O0 -64
LDFLAGS_SICOR  = -v -64

F95      = $(F95_GFORTRAN)
F95FLAGS = $(F95FLAGS_GFORTRAN)
F95FLAGS2 = $(F95FLAGS2_GFORTRAN)
LDFLAGS  = $(LDFLAGS_GFORTRAN)

# define locations for files

LIB_FILES = ../LibraryFiles
MOD_FILES = ../ModuleFiles
OBJ_FILES = ../ObjectFiles
EXE_FILES = ../Executables
SRC_FILES = ../SourceCode

CMD = DataVolumeTest.exe

DATA_VOLUME_CLASS_LIBS = $(LIB_FILES)/libDataVolume5DClass.a

DATA_VOLUME_CLASS_OBJS = $(OBJ_FILES)/DataVolume5DClass.o \
                         $(OBJ_FILES)/DataVolume5DClassInt4.o \
                         $(OBJ_FILES)/DataVolume5DClassInt8.o \
                         $(OBJ_FILES)/DataVolume5DClassRealSP.o \
                         $(OBJ_FILES)/DataVolume5DClassRealDP.o

LIBS_NEEDED = \
 $(LIB_FILES)/libMessagePassingInterface.a \
 $(LIB_FILES)/libErrorInformationClass.a \
 $(LIB_FILES)/libQuickSortClass.a \
 $(LIB_FILES)/libMPI2DataClassNoErrClass.a


OBJS = $(DATA_VOLUME_CLASS_OBJS)

# define global paths

INTERFACE_WITH_MPI_LOCATION = ../../InterfaceWithMPI
ERROR_INFORMATION_CLASS_LOCATION = ../../ErrorInformationClass
MPI2_DATA_CLASS_LOCATION = ../../MPI2DataClass
QUICK_SORT_CLASS_LOCATION = ../../QuickSortClass

# MPICH2 definitions

MPICH2_INCLUDE_LOCATION = /usr/local/include
MPICH2_LIBRARY_LOCATION = /usr/local/lib
MPICH2_LIBRARIES =
#MPICH2_LIBRARIES = -lmpi_f90 \
                   -lmpi_f77       \
                   -lmpi       \
                   -lopen-rte       \
                   -lopen-pal       \
                   -ldl        \
                   -lnsl      \
                   -lutil      \
                   -lm      \
                   -pthread

MPICH2_TEST_RUN_CMD = mpiexec -n 2
MPICH2_TEST_RUN_CMD_LOCAL = mpiexec -n 2
MPICH2_VALGRIND_TEST_RUN_CMD_LOCAL = mpiexec -n 2 valgrind --tool=memcheck --leak-check=yes --track-origins=yes 
#MPICH2_TEST_RUN_CMD = mpiexec -n 2

# OpenMPI definitions

OPENMPI_INCLUDE_LOCATION = /usr/lib64/mpi/gcc/openmpi/include
OPENMPI_LIBRARY_LOCATION = /usr/lib64/mpi/gcc/openmpi/lib64
OPENMPI_LIBRARIES = -lmpi_f90 \
                   -lmpi_f77       \
                   -lmpi       \
                   -lopen-rte       \
                   -lopen-pal       \
                   -ldl        \
                   -lnsl      \
                   -lutil      \
                   -lm      \
                   -pthread

OPENMPI_TEST_RUN_CMD = mpirun -np 2

DEFAULT_MPI_INCLUDE_LOCATION = ../
DEFAULT_MPI_LIBRARY_LOCATION = ../
DEFAULT_MPI_LIBRARIES =
DEFAULT_MPI_TEST_RUN_CMD = mpiexec -np 2
DEFAULT_MPI_TEST_RUN_CMD_LOCAL = mpiexec -np 2

MPI_INCLUDE_LOCATION = $(DEFAULT_MPI_INCLUDE_LOCATION)
MPI_LIBRARY_LOCATION = $(DEFAULT_MPI_LIBRARY_LOCATION)
MPI_LIBRARIES = $(DEFAULT_MPI_LIBRARIES)
TEST_RUN_CMD = $(DEFAULT_MPI_TEST_RUN_CMD)
TEST_RUN_CMD_LOCAL = $(DEFAULT_MPI_TEST_RUN_CMD_LOCAL)
VALGRIND_TEST_RUN_CMD_LOCAL = $(MPICH2_VALGRIND_TEST_RUN_CMD_LOCAL)

# define commands

MV_LIB = mv libDataVolume5DClass.a $(LIB_FILES)
MV_LIB_MOD = mv *.mod $(MOD_FILES)
MV_OBJ = mv *.o $(OBJ_FILES)
MV_MOD = mv *.mod $(MOD_FILES)
MV_EXE = mv $(CMD) $(EXE_FILES)

TEST_SUCCESS = DataVolume5DClassTest.PASSED
VALGRIND_TEST_SUCCESS = DataVolume5DClassValgrindTest.PASSED

DATA_VOLUME_CLASS_OBJS = $(OBJ_FILES)/DataVolume5DClass.o \
                         $(OBJ_FILES)/DataVolume5DClassInt4.o \
                         $(OBJ_FILES)/DataVolume5DClassInt8.o \
                         $(OBJ_FILES)/DataVolume5DClassRealSP.o \
                         $(OBJ_FILES)/DataVolume5DClassRealDP.o \
                         $(OBJ_FILES)/DataVolume5DClassLogical.o

OBJS = \
 $(DATA_VOLUME_CLASS_OBJS)

TEST_OBJS = \
 $(OBJ_FILES)/Check0DData.o \
 $(OBJ_FILES)/Check1DData.o \
 $(OBJ_FILES)/Check2DData.o \
 $(OBJ_FILES)/Check3DData.o \
 $(OBJ_FILES)/Check4DData.o \
 $(OBJ_FILES)/Check5DData.o \
 $(OBJ_FILES)/DataVolume5DClassCreateAndDestroyTest.o      \
 $(OBJ_FILES)/DataVolume5DClassRestartFileIOTest.o                 \
 $(OBJ_FILES)/DataVolume5DClassTest.o \
 $(OBJS)

#TEST_OBJS = \
# $(OBJ_FILES)/Check0DData.o \
# $(OBJ_FILES)/Check1DData.o \
# $(OBJ_FILES)/Check2DData.o \
# $(OBJ_FILES)/Check3DData.o \
# $(OBJ_FILES)/Check5DData.o \
# $(OBJ_FILES)/DataVolume5DClassCreateAndDestroyTest.o      \
# $(OBJ_FILES)/DataVolume5DClassFunctionTest.o              \
# $(OBJ_FILES)/DataVolume5DClassMessagePassingTest.o        \
# $(OBJ_FILES)/DataVolume5DClassRestartFileIOTest.o                 \
# $(OBJ_FILES)/DataVolume5DClassTest.o \
# $(OBJS)

PREVIOUS_TESTS = \
 MessagePassingInterfaceTest.PASSED \
 ErrorInformationClassTest.PASSED  \
 MPI2DataClassNoErrClassTest.PASSED \
 QuickSortClassTest.PASSED 

PREVIOUS_VALGRIND_TESTS = \
 MessagePassingInterfaceValgrindTest.PASSED \
 ErrorInformationClassValgrindTest.PASSED  \
 MPI2DataClassNoErrClassValgrindTest.PASSED \
 QuickSortClassValgrindTest.PASSED 

LIBS =                     \
 -lDataVolume5DClass         \
 -lMPI2DataClassNoErrClass \
 -lMessagePassingInterface \
 -lQuickSortClass          \
 -lErrorInformationClass

MODS = \
 -I$(MOD_FILES)

MESSAGE_PASSING_INTERFACE_LIB_OBJS = \
   $(INTERFACE_WITH_MPI_LOCATION)/ObjectFiles/*.o

ERROR_INFORMATION_CLASS_LIB_OBJS = \
   $(ERROR_INFORMATION_CLASS_LOCATION)/ObjectFiles/*.o

MPI2_DATA_CLASS_LIB_OBJS = \
   $(MPI2_DATA_CLASS_LOCATION)/ObjectFiles/*.o

QUICK_SORT_CLASS_LIB_OBJS = \
   $(QUICK_SORT_CLASS_LOCATION)/ObjectFiles/*.o

# Linkage rule

checkLibs:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cd $(QUICK_SORT_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"
	make all

all: $(LIB_FILES)/libDataVolume5DClass.a
	@echo " "
	@echo "-------------------------------------------------- "
	@echo "-   Hixon Technologies DataVolume definition     - "
	@echo "-    Copyright 2021 by Hixon Technologies, LLC   - "
	@echo "-    Used with permission.                       - "
	@echo "-    These routines come with no warranty.       - "
	@echo "-                                                - "
	@echo "-   Contact:  Ray Hixon, Hixon Technologies      - "
	@echo "-             fshixon@yahoo.com                  - "
	@echo "-------------------------------------------------- "
	@echo " "

test: $(TEST_SUCCESS)

$(TEST_SUCCESS):  \
      $(PREVIOUS_TESTS) \
      $(LIB_FILES)/libDataVolume5DClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) 
	$(MV_EXE)
	cd $(EXE_FILES); $(TEST_RUN_CMD) ./$(CMD)
	cp $(EXE_FILES)/$(TEST_SUCCESS) .

$(VALGRIND_TEST_SUCCESS):  \
      $(PREVIOUS_VALGRIND_TESTS) \
      $(LIB_FILES)/libDataVolume5DClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES)
	$(MV_EXE)
	cd $(EXE_FILES); $(VALGRIND_TEST_RUN_CMD_LOCAL) ./$(CMD) &> outValDataVolumeClassTest.txt
	cp $(EXE_FILES)/$(TEST_SUCCESS) ./$(VALGRIND_TEST_SUCCESS)

valgrindTest: $(VALGRIND_TEST_SUCCESS)

clean:
	rm -f *.a 
	rm -f *.o 
	rm -f *.d 
	rm -f *.mod 
	rm -f ifc*
	rm -f *.pc
	rm -f *.pcl
	rm -f $(LIB_FILES)/*.a
	rm -f $(MOD_FILES)/*.mod
	rm -f $(OBJ_FILES)/*.o
	rm -f $(OBJ_FILES)/*.d

cleanAll:
	rm -f *.a 
	rm -f *.o 
	rm -f *.d 
	rm -f *.mod 
	rm -f ifc*
	rm -f *.pc
	rm -f *.pcl
	rm -f $(LIB_FILES)/*.a
	rm -f $(MOD_FILES)/*.mod
	rm -f $(OBJ_FILES)/*.o
	rm -f $(OBJ_FILES)/*.d
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make clean
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; make clean
	cd $(QUICK_SORT_CLASS_LOCATION)/Makefile; make clean
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make clean

cleanTest:
	rm -f *.FAILED
	rm -f ../CodeRun/*.FAILED
	rm -f *.PASSED
	rm -f ../CodeRun/*.PASSED
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make cleanTest
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; make cleanTest
	cd $(QUICK_SORT_CLASS_LOCATION)/Makefile; make cleanTest
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make cleanTest

# Dependency definitions

#------------------MessagePassingInterface lib----------------------------

$(LIB_FILES)/libMessagePassingInterface.a: \
  $(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a
	cp -p $(INTERFACE_WITH_MPI_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp -p $(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"

MessagePassingInterfaceTest.PASSED: \
  $(LIB_FILES)/libMessagePassingInterface.a \
  $(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceTest.PASSED
	cp $(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceTest.PASSED .

MessagePassingInterfaceValgrindTest.PASSED: \
  $(LIB_FILES)/libMessagePassingInterface.a \
  $(INTERFACE_WITH_MPI_LOCATION)/Makefile/MessagePassingInterfaceValgrindTest.PASSED
	cp $(INTERFACE_WITH_MPI_LOCATION)/Makefile/MessagePassingInterfaceValgrindTest.PASSED .

$(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceTest.PASSED:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; \
	make test  \
	"F95=$(F95)"  \
	"F95FLAGS=$(F95FLAGS)"  \
	"F95FLAGS2=$(F95FLAGS2)"  \
	"LDFLAGS = $(LDFLAGS)"  \
	"MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
	"MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
	"MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
	"TEST_RUN_CMD = $(TEST_RUN_CMD)"

$(INTERFACE_WITH_MPI_LOCATION)/Makefile/MessagePassingInterfaceValgrindTest.PASSED:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; \
	make valgrindTest  \
	"F95=$(F95)"  \
	"F95FLAGS=$(F95FLAGS)"  \
	"F95FLAGS2=$(F95FLAGS2)"  \
	"LDFLAGS = $(LDFLAGS)"  \
	"MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
	"MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
	"MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
	"TEST_RUN_CMD = $(VALGRIND_TEST_RUN_CMD_LOCAL)"

#------------------ErrorInformationClass lib---------------------------

$(LIB_FILES)/libErrorInformationClass.a: \
  $(ERROR_INFORMATION_CLASS_LOCATION)/LibraryFiles/libErrorInformationClass.a
	cp -p $(ERROR_INFORMATION_CLASS_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp -p $(ERROR_INFORMATION_CLASS_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(ERROR_INFORMATION_CLASS_LOCATION)/LibraryFiles/libErrorInformationClass.a:
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"

ErrorInformationClassTest.PASSED: \
  $(LIB_FILES)/libErrorInformationClass.a \
  $(ERROR_INFORMATION_CLASS_LOCATION)/Executables/ErrorInformationClassTest.PASSED
	cp $(ERROR_INFORMATION_CLASS_LOCATION)/Executables/ErrorInformationClassTest.PASSED .

ErrorInformationClassValgrindTest.PASSED: \
  $(LIB_FILES)/libErrorInformationClass.a \
  $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile/ErrorInformationClassValgrindTest.PASSED
	cp $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile/ErrorInformationClassValgrindTest.PASSED .

$(ERROR_INFORMATION_CLASS_LOCATION)/Executables/ErrorInformationClassTest.PASSED:
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; \
	make test  \
	"F95=$(F95)"  \
	"F95FLAGS=$(F95FLAGS)"  \
	"F95FLAGS2=$(F95FLAGS2)"  \
	"LDFLAGS = $(LDFLAGS)"  \
	"MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
	"MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
	"MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
	"TEST_RUN_CMD = $(TEST_RUN_CMD)"

$(ERROR_INFORMATION_CLASS_LOCATION)/Makefile/ErrorInformationClassValgrindTest.PASSED:
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; \
	make valgrindTest  \
	"F95=$(F95)"  \
	"F95FLAGS=$(F95FLAGS)"  \
	"F95FLAGS2=$(F95FLAGS2)"  \
	"LDFLAGS = $(LDFLAGS)"  \
	"MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
	"MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
	"MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
	"TEST_RUN_CMD = $(VALGRIND_TEST_RUN_CMD_LOCAL)"

#------------------QuickSortClass lib----------------------------

$(LIB_FILES)/libQuickSortClass.a: \
  $(QUICK_SORT_CLASS_LOCATION)/LibraryFiles/libQuickSortClass.a
	cp -p $(QUICK_SORT_CLASS_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp -p $(QUICK_SORT_CLASS_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(QUICK_SORT_CLASS_LOCATION)/LibraryFiles/libQuickSortClass.a:
	cd $(QUICK_SORT_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"

QuickSortClassTest.PASSED: \
  $(LIB_FILES)/libQuickSortClass.a \
  $(QUICK_SORT_CLASS_LOCATION)/Executables/QuickSortClassTest.PASSED
	cp $(QUICK_SORT_CLASS_LOCATION)/Executables/QuickSortClassTest.PASSED .

QuickSortClassValgrindTest.PASSED: \
  $(LIB_FILES)/libQuickSortClass.a \
  $(QUICK_SORT_CLASS_LOCATION)/Makefile/QuickSortClassValgrindTest.PASSED
	cp $(QUICK_SORT_CLASS_LOCATION)/Makefile/QuickSortClassValgrindTest.PASSED .

$(QUICK_SORT_CLASS_LOCATION)/Executables/QuickSortClassTest.PASSED:
	cd $(QUICK_SORT_CLASS_LOCATION)/Makefile; \
	make test  \
	"F95=$(F95)"  \
	"F95FLAGS=$(F95FLAGS)"  \
	"F95FLAGS2=$(F95FLAGS2)"  \
	"LDFLAGS = $(LDFLAGS)"  \
	"MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
	"MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
	"MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
	"TEST_RUN_CMD = $(TEST_RUN_CMD)"

$(QUICK_SORT_CLASS_LOCATION)/Makefile/QuickSortClassValgrindTest.PASSED:
	cd $(QUICK_SORT_CLASS_LOCATION)/Makefile; \
	make valgrindTest  \
	"F95=$(F95)"  \
	"F95FLAGS=$(F95FLAGS)"  \
	"F95FLAGS2=$(F95FLAGS2)"  \
	"LDFLAGS = $(LDFLAGS)"  \
	"MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
	"MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
	"MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
	"TEST_RUN_CMD = $(VALGRIND_TEST_RUN_CMD_LOCAL)"

#------------------MPI2DataClassNoErrClass lib----------------------------

$(LIB_FILES)/libMPI2DataClassNoErrClass.a: \
  $(MPI2_DATA_CLASS_LOCATION)/LibraryFiles/libMPI2DataClassNoErrClass.a
	cp -p $(MPI2_DATA_CLASS_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp -p $(MPI2_DATA_CLASS_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(MPI2_DATA_CLASS_LOCATION)/LibraryFiles/libMPI2DataClassNoErrClass.a:
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"

MPI2DataClassNoErrClassTest.PASSED: \
  $(LIB_FILES)/libMPI2DataClassNoErrClass.a \
  $(MPI2_DATA_CLASS_LOCATION)/Executables/MPI2DataClassNoErrClassTest.PASSED
	cp $(MPI2_DATA_CLASS_LOCATION)/Executables/MPI2DataClassNoErrClassTest.PASSED .

MPI2DataClassNoErrClassValgrindTest.PASSED: \
  $(LIB_FILES)/libMPI2DataClassNoErrClass.a \
  $(MPI2_DATA_CLASS_LOCATION)/Makefile/MPI2DataClassNoErrClassValgrindTest.PASSED
	cp $(MPI2_DATA_CLASS_LOCATION)/Makefile/MPI2DataClassNoErrClassValgrindTest.PASSED .

$(MPI2_DATA_CLASS_LOCATION)/Executables/MPI2DataClassNoErrClassTest.PASSED:
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; \
	make test  \
	"F95=$(F95)"  \
	"F95FLAGS=$(F95FLAGS)"  \
	"F95FLAGS2=$(F95FLAGS2)"  \
	"LDFLAGS = $(LDFLAGS)"  \
	"MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
	"MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
	"MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
	"TEST_RUN_CMD = $(TEST_RUN_CMD)"

$(MPI2_DATA_CLASS_LOCATION)/Makefile/MPI2DataClassNoErrClassValgrindTest.PASSED:
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; \
	make valgrindTest  \
	"F95=$(F95)"  \
	"F95FLAGS=$(F95FLAGS)"  \
	"F95FLAGS2=$(F95FLAGS2)"  \
	"LDFLAGS = $(LDFLAGS)"  \
	"MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
	"MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
	"MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
	"TEST_RUN_CMD = $(VALGRIND_TEST_RUN_CMD_LOCAL)"

#------------------DataVolume lib----------------------------

$(LIB_FILES)/libDataVolume5DClass.a: \
	$(LIBS_NEEDED) \
        $(OBJS) 
	ar r libDataVolume5DClass.a $(OBJS) $(MESSAGE_PASSING_INTERFACE_LIB_OBJS) $(ERROR_INFORMATION_CLASS_LIB_OBJS)
	$(MV_LIB)

$(OBJ_FILES)/DataVolume5DClass.o:           \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClass.f90   \
    $(SRC_FILES)/DataVolume5DClassTotInt4Routines.f90 \
    $(SRC_FILES)/DataVolume5DClassTotInt8Routines.f90 \
    $(SRC_FILES)/DataVolume5DClassTotRealSPRoutines.f90 \
    $(SRC_FILES)/DataVolume5DClassTotRealDPRoutines.f90 \
    $(SRC_FILES)/DataVolume5DClassTotLogicalRoutines.f90 \
    $(OBJ_FILES)/DataVolume5DClassInt4.o   \
    $(OBJ_FILES)/DataVolume5DClassInt8.o   \
    $(OBJ_FILES)/DataVolume5DClassRealSP.o   \
    $(OBJ_FILES)/DataVolume5DClassRealDP.o   \
    $(OBJ_FILES)/DataVolume5DClassLogical.o
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/DataVolume5DClass.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/DataVolume5DClassRealSP.o:           \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassRealSP.f90      \
    $(SRC_FILES)/DataVolume5DClassRealSPRoutines.f90 
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/DataVolume5DClassRealSP.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/DataVolume5DClassRealDP.o:           \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassRealDP.f90      \
    $(SRC_FILES)/DataVolume5DClassRealDPRoutines.f90 
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/DataVolume5DClassRealDP.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/DataVolume5DClassInt4.o:           \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassInt4.f90       \
    $(SRC_FILES)/DataVolume5DClassInt4Routines.f90 
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/DataVolume5DClassInt4.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/DataVolume5DClassInt8.o:           \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassInt8.f90       \
    $(SRC_FILES)/DataVolume5DClassInt8Routines.f90 
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/DataVolume5DClassInt8.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/DataVolume5DClassLogical.o:           \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassLogical.f90       \
    $(SRC_FILES)/DataVolume5DClassLogicalRoutines.f90 
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/DataVolume5DClassLogical.f90
	$(MV_OBJ); $(MV_MOD)

$(SRC_FILES)/DataVolume5DClassTotRealSPRoutines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassTotRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateTotSedScript.txt
	cd $(SRC_FILES); ./TemplateTotSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassTotRealDPRoutines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassTotRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateTotSedScript.txt
	cd $(SRC_FILES); ./TemplateTotSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassTotInt4Routines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassTotRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateTotSedScript.txt
	cd $(SRC_FILES); ./TemplateTotSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassTotInt8Routines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassTotRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateTotSedScript.txt
	cd $(SRC_FILES); ./TemplateTotSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassTotLogicalRoutines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassTotRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateTotSedScript.txt
	cd $(SRC_FILES); ./TemplateTotSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassRealSPRoutines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateSedScript.txt
	cd $(SRC_FILES); ./TemplateSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassRealDPRoutines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateSedScript.txt
	cd $(SRC_FILES); ./TemplateSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassInt4Routines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateSedScript.txt
	cd $(SRC_FILES); ./TemplateSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassInt8Routines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateSedScript.txt
	cd $(SRC_FILES); ./TemplateSedScript.txt; cd ../Makefile

$(SRC_FILES)/DataVolume5DClassLogicalRoutines.f90: \
	$(LIBS_NEEDED) \
    $(SRC_FILES)/DataVolume5DClassRoutinesTemplate.f90 \
    $(SRC_FILES)/TemplateSedScript.txt
	cd $(SRC_FILES); ./TemplateSedScript.txt; cd ../Makefile

#-------------testing routines----------------------------------------

$(OBJ_FILES)/Check0DData.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/Check0DData.f90
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check0DData.f90
	$(MV_OBJ)

$(OBJ_FILES)/Check1DData.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/Check1DData.f90
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check1DData.f90
	$(MV_OBJ)

$(OBJ_FILES)/Check2DData.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/Check2DData.f90
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check2DData.f90
	$(MV_OBJ)

$(OBJ_FILES)/Check3DData.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/Check3DData.f90
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check3DData.f90
	$(MV_OBJ)

$(OBJ_FILES)/Check4DData.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/Check4DData.f90
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check4DData.f90
	$(MV_OBJ)

$(OBJ_FILES)/Check5DData.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/Check5DData.f90
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check5DData.f90
	$(MV_OBJ)

$(OBJ_FILES)/DataVolume5DClassCreateAndDestroyTest.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/DataVolume5DClassCreateAndDestroyTest.f90     \
    $(SRC_FILES)/DataVolume5DClassCreateAndDestroyTest5DTemplate.f90 \
    $(SRC_FILES)/TemplateTestSedScript.txt \
    $(OBJ_FILES)/Check5DData.o	      \
    $(OBJ_FILES)/DataVolume5DClass.o
	cd $(SRC_FILES); ./TemplateTestSedScript.txt; cd ../Makefile
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/DataVolume5DClassCreateAndDestroyTest.f90
	$(MV_OBJ)

$(OBJ_FILES)/DataVolume5DClassFunctionTest.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/DataVolume5DClassFunctionTest.f90     \
    $(OBJ_FILES)/Check0DData.o	      \
    $(OBJ_FILES)/Check1DData.o	      \
    $(OBJ_FILES)/Check2DData.o	      \
    $(OBJ_FILES)/Check3DData.o	      \
    $(OBJ_FILES)/Check5DData.o	      \
    $(OBJ_FILES)/DataVolume5DClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/DataVolume5DClassFunctionTest.f90
	$(MV_OBJ)

$(OBJ_FILES)/DataVolume5DClassMessagePassingTest.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/DataVolume5DClassMessagePassingTest.f90     \
    $(OBJ_FILES)/Check0DData.o	      \
    $(OBJ_FILES)/Check1DData.o	      \
    $(OBJ_FILES)/Check2DData.o	      \
    $(OBJ_FILES)/Check3DData.o	      \
    $(OBJ_FILES)/Check5DData.o	      \
    $(OBJ_FILES)/DataVolume5DClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/DataVolume5DClassMessagePassingTest.f90
	$(MV_OBJ)

$(OBJ_FILES)/DataVolume5DClassMPIIOTest.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/DataVolume5DClassMPIIOTest.f90     \
    $(OBJ_FILES)/Check0DData.o	      \
    $(OBJ_FILES)/Check1DData.o	      \
    $(OBJ_FILES)/Check2DData.o	      \
    $(OBJ_FILES)/Check3DData.o	      \
    $(OBJ_FILES)/Check5DData.o	      \
    $(OBJ_FILES)/DataVolume5DClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/DataVolume5DClassMPIIOTest.f90
	$(MV_OBJ)

$(OBJ_FILES)/DataVolume5DClassRestartFileIOTest.o:	  \
    $(LIBS_NEEDED)	           \
    $(SRC_FILES)/DataVolume5DClassRestartFileIOTest.f90     \
    $(OBJ_FILES)/Check0DData.o	      \
    $(OBJ_FILES)/Check1DData.o	      \
    $(OBJ_FILES)/Check2DData.o	      \
    $(OBJ_FILES)/Check3DData.o	      \
    $(OBJ_FILES)/Check5DData.o	      \
    $(OBJ_FILES)/DataVolume5DClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/DataVolume5DClassRestartFileIOTest.f90
	$(MV_OBJ)

$(OBJ_FILES)/DataVolume5DClassTest.o:	             \
    $(LIBS_NEEDED)	           \
    $(LIB_FILES)/libDataVolume5DClass.a                 \
    $(SRC_FILES)/DataVolume5DClassTest.f90	        \
    $(OBJ_FILES)/DataVolume5DClassCreateAndDestroyTest.o  \
    $(OBJ_FILES)/Check0DData.o	               \
    $(OBJ_FILES)/Check1DData.o	               \
    $(OBJ_FILES)/Check2DData.o	               \
    $(OBJ_FILES)/Check3DData.o	               \
    $(OBJ_FILES)/Check5DData.o	               \
    $(OBJ_FILES)/DataVolume5DClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/DataVolume5DClassTest.f90
	$(MV_OBJ)

#$(OBJ_FILES)/DataVolume5DClassTest.o:	             \
#    $(LIBS_NEEDED)	           \
#    $(SRC_FILES)/DataVolume5DClassTest.f90	        \
#    $(OBJ_FILES)/DataVolume5DClassCreateAndDestroyTest.o  \
#    $(OBJ_FILES)/DataVolume5DClassFunctionTest.o	  \
#    $(OBJ_FILES)/DataVolume5DClassMessagePassingTest.o    \
#    $(OBJ_FILES)/DataVolume5DClassMPIIOTest.o	     \
#    $(OBJ_FILES)/DataVolume5DClassRestartFileIOTest.o     \
#    $(OBJ_FILES)/Check0DData.o	               \
#    $(OBJ_FILES)/Check1DData.o	               \
#    $(OBJ_FILES)/Check2DData.o	               \
#    $(OBJ_FILES)/Check3DData.o	               \
#    $(OBJ_FILES)/Check5DData.o	               \
#    $(OBJ_FILES)/DataVolume5DClass.o
#	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/DataVolume5DClassTest.f90
#	$(MV_OBJ)

