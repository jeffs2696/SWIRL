### MAKEFILE DEFINITIONS ###

F95_IFC      = ifort
F95FLAGS_IFC = -c  -O0 -CU -CB
#F95FLAGS_IFC = -c  -O3 -pad -align -prec_div
LDFLAGS_IFC  = -static

F95_G95      = mpif90
#F95FLAGS_G95 = -c -O3
F95FLAGS_G95 = -c -O0 -Wall -Werror -Wline-truncation -Wmissing-intent -Wuninitialized -Wprecision-loss -fbounds-check
F95FLAGS2_G95 = -c -O0 -Wall -Werror -Wline-truncation -Wmissing-intent -Wuninitialized -Wprecision-loss -fbounds-check
LDFLAGS_G95  =

F95_GFORTRAN      = mpif90
#F95FLAGS_GFORTRAN = -c -O3
F95FLAGS_GFORTRAN = -O -c -std=f2008 -Werror -Wall -Wline-truncation -Wuninitialized -fbounds-check -Warray-temporaries -Wconversion -Wconversion-extra -Wintrinsics-std -fcheck=all
F95FLAGS2_GFORTRAN = -O -c -std=f2008 -Werror -Wall -Wline-truncation -Wuninitialized -fbounds-check -Warray-temporaries -Wconversion -Wconversion-extra -Wintrinsics-std -fcheck=all
LDFLAGS_GFORTRAN  =

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

CMD = MPI2DataClassNoErrClassTest.exe

# define global paths

#INTERFACE_WITH_MPI_LOCATION = ../../../InterfaceWithMPI
INTERFACE_WITH_MPI_LOCATION = ../../InterfaceWithMPI
ERROR_INFORMATION_CLASS_LOCATION = ../../ErrorInformationClass

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
MPICH2_VALGRIND_TEST_RUN_CMD_LOCAL = mpiexec -n 2 valgrind --tool=memcheck --leak-check=yes --track-origins=yes

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

# LAM MPI definitions

LAMMPI_INCLUDE_LOCATION = /usr/local/include
LAMMPI_LIBRARY_LOCATION = /usr/local/library
LAMMPI_LIBRARIES = -llamf77mpi \
                   -lmpi       \
                   -llam       \
                   -lutil      \
                   -ldl        \
                   -pthread

LAM_TEST_RUN_CMD = mpirun -np 2 -s n0

SICORMPI_INCLUDE_LOCATION = /usr/include/gentoo-multilib/n64
SICORMPI_LIBRARY_LOCATION = /usr/lib
SICORMPI_LIBRARIES = -lscmpi_debug

SICOR_TEST_RUN_CMD = srun -p sca -n 2

MPI_INCLUDE_LOCATION = $(MPICH2_INCLUDE_LOCATION)
MPI_LIBRARY_LOCATION = $(MPICH2_LIBRARY_LOCATION)
MPI_LIBRARIES = $(MPICH2_LIBRARIES)
TEST_RUN_CMD = $(MPICH2_TEST_RUN_CMD)
VALGRIND_TEST_RUN_CMD_LOCAL = $(MPICH2_VALGRIND_TEST_RUN_CMD_LOCAL)

# define commands

MV_LIB = mv libMPI2DataClassNoErrClass.a $(LIB_FILES)
MV_LIB_MOD = mv *.mod $(MOD_FILES)
MV_OBJ = mv *.o $(OBJ_FILES)
MV_MOD = mv *.mod $(MOD_FILES)
MV_EXE = mv $(CMD) $(EXE_FILES)

TEST_SUCCESS = MPI2DataClassNoErrClassTest.PASSED
VALGRIND_TEST_SUCCESS = MPI2DataClassNoErrClassValgrindTest.PASSED

MPI2_DATA_CLASS_NEC_LIBS =            \
    $(LIB_FILES)/MPI2DataClassNoErrClass.a  \

MPI2_DATA_CLASS_NEC_OBJS =            \
    $(OBJ_FILES)/MPI2DataClassNoErrClass.o \
    $(OBJ_FILES)/MPIStructLL.o \
    $(OBJ_FILES)/MPI2DataClassNEC.o

LIBS_NEEDED = \
 $(LIB_FILES)/libMessagePassingInterface.a \
 $(LIB_FILES)/libErrorInformationClass.a

OBJS = \
 $(MPI2_DATA_CLASS_NEC_OBJS)

TEST_OBJS = \
 $(OBJ_FILES)/Check0DData.o \
 $(OBJ_FILES)/Check1DData.o \
 $(OBJ_FILES)/Check2DData.o \
 $(OBJ_FILES)/Check3DData.o \
 $(OBJ_FILES)/Check4DData.o \
 $(OBJ_FILES)/Check5DData.o \
 $(OBJ_FILES)/Test1DArrayPassing.o \
 $(OBJ_FILES)/TMatModule.o \
 $(OBJ_FILES)/Test2DArrayPassing.o \
 $(OBJ_FILES)/Test3DArrayPassing.o \
 $(OBJ_FILES)/Test4DArrayPassing.o \
 $(OBJ_FILES)/Test5DArrayPassing.o \
 $(OBJ_FILES)/MPI2DataClassNoErrClassTest.o

PREVIOUS_TESTS = \
 MessagePassingInterfaceTest.PASSED \
 ErrorInformationClassTest.PASSED 

PREVIOUS_VALGRIND_TESTS = \
 MessagePassingInterfaceValgrindTest.PASSED \
 ErrorInformationClassValgrindTest.PASSED 

LIBS = \
 -lMPI2DataClassNoErrClass \
 -lMessagePassingInterface \
 -lErrorInformationClass

MODS = \
 -I$(MOD_FILES)

MESSAGE_PASSING_INTERFACE_LIB_OBJS = \
   $(INTERFACE_WITH_MPI_LOCATION)/ObjectFiles/*.o

ERROR_INFORMATION_CLASS_LIB_OBJS = \
   $(ERROR_INFORMATION_CLASS_LOCATION)/ObjectFiles/*.o

# Linkage rule

checkLibs:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	make all


all: $(LIB_FILES)/libMPI2DataClassNoErrClass.a
	@echo " "
	@echo "-------------------------------------------------- "
	@echo "-   Hixon Technologies MPI2DataClass routines     - "
	@echo "-    Copyright 2021 by Hixon Technologies, LLC   - "
	@echo "-    Used with permission.                       - "
	@echo "-    These routines come with no warranty.       - "
	@echo "-                                                - "
	@echo "-   Contact:  Ray Hixon, Hixon Technologies      - "
	@echo "-             rhixon@wideopenwest.com            - "
	@echo "-------------------------------------------------- "
	@echo " "

test: $(TEST_SUCCESS)

$(TEST_SUCCESS):  \
      $(PREVIOUS_TESTS) \
      $(LIB_FILES)/libMPI2DataClassNoErrClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES)
	$(MV_EXE)
	cd $(EXE_FILES); $(TEST_RUN_CMD) ./$(CMD)
	cp $(EXE_FILES)/$(TEST_SUCCESS) .

$(VALGRIND_TEST_SUCCESS):  \
      $(PREVIOUS_VALGRIND_TESTS) \
      $(LIB_FILES)/libMPI2DataClassNoErrClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES)
	$(MV_EXE)
	cd $(EXE_FILES); $(VALGRIND_TEST_RUN_CMD_LOCAL) ./$(CMD) &> outValMPI2DataClassNoErrClassTest.txt
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
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make clean
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; make clean

cleanTest:
	rm -f *.FAILED
	rm -f ../Executables/*.FAILED
	rm -f *.PASSED
	rm -f ../Executables/*.PASSED
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make cleanTest
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; make cleanTest

# Dependency definitions

#------------------MessagePassingInterface lib----------------------------

$(LIB_FILES)/libMessagePassingInterface.a: \
  $(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a
	cp -p $(INTERFACE_WITH_MPI_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp -p $(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"

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

#------------------ErrorInformationClass lib----------------------------

$(LIB_FILES)/libErrorInformationClass.a: \
  $(ERROR_INFORMATION_CLASS_LOCATION)/LibraryFiles/libErrorInformationClass.a
	cp -p $(ERROR_INFORMATION_CLASS_LOCATION)/ModuleFiles/*.mod $(MOD_FILES); \
	cp -p $(ERROR_INFORMATION_CLASS_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(ERROR_INFORMATION_CLASS_LOCATION)/LibraryFiles/libErrorInformationClass.a:
	cd $(ERROR_INFORMATION_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"

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

#--------------------MPI2DataClass lib---------------------------------

$(LIB_FILES)/libMPI2DataClassNoErrClass.a: \
        $(OBJS) \
        $(LIBS_NEEDED)
	ar r libMPI2DataClassNoErrClass.a $(OBJS) $(MESSAGE_PASSING_INTERFACE_LIB_OBJS)
	$(MV_LIB)

$(OBJ_FILES)/MPI2DataClassNoErrClass.o:           \
    $(SRC_FILES)/MPI2DataClassNoErrClass.f90      \
    $(OBJ_FILES)/MPI2DataClassNEC.o      \
    $(LIBS_NEEDED)
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/MPI2DataClassNoErrClass.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPIStructLL.o:              \
    $(SRC_FILES)/MPIStructLL.f90         \
    $(SRC_FILES)/AddLL1D.f90             \
    $(SRC_FILES)/AddLL2D.f90             \
    $(SRC_FILES)/AddLL3D.f90             \
    $(SRC_FILES)/AddLL4D.f90             \
    $(SRC_FILES)/AddLLEntryInt4.f90       \
    $(SRC_FILES)/AddLLEntryInt8.f90       \
    $(SRC_FILES)/AddLLEntryLogical.f90   \
    $(SRC_FILES)/AddLLEntryReal4.f90      \
    $(SRC_FILES)/AddLLEntryReal8.f90      \
    $(SRC_FILES)/AddLLEntryCharacter.f90 \
    $(LIBS_NEEDED)
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/MPIStructLL.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI2DataClassNEC.o:           \
    $(SRC_FILES)/MPI2DataClassNEC.f90      \
    $(SRC_FILES)/AddCharacterDataToStruct.f90       \
    $(SRC_FILES)/AddInt4DataToStruct.f90       \
    $(SRC_FILES)/AddInt8DataToStruct.f90       \
    $(SRC_FILES)/AddLogicalDataToStruct.f90       \
    $(SRC_FILES)/AddReal4DataToStruct.f90       \
    $(SRC_FILES)/AddReal8DataToStruct.f90       \
    $(OBJ_FILES)/MPIStructLL.o      \
    $(LIBS_NEEDED)
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/MPI2DataClassNEC.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check0DData.o:           \
    $(SRC_FILES)/Check0DData.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Check0DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check1DData.o:           \
    $(SRC_FILES)/Check1DData.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Check1DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check2DData.o:           \
    $(SRC_FILES)/Check2DData.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Check2DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check3DData.o:           \
    $(SRC_FILES)/Check3DData.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Check3DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check4DData.o:           \
    $(SRC_FILES)/Check4DData.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Check4DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check5DData.o:           \
    $(SRC_FILES)/Check5DData.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Check5DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Test1DArrayPassing.o:           \
    $(SRC_FILES)/Test1DArrayPassing.f90      \
    $(OBJ_FILES)/Check1DData.o
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Test1DArrayPassing.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/TMatModule.o:           \
    $(SRC_FILES)/TMatModule.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/TMatModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Test2DArrayPassing.o:           \
    $(SRC_FILES)/Test2DArrayPassing.f90      \
    $(OBJ_FILES)/TMatModule.o                \
    $(OBJ_FILES)/Check2DData.o
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Test2DArrayPassing.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Test3DArrayPassing.o:           \
    $(SRC_FILES)/Test3DArrayPassing.f90      \
    $(OBJ_FILES)/TMatModule.o                \
    $(OBJ_FILES)/Check3DData.o
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Test3DArrayPassing.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Test4DArrayPassing.o:           \
    $(SRC_FILES)/Test4DArrayPassing.f90      \
    $(OBJ_FILES)/TMatModule.o                \
    $(OBJ_FILES)/Check4DData.o
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Test4DArrayPassing.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Test5DArrayPassing.o:           \
    $(SRC_FILES)/Test5DArrayPassing.f90      \
    $(OBJ_FILES)/TMatModule.o                \
    $(OBJ_FILES)/Check5DData.o
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/Test5DArrayPassing.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI2DataClassNoErrClassTest.o:                \
    $(SRC_FILES)/MPI2DataClassNoErrClassTest.f90      \
    $(OBJ_FILES)/Test1DArrayPassing.o \
    $(OBJ_FILES)/Test2DArrayPassing.o \
    $(OBJ_FILES)/Test3DArrayPassing.o \
    $(OBJ_FILES)/Test4DArrayPassing.o \
    $(OBJ_FILES)/Test5DArrayPassing.o \
    $(OBJ_FILES)/Check0DData.o \
    $(OBJ_FILES)/Check1DData.o \
    $(OBJ_FILES)/Check2DData.o \
    $(OBJ_FILES)/Check3DData.o \
    $(OBJ_FILES)/Check4DData.o \
    $(OBJ_FILES)/Check5DData.o \
    $(OBJ_FILES)/MPI2DataClassNoErrClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/MPI2DataClassNoErrClassTest.f90
	$(MV_OBJ)

