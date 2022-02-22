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
F95FLAGS_GFORTRAN = -c -g -O0 -Wall -Werror -Wline-truncation -Wuninitialized -std=f2008 -fbounds-check
F95FLAGS2_GFORTRAN = -c -g -O0 -Wall -Werror -Wline-truncation -Wuninitialized -std=f2008 -fbounds-check
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

CMD = QuickSortClassTest.exe

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

LAM_TEST_RUN_CMD = mpirun -np 2

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

MV_LIB = mv libQuickSortClass.a $(LIB_FILES)
MV_LIB_MOD = mv *.mod $(MOD_FILES)
MV_OBJ = mv *.o $(OBJ_FILES)
MV_MOD = mv *.mod $(MOD_FILES)
MV_EXE = mv $(CMD) $(EXE_FILES)

TEST_SUCCESS = QuickSortClassTest.PASSED
VALGRIND_TEST_SUCCESS = QuickSortClassValgrindTest.PASSED

QUICK_SORT_CLASS_LIBS =            \
    $(LIB_FILES)/QuickSortClass.a  \

QUICK_SORT_CLASS_OBJS =            \
    $(OBJ_FILES)/QuickSortClass.o \
    $(OBJ_FILES)/QuickSortInt4.o \
    $(OBJ_FILES)/QuickSortInt8.o \
    $(OBJ_FILES)/QuickSortReal4.o \
    $(OBJ_FILES)/QuickSortReal8.o 

LIBS_NEEDED = \
 $(LIB_FILES)/libMessagePassingInterface.a \
 $(LIB_FILES)/libErrorInformationClass.a

OBJS = \
 $(QUICK_SORT_CLASS_OBJS)

TEST_OBJS = \
 $(OBJ_FILES)/QuickSortClassTest.o \
 $(OBJ_FILES)/CheckSorting.o

PREVIOUS_TESTS = \
 MessagePassingInterfaceTest.PASSED \
 ErrorInformationClassTest.PASSED 

PREVIOUS_VALGRIND_TESTS = \
 MessagePassingInterfaceValgrindTest.PASSED \
 ErrorInformationClassValgrindTest.PASSED 

LIBS = \
 -lQuickSortClass \
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


all: $(LIB_FILES)/libQuickSortClass.a
	@echo " "
	@echo "-------------------------------------------------- "
	@echo "-   Hixon Technologies QuickSortClass routines     - "
	@echo "-    Copyright 2013 by Hixon Technologies, LLC   - "
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
      $(LIB_FILES)/libQuickSortClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES)
	$(MV_EXE)
	cd $(EXE_FILES); $(TEST_RUN_CMD) ./$(CMD)
	cp $(EXE_FILES)/$(TEST_SUCCESS) .

$(VALGRIND_TEST_SUCCESS):  \
      $(PREVIOUS_VALGRIND_TESTS) \
      $(LIB_FILES)/libQuickSortClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES)
	$(MV_EXE)
	cd $(EXE_FILES); $(VALGRIND_TEST_RUN_CMD_LOCAL) ./$(CMD) &> outValQuickSortClassTest.txt
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

#--------------------QuickSortClass lib---------------------------------

$(LIB_FILES)/libQuickSortClass.a: \
        $(OBJS) \
        $(LIBS_NEEDED)
	ar r libQuickSortClass.a $(OBJS) 
	$(MV_LIB)

$(OBJ_FILES)/QuickSortClass.o:        \
    $(SRC_FILES)/QuickSortClass.f90   \
    $(OBJ_FILES)/QuickSortInt4.o      \
    $(OBJ_FILES)/QuickSortInt8.o      \
    $(OBJ_FILES)/QuickSortReal4.o     \
    $(OBJ_FILES)/QuickSortReal8.o     \
    $(LIBS_NEEDED)
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/QuickSortClass.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/QuickSortInt4.o:           \
    $(SRC_FILES)/QuickSortInt4.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/QuickSortInt4.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/QuickSortInt8.o:           \
    $(SRC_FILES)/QuickSortInt8.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/QuickSortInt8.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/QuickSortReal4.o:           \
    $(SRC_FILES)/QuickSortReal4.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/QuickSortReal4.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/QuickSortReal8.o:           \
    $(SRC_FILES)/QuickSortReal8.f90
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/QuickSortReal8.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/CheckSorting.o:          \
    $(SRC_FILES)/CheckSorting.f90     \
    $(OBJ_FILES)/QuickSortInt4.o      \
    $(OBJ_FILES)/QuickSortInt8.o      \
    $(OBJ_FILES)/QuickSortReal4.o     \
    $(OBJ_FILES)/QuickSortReal8.o     
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/CheckSorting.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/QuickSortClassTest.o:           \
    $(SRC_FILES)/QuickSortClassTest.f90      \
    $(OBJ_FILES)/CheckSorting.o              \
    $(OBJ_FILES)/QuickSortClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/QuickSortClassTest.f90
	$(MV_OBJ)

