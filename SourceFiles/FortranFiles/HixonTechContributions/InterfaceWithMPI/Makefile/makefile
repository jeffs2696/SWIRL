### MAKEFILE DEFINITIONS ###

#SHELL=/bin/csh -f

F95_IFC      = ifort
#F95FLAGS_IFC = -c  -O3 -pad -align -prec_div
F95FLAGS_IFC = -c -g -Zp8 -pc80 -fp-model strict -fno-alias -O0 -C \
               -debug extended -traceback -ftrapuv \
               -debug-parameters all -fp-stack-check -heap-arrays
F95FLAGS2_IFC = -c -g -Zp8 -pc80 -fp-model strict -fno-alias -O0 -C \
                -debug extended -traceback -ftrapuv \
                -debug-parameters all -fp-stack-check -heap-arrays
LDFLAGS_IFC  = -static

F95_G95      = mpif77
#F95_G95      = mpif90
F95FLAGS_G95 = -c -O3
F95FLAGS2_G95 = -c -O3
#F95FLAGS_G95 = -c -O0 -Wall -Werror -Wline-truncation -Wmissing-intent -Wuninitialized -Wprecision-loss -std=f95 -fbounds-check
#F95FLAGS2_G95 = -c -O0 -Wall -Werror -Wline-truncation -Wmissing-intent -Wuninitialized -Wprecision-loss -fbounds-check
LDFLAGS_G95  =

F95_GFORTRAN      = mpif90
#F95FLAGS_GFORTRAN = -c -O3
#F95FLAGS2_GFORTRAN = -c -O3
F95FLAGS_GFORTRAN = -O -c -std=f2008 -Werror -Wall -Wline-truncation -Wuninitialized -fbounds-check -Warray-temporaries -Wconversion -Wconversion-extra -Wintrinsics-std -fcheck=all
F95FLAGS2_GFORTRAN = -O -c -std=f2008 -Werror -Wall -Wline-truncation -Wuninitialized -fbounds-check -Warray-temporaries -Wconversion -Wconversion-extra -Wintrinsics-std -fcheck=all
LDFLAGS_GFORTRAN  =

F95_SICOR      = mpif90
F95FLAGS_SICOR = -c -O3 -64
LDFLAGS_SICOR  = -v -64

F95_GRCGX      = pgf90
F95FLAGS_GRCGX = -c -O3 
LDFLAGS_GRCGX  = -v 

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

MPICH2MPI_INCLUDE_LOCATION = /usr/local/include
MPICH2MPI_LIBRARY_LOCATION = /usr/local/lib
MPICH2MPI_LIBRARIES = -lmpich  \
                      -lfmpich \
                      -lpthread \
                      -lmpl

#MPICH2MPI_LIBRARIES = -lmpi_f90 \
                   -lmpi_f77       \
                   -lmpi       \
                   -lopen-rte       \
                   -lopen-pal       \
                   -ldl        \
                   -lnsl      \
                   -lutil      \
                   -lm      \
                   -pthread

#MPICH2_MPI_TEST_RUN_CMD = mpirun -machinefile ~/mpd.hosts -np 2
MPICH2MPI_TEST_RUN_CMD = mpiexec -n 2

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

LAMMPI_INCLUDE_LOCATION = /usr/local/include
LAMMPI_LIBRARY_LOCATION = /usr/local/library
LAMMPI_LIBRARIES = -llamf77mpi \
                   -lmpi       \
                   -llam       \
                   -lutil      \
                   -ldl        \
                   -pthread

LAMMPI_TEST_RUN_CMD = lamboot -v; mpirun -np 2 -s n0

SICORMPI_INCLUDE_LOCATION = /usr/include/gentoo-multilib/n64
SICORMPI_LIBRARY_LOCATION = /usr/lib
SICORMPI_LIBRARIES = -lscmpi_debug

SICORMPI_TEST_RUN_CMD = srun -p sca -n 2 

GRCGXMPI_INCLUDE_LOCATION = /usr/include/mpich/pgi5/x86_64
GRCGXMPI_LIBRARY_LOCATION = /usr/lib
GRCGXMPI_LIBRARIES = 

GRCGXMPI_TEST_RUN_CMD = mpirun -np 2 -s n0

MPI_INCLUDE_LOCATION = $(MPICH2MPI_INCLUDE_LOCATION)
MPI_LIBRARY_LOCATION = $(MPICH2MPI_LIBRARY_LOCATION)
MPI_LIBRARIES = $(MPICH2MPI_LIBRARIES)
TEST_RUN_CMD = $(MPICH2MPI_TEST_RUN_CMD)

CMD = MessagePassingInterfaceTest.exe

TEST_SUCCESS = MessagePassingInterfaceTest.PASSED

# define commands

MV_LIB = mv libMessagePassingInterface.a $(LIB_FILES)
MV_LIB_MOD = mv *.mod $(LIB_FILES)
MV_OBJ = mv *.o $(OBJ_FILES)
MV_MOD = mv *.mod $(MOD_FILES)
MV_EXE = mv $(CMD) $(EXE_FILES)
RM_TEST_RESULTS = rm -f $(EXE_FILES)/MessagePassingInterfaceTest.*

MESSAGE_PASSING_INTERFACE_LIBS =            \
    $(LIB_FILES)/MessagePassingInterface.a  \

MESSAGE_PASSING_INTERFACE_OBJS =            \
    $(OBJ_FILES)/MessagePassingInterface.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/MPI_ABORTModule.o \
    $(OBJ_FILES)/MPI_ALLGATHERCharacterModule.o  \
    $(OBJ_FILES)/MPI_ALLGATHERReal8Module.o  \
    $(OBJ_FILES)/MPI_ALLGATHERInt4Module.o  \
    $(OBJ_FILES)/MPI_ALLGATHERInt8Module.o  \
    $(OBJ_FILES)/MPI_ALLGATHERLogicalModule.o  \
    $(OBJ_FILES)/MPI_ALLGATHERReal4Module.o  \
    $(OBJ_FILES)/MPI_ALLGATHERCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERReal8Module0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERInt4Module0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERInt8Module0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERLogicalModule0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERReal4Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEInt4Module.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEInt8Module.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEReal4Module.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEReal8Module.o  \
    $(OBJ_FILES)/MPI_ALLREDUCELogicalModule.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEInt4Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEInt8Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEReal4Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEReal8Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCELogicalModule0D.o  \
    $(OBJ_FILES)/MPI_ALLTOALLVReal8Module.o  \
    $(OBJ_FILES)/MPI_BARRIERModule.o        \
    $(OBJ_FILES)/MPI_BCASTCharacterModule0D.o \
    $(OBJ_FILES)/MPI_BCASTInt4Module0D.o  \
    $(OBJ_FILES)/MPI_BCASTInt8Module0D.o  \
    $(OBJ_FILES)/MPI_BCASTLogicalModule0D.o   \
    $(OBJ_FILES)/MPI_BCASTReal4Module0D.o     \
    $(OBJ_FILES)/MPI_BCASTReal8Module0D.o     \
    $(OBJ_FILES)/MPI_BCASTCharacterModule.o \
    $(OBJ_FILES)/MPI_BCASTInt4Module.o  \
    $(OBJ_FILES)/MPI_BCASTInt8Module.o  \
    $(OBJ_FILES)/MPI_BCASTLogicalModule.o   \
    $(OBJ_FILES)/MPI_BCASTReal4Module.o     \
    $(OBJ_FILES)/MPI_BCASTReal8Module.o     \
    $(OBJ_FILES)/MPI_CART_CREATEModule.o  \
    $(OBJ_FILES)/MPI_CART_SHIFTModule.o  \
    $(OBJ_FILES)/MPI_COMM_CREATEModule.o  \
    $(OBJ_FILES)/MPI_COMM_GET_ATTRModule.o  \
    $(OBJ_FILES)/MPI_COMM_GROUPModule.o      \
    $(OBJ_FILES)/MPI_COMM_RANKModule.o      \
    $(OBJ_FILES)/MPI_COMM_SIZEModule.o      \
    $(OBJ_FILES)/MPI_COMM_SPLITModule.o      \
    $(OBJ_FILES)/MPI_ERRHANDLER_SETModule.o      \
    $(OBJ_FILES)/MPI_ERROR_STRINGModule.o      \
    $(OBJ_FILES)/MPI_GATHERCharacterModule.o  \
    $(OBJ_FILES)/MPI_GATHERCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_GATHERReal8Module.o  \
    $(OBJ_FILES)/MPI_GATHERReal8Module0D.o  \
    $(OBJ_FILES)/MPI_GATHERInt4Module.o  \
    $(OBJ_FILES)/MPI_GATHERInt8Module.o  \
    $(OBJ_FILES)/MPI_GATHERInt4Module0D.o  \
    $(OBJ_FILES)/MPI_GATHERInt8Module0D.o  \
    $(OBJ_FILES)/MPI_GATHERLogicalModule.o  \
    $(OBJ_FILES)/MPI_GATHERLogicalModule0D.o  \
    $(OBJ_FILES)/MPI_GATHERReal4Module.o  \
    $(OBJ_FILES)/MPI_GATHERReal4Module0D.o  \
    $(OBJ_FILES)/MPI_GET_ADDRESSCharacterModule0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSInt4Module0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSInt8Module0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSLogicalModule0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSReal4Module0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSReal8Module0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSCharacterModule.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSInt4Module.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSInt8Module.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSLogicalModule.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSReal4Module.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSReal8Module.o    \
    $(OBJ_FILES)/MPI_GET_COUNTModule.o    \
    $(OBJ_FILES)/MPI_GET_VERSIONModule.o    \
    $(OBJ_FILES)/MPI_GROUP_INCLModule.o      \
    $(OBJ_FILES)/MPI_GROUP_RANKModule.o      \
    $(OBJ_FILES)/MPI_GROUP_SIZEModule.o      \
    $(OBJ_FILES)/MPI_FINALIZEModule.o       \
    $(OBJ_FILES)/MPI_FINALIZEDModule.o      \
    $(OBJ_FILES)/MPI_INITModule.o           \
    $(OBJ_FILES)/MPI_INITIALIZEDModule.o    \
    $(OBJ_FILES)/MPI_INTERCOMM_CREATEModule.o    \
    $(OBJ_FILES)/MPI_IPROBEModule.o         \
    $(OBJ_FILES)/MPI_IRECVCharacterModule.o \
    $(OBJ_FILES)/MPI_IRECVInt4Module.o  \
    $(OBJ_FILES)/MPI_IRECVInt8Module.o  \
    $(OBJ_FILES)/MPI_IRECVLogicalModule.o   \
    $(OBJ_FILES)/MPI_IRECVReal4Module.o     \
    $(OBJ_FILES)/MPI_IRECVReal8Module.o     \
    $(OBJ_FILES)/MPI_IRECVCharacterModule0D.o \
    $(OBJ_FILES)/MPI_IRECVInt4Module0D.o  \
    $(OBJ_FILES)/MPI_IRECVInt8Module0D.o  \
    $(OBJ_FILES)/MPI_IRECVLogicalModule0D.o   \
    $(OBJ_FILES)/MPI_IRECVReal4Module0D.o     \
    $(OBJ_FILES)/MPI_IRECVReal8Module0D.o     \
    $(OBJ_FILES)/MPI_ISENDCharacterModule.o \
    $(OBJ_FILES)/MPI_ISENDInt4Module.o  \
    $(OBJ_FILES)/MPI_ISENDInt8Module.o  \
    $(OBJ_FILES)/MPI_ISENDLogicalModule.o   \
    $(OBJ_FILES)/MPI_ISENDReal4Module.o     \
    $(OBJ_FILES)/MPI_ISENDReal8Module.o     \
    $(OBJ_FILES)/MPI_ISENDCharacterModule0D.o \
    $(OBJ_FILES)/MPI_ISENDInt4Module0D.o  \
    $(OBJ_FILES)/MPI_ISENDInt8Module0D.o  \
    $(OBJ_FILES)/MPI_ISENDLogicalModule0D.o   \
    $(OBJ_FILES)/MPI_ISENDReal4Module0D.o     \
    $(OBJ_FILES)/MPI_ISENDReal8Module0D.o     \
    $(OBJ_FILES)/MPI_PROBEModule.o          \
    $(OBJ_FILES)/MPI_RECVCharacterModule.o  \
    $(OBJ_FILES)/MPI_RECVInt4Module.o   \
    $(OBJ_FILES)/MPI_RECVInt8Module.o   \
    $(OBJ_FILES)/MPI_RECVLogicalModule.o    \
    $(OBJ_FILES)/MPI_RECVReal4Module.o      \
    $(OBJ_FILES)/MPI_RECVReal8Module.o      \
    $(OBJ_FILES)/MPI_RECVCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_RECVInt4Module0D.o   \
    $(OBJ_FILES)/MPI_RECVInt8Module0D.o   \
    $(OBJ_FILES)/MPI_RECVLogicalModule0D.o    \
    $(OBJ_FILES)/MPI_RECVReal4Module0D.o      \
    $(OBJ_FILES)/MPI_RECVReal8Module0D.o      \
    $(OBJ_FILES)/MPI_SENDCharacterModule.o  \
    $(OBJ_FILES)/MPI_SENDInt4Module.o   \
    $(OBJ_FILES)/MPI_SENDInt8Module.o   \
    $(OBJ_FILES)/MPI_SENDLogicalModule.o    \
    $(OBJ_FILES)/MPI_SENDReal4Module.o      \
    $(OBJ_FILES)/MPI_SENDReal8Module.o      \
    $(OBJ_FILES)/MPI_SENDCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_SENDInt4Module0D.o   \
    $(OBJ_FILES)/MPI_SENDInt8Module0D.o   \
    $(OBJ_FILES)/MPI_SENDLogicalModule0D.o    \
    $(OBJ_FILES)/MPI_SENDReal4Module0D.o      \
    $(OBJ_FILES)/MPI_SENDReal8Module0D.o      \
    $(OBJ_FILES)/MPI_SENDRECVCharacterModule.o  \
    $(OBJ_FILES)/MPI_SENDRECVInt4Module.o   \
    $(OBJ_FILES)/MPI_SENDRECVInt8Module.o   \
    $(OBJ_FILES)/MPI_SENDRECVLogicalModule.o    \
    $(OBJ_FILES)/MPI_SENDRECVReal4Module.o      \
    $(OBJ_FILES)/MPI_SENDRECVReal8Module.o      \
    $(OBJ_FILES)/MPI_SENDRECVCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_SENDRECVInt4Module0D.o   \
    $(OBJ_FILES)/MPI_SENDRECVInt8Module0D.o   \
    $(OBJ_FILES)/MPI_SENDRECVLogicalModule0D.o    \
    $(OBJ_FILES)/MPI_SENDRECVReal4Module0D.o      \
    $(OBJ_FILES)/MPI_SENDRECVReal8Module0D.o      \
    $(OBJ_FILES)/MPI_SIZEOFCharModule.o      \
    $(OBJ_FILES)/MPI_SIZEOFInt4Module.o      \
    $(OBJ_FILES)/MPI_SIZEOFInt8Module.o      \
    $(OBJ_FILES)/MPI_SIZEOFLogicalModule.o      \
    $(OBJ_FILES)/MPI_SIZEOFReal4Module.o      \
    $(OBJ_FILES)/MPI_SIZEOFReal8Module.o      \
    $(OBJ_FILES)/MPI_TESTModule.o           \
    $(OBJ_FILES)/MPI_TYPE_COMMITModule.o      \
    $(OBJ_FILES)/MPI_TYPE_CREATE_STRUCTModule.o      \
    $(OBJ_FILES)/MPI_TYPE_FREEModule.o      \
    $(OBJ_FILES)/MPI_TYPE_GET_CONTENTSModule.o      \
    $(OBJ_FILES)/MPI_TYPE_GET_ENVELOPEModule.o      \
    $(OBJ_FILES)/MPI_TYPE_GET_EXTENTModule.o      \
    $(OBJ_FILES)/MPI_TYPE_SIZEModule.o      \
    $(OBJ_FILES)/MPI_WAITModule.o           \
    $(OBJ_FILES)/MPI_WTICKModule.o          \
    $(OBJ_FILES)/MPI_WTIMEModule.o           

OBJS = \
 $(MESSAGE_PASSING_INTERFACE_OBJS)

TEST_OBJS = \
 $(OBJ_FILES)/MessagePassingInterfaceTest.o

LIBS = \
 -lMessagePassingInterface

INCLUDE_MODS = \
 -I$(MOD_FILES)

# Linkage rule

all: $(LIB_FILES)/libMessagePassingInterface.a
	@echo " "
	@echo "-------------------------------------------------- "
	@echo "-   Hixon Technologies MPI Interface routines    - "
	@echo "-    Copyright 2020 by Hixon Technologies, LLC   - "
	@echo "-    Used with permission.                       - "
	@echo "-    These routines come with no warranty.       - "
	@echo "-                                                - "
	@echo "-   Contact:  Ray Hixon, Hixon Technologies      - "
	@echo "-             fshixon@yahoo.com                  - "
	@echo "-------------------------------------------------- "
	@echo " "

test: $(TEST_SUCCESS)

$(TEST_SUCCESS): \
      $(TEST_OBJS) \
      $(LIB_FILES)/libMessagePassingInterface.a
	$(RM_TEST_RESULTS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES) $(INCLUDE_MODS)
	$(MV_EXE)
	cd $(EXE_FILES); $(TEST_RUN_CMD) ./$(CMD)
	cp $(EXE_FILES)/$(TEST_SUCCESS) .

clean:
	/bin/rm -f *.a 
	/bin/rm -f *.o 
	/bin/rm -f *.d 
	/bin/rm -f *.mod 
	/bin/rm -f ifc*
	/bin/rm -f *.pc
	/bin/rm -f *.pcl
	/bin/rm -f $(LIB_FILES)/*.a
	/bin/rm -f $(OBJ_FILES)/*.o
	/bin/rm -f $(OBJ_FILES)/*.d
	/bin/rm -f $(LIB_FILES)/*.mod 
	/bin/rm -f $(MOD_FILES)/*.mod 

cleanTest:
	rm -f *.FAILED
	rm -f ../Executables/*.FAILED
	rm -f *.PASSED
	rm -f ../Executables/*.PASSED

# Dependency definitions

$(LIB_FILES)/libMessagePassingInterface.a: \
        $(OBJS)
	ar r libMessagePassingInterface.a $(OBJS)
	$(MV_LIB)

$(OBJ_FILES)/MessagePassingInterfaceTest.o:                \
    $(SRC_FILES)/MessagePassingInterfaceTest.f90      \
    $(OBJ_FILES)/MessagePassingInterface.o
	$(F95) $(F95FLAGS) $(SRC_FILES)/MessagePassingInterfaceTest.f90 $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION)
	$(MV_OBJ)

$(OBJ_FILES)/IncludeMPIImplementation.o: \
    $(SRC_FILES)/IncludeMPIImplementation.f90
	$(F95) $(F95FLAGS2) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/IncludeMPIImplementation.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MessagePassingInterface.o:                \
    $(SRC_FILES)/MessagePassingInterface.f90      \
    $(OBJ_FILES)/IncludeMPIImplementation.o \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/MPI_ABORTModule.o \
    $(OBJ_FILES)/MPI_ALLGATHERCharacterModule.o  \
    $(OBJ_FILES)/MPI_ALLGATHERReal4Module.o  \
    $(OBJ_FILES)/MPI_ALLGATHERReal8Module.o  \
    $(OBJ_FILES)/MPI_ALLGATHERInt4Module.o  \
    $(OBJ_FILES)/MPI_ALLGATHERInt8Module.o  \
    $(OBJ_FILES)/MPI_ALLGATHERLogicalModule.o  \
    $(OBJ_FILES)/MPI_ALLGATHERCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERReal4Module0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERReal8Module0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERInt4Module0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERInt8Module0D.o  \
    $(OBJ_FILES)/MPI_ALLGATHERLogicalModule0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEReal4Module.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEReal8Module.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEInt4Module.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEInt8Module.o  \
    $(OBJ_FILES)/MPI_ALLREDUCELogicalModule.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEReal4Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEReal8Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEInt4Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCEInt8Module0D.o  \
    $(OBJ_FILES)/MPI_ALLREDUCELogicalModule0D.o  \
    $(OBJ_FILES)/MPI_ALLTOALLVReal8Module.o  \
    $(OBJ_FILES)/MPI_BARRIERModule.o        \
    $(OBJ_FILES)/MPI_BCASTCharacterModule0D.o \
    $(OBJ_FILES)/MPI_BCASTInt4Module0D.o  \
    $(OBJ_FILES)/MPI_BCASTInt8Module0D.o  \
    $(OBJ_FILES)/MPI_BCASTLogicalModule0D.o   \
    $(OBJ_FILES)/MPI_BCASTReal4Module0D.o     \
    $(OBJ_FILES)/MPI_BCASTReal8Module0D.o     \
    $(OBJ_FILES)/MPI_BCASTCharacterModule.o \
    $(OBJ_FILES)/MPI_BCASTInt4Module.o  \
    $(OBJ_FILES)/MPI_BCASTInt8Module.o  \
    $(OBJ_FILES)/MPI_BCASTLogicalModule.o   \
    $(OBJ_FILES)/MPI_BCASTReal4Module.o     \
    $(OBJ_FILES)/MPI_BCASTReal8Module.o     \
    $(OBJ_FILES)/MPI_CART_CREATEModule.o  \
    $(OBJ_FILES)/MPI_CART_SHIFTModule.o  \
    $(OBJ_FILES)/MPI_COMM_CREATEModule.o  \
    $(OBJ_FILES)/MPI_COMM_GET_ATTRModule.o      \
    $(OBJ_FILES)/MPI_COMM_GROUPModule.o      \
    $(OBJ_FILES)/MPI_COMM_RANKModule.o      \
    $(OBJ_FILES)/MPI_COMM_SIZEModule.o      \
    $(OBJ_FILES)/MPI_COMM_SPLITModule.o      \
    $(OBJ_FILES)/MPI_ERROR_STRINGModule.o      \
    $(OBJ_FILES)/MPI_ERRHANDLER_SETModule.o      \
    $(OBJ_FILES)/MPI_GATHERCharacterModule.o  \
    $(OBJ_FILES)/MPI_GATHERCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_GATHERReal8Module.o  \
    $(OBJ_FILES)/MPI_GATHERReal8Module0D.o  \
    $(OBJ_FILES)/MPI_GATHERInt4Module.o  \
    $(OBJ_FILES)/MPI_GATHERInt8Module.o  \
    $(OBJ_FILES)/MPI_GATHERInt4Module0D.o  \
    $(OBJ_FILES)/MPI_GATHERInt8Module0D.o  \
    $(OBJ_FILES)/MPI_GATHERLogicalModule.o  \
    $(OBJ_FILES)/MPI_GATHERLogicalModule0D.o  \
    $(OBJ_FILES)/MPI_GATHERReal4Module.o  \
    $(OBJ_FILES)/MPI_GATHERReal4Module0D.o  \
    $(OBJ_FILES)/MPI_GET_ADDRESSCharacterModule0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSInt4Module0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSInt8Module0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSLogicalModule0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSReal4Module0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSReal8Module0D.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSCharacterModule.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSInt4Module.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSInt8Module.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSLogicalModule.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSReal4Module.o    \
    $(OBJ_FILES)/MPI_GET_ADDRESSReal8Module.o    \
    $(OBJ_FILES)/MPI_GET_COUNTModule.o    \
    $(OBJ_FILES)/MPI_GET_VERSIONModule.o    \
    $(OBJ_FILES)/MPI_GROUP_INCLModule.o      \
    $(OBJ_FILES)/MPI_GROUP_RANKModule.o      \
    $(OBJ_FILES)/MPI_GROUP_SIZEModule.o      \
    $(OBJ_FILES)/MPI_FINALIZEModule.o       \
    $(OBJ_FILES)/MPI_FINALIZEDModule.o      \
    $(OBJ_FILES)/MPI_INITModule.o           \
    $(OBJ_FILES)/MPI_INITIALIZEDModule.o    \
    $(OBJ_FILES)/MPI_INTERCOMM_CREATEModule.o    \
    $(OBJ_FILES)/MPI_IPROBEModule.o         \
    $(OBJ_FILES)/MPI_IRECVCharacterModule.o \
    $(OBJ_FILES)/MPI_IRECVInt4Module.o  \
    $(OBJ_FILES)/MPI_IRECVInt8Module.o  \
    $(OBJ_FILES)/MPI_IRECVLogicalModule.o   \
    $(OBJ_FILES)/MPI_IRECVReal4Module.o     \
    $(OBJ_FILES)/MPI_IRECVReal8Module.o     \
    $(OBJ_FILES)/MPI_IRECVCharacterModule0D.o \
    $(OBJ_FILES)/MPI_IRECVInt4Module0D.o  \
    $(OBJ_FILES)/MPI_IRECVInt8Module0D.o  \
    $(OBJ_FILES)/MPI_IRECVLogicalModule0D.o   \
    $(OBJ_FILES)/MPI_IRECVReal4Module0D.o     \
    $(OBJ_FILES)/MPI_IRECVReal8Module0D.o     \
    $(OBJ_FILES)/MPI_ISENDCharacterModule.o \
    $(OBJ_FILES)/MPI_ISENDInt4Module.o  \
    $(OBJ_FILES)/MPI_ISENDInt8Module.o  \
    $(OBJ_FILES)/MPI_ISENDLogicalModule.o   \
    $(OBJ_FILES)/MPI_ISENDReal4Module.o     \
    $(OBJ_FILES)/MPI_ISENDReal8Module.o     \
    $(OBJ_FILES)/MPI_ISENDCharacterModule0D.o \
    $(OBJ_FILES)/MPI_ISENDInt4Module0D.o  \
    $(OBJ_FILES)/MPI_ISENDInt8Module0D.o  \
    $(OBJ_FILES)/MPI_ISENDLogicalModule0D.o   \
    $(OBJ_FILES)/MPI_ISENDReal4Module0D.o     \
    $(OBJ_FILES)/MPI_ISENDReal8Module0D.o     \
    $(OBJ_FILES)/MPI_PROBEModule.o          \
    $(OBJ_FILES)/MPI_RECVCharacterModule.o  \
    $(OBJ_FILES)/MPI_RECVInt4Module.o   \
    $(OBJ_FILES)/MPI_RECVInt8Module.o   \
    $(OBJ_FILES)/MPI_RECVLogicalModule.o    \
    $(OBJ_FILES)/MPI_RECVReal4Module.o      \
    $(OBJ_FILES)/MPI_RECVReal8Module.o      \
    $(OBJ_FILES)/MPI_RECVCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_RECVInt4Module0D.o   \
    $(OBJ_FILES)/MPI_RECVInt8Module0D.o   \
    $(OBJ_FILES)/MPI_RECVLogicalModule0D.o    \
    $(OBJ_FILES)/MPI_RECVReal4Module0D.o      \
    $(OBJ_FILES)/MPI_RECVReal8Module0D.o      \
    $(OBJ_FILES)/MPI_SENDCharacterModule.o  \
    $(OBJ_FILES)/MPI_SENDInt4Module.o   \
    $(OBJ_FILES)/MPI_SENDInt8Module.o   \
    $(OBJ_FILES)/MPI_SENDLogicalModule.o    \
    $(OBJ_FILES)/MPI_SENDReal4Module.o      \
    $(OBJ_FILES)/MPI_SENDReal8Module.o      \
    $(OBJ_FILES)/MPI_SENDCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_SENDInt4Module0D.o   \
    $(OBJ_FILES)/MPI_SENDInt8Module0D.o   \
    $(OBJ_FILES)/MPI_SENDLogicalModule0D.o    \
    $(OBJ_FILES)/MPI_SENDReal4Module0D.o      \
    $(OBJ_FILES)/MPI_SENDReal8Module0D.o      \
    $(OBJ_FILES)/MPI_SENDRECVCharacterModule.o  \
    $(OBJ_FILES)/MPI_SENDRECVInt4Module.o   \
    $(OBJ_FILES)/MPI_SENDRECVInt8Module.o   \
    $(OBJ_FILES)/MPI_SENDRECVLogicalModule.o    \
    $(OBJ_FILES)/MPI_SENDRECVReal4Module.o      \
    $(OBJ_FILES)/MPI_SENDRECVReal8Module.o      \
    $(OBJ_FILES)/MPI_SENDRECVCharacterModule0D.o  \
    $(OBJ_FILES)/MPI_SENDRECVInt4Module0D.o   \
    $(OBJ_FILES)/MPI_SENDRECVInt8Module0D.o   \
    $(OBJ_FILES)/MPI_SENDRECVLogicalModule0D.o    \
    $(OBJ_FILES)/MPI_SENDRECVReal4Module0D.o      \
    $(OBJ_FILES)/MPI_SENDRECVReal8Module0D.o      \
    $(OBJ_FILES)/MPI_SIZEOFCharModule.o      \
    $(OBJ_FILES)/MPI_SIZEOFInt4Module.o      \
    $(OBJ_FILES)/MPI_SIZEOFInt8Module.o      \
    $(OBJ_FILES)/MPI_SIZEOFLogicalModule.o      \
    $(OBJ_FILES)/MPI_SIZEOFReal4Module.o      \
    $(OBJ_FILES)/MPI_SIZEOFReal8Module.o      \
    $(OBJ_FILES)/MPI_TESTModule.o           \
    $(OBJ_FILES)/MPI_TYPE_COMMITModule.o      \
    $(OBJ_FILES)/MPI_TYPE_CREATE_STRUCTModule.o      \
    $(OBJ_FILES)/MPI_TYPE_FREEModule.o      \
    $(OBJ_FILES)/MPI_TYPE_GET_CONTENTSModule.o      \
    $(OBJ_FILES)/MPI_TYPE_GET_ENVELOPEModule.o      \
    $(OBJ_FILES)/MPI_TYPE_GET_EXTENTModule.o      \
    $(OBJ_FILES)/MPI_TYPE_SIZEModule.o      \
    $(OBJ_FILES)/MPI_WAITModule.o           \
    $(OBJ_FILES)/MPI_WTICKModule.o          \
    $(OBJ_FILES)/MPI_WTIMEModule.o           
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MessagePassingInterface.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPIKindDefs.o: \
    $(SRC_FILES)/MPIKindDefs.f90 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPIKindDefs.f90
	$(MV_OBJ); $(MV_MOD)

#-----------------------MPI_ABORT----------------------------------------

$(OBJ_FILES)/MPI_ABORTModule.o: \
    $(SRC_FILES)/MPI_ABORTModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ABORTModule.f90
	$(MV_OBJ); $(MV_MOD)

#-----------------------MPI_ALLGATHER----------------------------------------

$(OBJ_FILES)/MPI_ALLGATHERCharacterModule.o: \
    $(SRC_FILES)/MPI_ALLGATHERCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERReal4Module.o: \
    $(SRC_FILES)/MPI_ALLGATHERReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERReal8Module.o: \
    $(SRC_FILES)/MPI_ALLGATHERReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERInt4Module.o: \
    $(SRC_FILES)/MPI_ALLGATHERInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERInt8Module.o: \
    $(SRC_FILES)/MPI_ALLGATHERInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERLogicalModule.o: \
    $(SRC_FILES)/MPI_ALLGATHERLogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERReal4Module0D.o: \
    $(SRC_FILES)/MPI_ALLGATHERReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERReal8Module0D.o: \
    $(SRC_FILES)/MPI_ALLGATHERReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERCharacterModule0D.o: \
    $(SRC_FILES)/MPI_ALLGATHERCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERInt4Module0D.o: \
    $(SRC_FILES)/MPI_ALLGATHERInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERInt8Module0D.o: \
    $(SRC_FILES)/MPI_ALLGATHERInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLGATHERLogicalModule0D.o: \
    $(SRC_FILES)/MPI_ALLGATHERLogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLGATHERLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_ALLREDUCE--------------------------------------------------

$(OBJ_FILES)/MPI_ALLREDUCEInt4Module0D.o: \
    $(SRC_FILES)/MPI_ALLREDUCEInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCEInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCEInt8Module0D.o: \
    $(SRC_FILES)/MPI_ALLREDUCEInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCEInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCELogicalModule0D.o: \
    $(SRC_FILES)/MPI_ALLREDUCELogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCELogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCEReal4Module0D.o: \
    $(SRC_FILES)/MPI_ALLREDUCEReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCEReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCEReal8Module0D.o: \
    $(SRC_FILES)/MPI_ALLREDUCEReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCEReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCEInt4Module.o: \
    $(SRC_FILES)/MPI_ALLREDUCEInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCEInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCEInt8Module.o: \
    $(SRC_FILES)/MPI_ALLREDUCEInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCEInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCELogicalModule.o: \
    $(SRC_FILES)/MPI_ALLREDUCELogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCELogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCEReal4Module.o: \
    $(SRC_FILES)/MPI_ALLREDUCEReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCEReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ALLREDUCEReal8Module.o: \
    $(SRC_FILES)/MPI_ALLREDUCEReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLREDUCEReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_ALLTOALLV--------------------------------------------------

$(OBJ_FILES)/MPI_ALLTOALLVReal8Module.o: \
    $(SRC_FILES)/MPI_ALLTOALLVReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ALLTOALLVReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_BARRIER--------------------------------------------------

$(OBJ_FILES)/MPI_BARRIERModule.o: \
    $(SRC_FILES)/MPI_BARRIERModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BARRIERModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_BCAST--------------------------------------------------

$(OBJ_FILES)/MPI_BCASTCharacterModule.o: \
    $(SRC_FILES)/MPI_BCASTCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTInt4Module.o: \
    $(SRC_FILES)/MPI_BCASTInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTInt8Module.o: \
    $(SRC_FILES)/MPI_BCASTInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTLogicalModule.o: \
    $(SRC_FILES)/MPI_BCASTLogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTReal4Module.o: \
    $(SRC_FILES)/MPI_BCASTReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTReal8Module.o: \
    $(SRC_FILES)/MPI_BCASTReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTCharacterModule0D.o: \
    $(SRC_FILES)/MPI_BCASTCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTInt4Module0D.o: \
    $(SRC_FILES)/MPI_BCASTInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTInt8Module0D.o: \
    $(SRC_FILES)/MPI_BCASTInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTLogicalModule0D.o: \
    $(SRC_FILES)/MPI_BCASTLogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTReal4Module0D.o: \
    $(SRC_FILES)/MPI_BCASTReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_BCASTReal8Module0D.o: \
    $(SRC_FILES)/MPI_BCASTReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_BCASTReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_CART_CREATE------------------------------------------

$(OBJ_FILES)/MPI_CART_CREATEModule.o: \
    $(SRC_FILES)/MPI_CART_CREATEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_CART_CREATEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_CART_SHIFT------------------------------------------

$(OBJ_FILES)/MPI_CART_SHIFTModule.o: \
    $(SRC_FILES)/MPI_CART_SHIFTModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_CART_SHIFTModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_COMM_CREATE------------------------------------------

$(OBJ_FILES)/MPI_COMM_CREATEModule.o: \
    $(SRC_FILES)/MPI_COMM_CREATEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_COMM_CREATEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_COMM_GET_ATTR------------------------------------------

$(OBJ_FILES)/MPI_COMM_GET_ATTRModule.o: \
    $(SRC_FILES)/MPI_COMM_GET_ATTRModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_COMM_GET_ATTRModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_COMM_GROUP----------------------------------------------

$(OBJ_FILES)/MPI_COMM_GROUPModule.o: \
    $(SRC_FILES)/MPI_COMM_GROUPModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_COMM_GROUPModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_COMM_RANK----------------------------------------------

$(OBJ_FILES)/MPI_COMM_RANKModule.o: \
    $(SRC_FILES)/MPI_COMM_RANKModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_COMM_RANKModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_COMM_SIZE----------------------------------------------

$(OBJ_FILES)/MPI_COMM_SIZEModule.o: \
    $(SRC_FILES)/MPI_COMM_SIZEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_COMM_SIZEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_COMM_SPLIT----------------------------------------------

$(OBJ_FILES)/MPI_COMM_SPLITModule.o: \
    $(SRC_FILES)/MPI_COMM_SPLITModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_COMM_SPLITModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_ERRHANDLER_SET----------------------------------------------

$(OBJ_FILES)/MPI_ERRHANDLER_SETModule.o: \
    $(SRC_FILES)/MPI_ERRHANDLER_SETModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ERRHANDLER_SETModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_ERROR_STRING----------------------------------------------

$(OBJ_FILES)/MPI_ERROR_STRINGModule.o: \
    $(SRC_FILES)/MPI_ERROR_STRINGModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ERROR_STRINGModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_FINALIZE-----------------------------------------------

$(OBJ_FILES)/MPI_FINALIZEModule.o: \
    $(SRC_FILES)/MPI_FINALIZEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_FINALIZEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_FINALIZED----------------------------------------------

$(OBJ_FILES)/MPI_FINALIZEDModule.o: \
    $(SRC_FILES)/MPI_FINALIZEDModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_FINALIZEDModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GATHER-------------------------------------------------

$(OBJ_FILES)/MPI_GATHERReal4Module.o: \
    $(SRC_FILES)/MPI_GATHERReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERReal4Module0D.o: \
    $(SRC_FILES)/MPI_GATHERReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERReal8Module.o: \
    $(SRC_FILES)/MPI_GATHERReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERReal8Module0D.o: \
    $(SRC_FILES)/MPI_GATHERReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERInt4Module.o: \
    $(SRC_FILES)/MPI_GATHERInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERInt4Module0D.o: \
    $(SRC_FILES)/MPI_GATHERInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERInt8Module.o: \
    $(SRC_FILES)/MPI_GATHERInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERInt8Module0D.o: \
    $(SRC_FILES)/MPI_GATHERInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERLogicalModule.o: \
    $(SRC_FILES)/MPI_GATHERLogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERLogicalModule0D.o: \
    $(SRC_FILES)/MPI_GATHERLogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERCharacterModule.o: \
    $(SRC_FILES)/MPI_GATHERCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GATHERCharacterModule0D.o: \
    $(SRC_FILES)/MPI_GATHERCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GATHERCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GET_COUNT----------------------------------------------

$(OBJ_FILES)/MPI_GET_COUNTModule.o: \
    $(SRC_FILES)/MPI_GET_COUNTModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_COUNTModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GET_VERSION----------------------------------------------

$(OBJ_FILES)/MPI_GET_VERSIONModule.o: \
    $(SRC_FILES)/MPI_GET_VERSIONModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_VERSIONModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GROUP_INCL----------------------------------------------

$(OBJ_FILES)/MPI_GROUP_INCLModule.o: \
    $(SRC_FILES)/MPI_GROUP_INCLModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GROUP_INCLModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GROUP_RANK----------------------------------------------

$(OBJ_FILES)/MPI_GROUP_RANKModule.o: \
    $(SRC_FILES)/MPI_GROUP_RANKModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GROUP_RANKModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GROUP_SIZE----------------------------------------------

$(OBJ_FILES)/MPI_GROUP_SIZEModule.o: \
    $(SRC_FILES)/MPI_GROUP_SIZEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GROUP_SIZEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_INITIALIZED----------------------------------------------

$(OBJ_FILES)/MPI_INITIALIZEDModule.o: \
    $(SRC_FILES)/MPI_INITIALIZEDModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_INITIALIZEDModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_INTERCOMM_CREATE----------------------------------------------

$(OBJ_FILES)/MPI_INTERCOMM_CREATEModule.o: \
    $(SRC_FILES)/MPI_INTERCOMM_CREATEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_INTERCOMM_CREATEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_INIT----------------------------------------------

$(OBJ_FILES)/MPI_INITModule.o: \
    $(SRC_FILES)/MPI_INITModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_INITModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_IPROBE----------------------------------------------

$(OBJ_FILES)/MPI_IPROBEModule.o: \
    $(SRC_FILES)/MPI_IPROBEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IPROBEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_IRECV----------------------------------------------

$(OBJ_FILES)/MPI_IRECVCharacterModule.o: \
    $(SRC_FILES)/MPI_IRECVCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVInt4Module.o: \
    $(SRC_FILES)/MPI_IRECVInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVInt8Module.o: \
    $(SRC_FILES)/MPI_IRECVInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVLogicalModule.o: \
    $(SRC_FILES)/MPI_IRECVLogicalModule.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVReal4Module.o: \
    $(SRC_FILES)/MPI_IRECVReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVReal8Module.o: \
    $(SRC_FILES)/MPI_IRECVReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVCharacterModule0D.o: \
    $(SRC_FILES)/MPI_IRECVCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVInt4Module0D.o: \
    $(SRC_FILES)/MPI_IRECVInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVInt8Module0D.o: \
    $(SRC_FILES)/MPI_IRECVInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVLogicalModule0D.o: \
    $(SRC_FILES)/MPI_IRECVLogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVReal4Module0D.o: \
    $(SRC_FILES)/MPI_IRECVReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_IRECVReal8Module0D.o: \
    $(SRC_FILES)/MPI_IRECVReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_IRECVReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_ISEND----------------------------------------------

$(OBJ_FILES)/MPI_ISENDCharacterModule.o: \
    $(SRC_FILES)/MPI_ISENDCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDInt4Module.o: \
    $(SRC_FILES)/MPI_ISENDInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDInt8Module.o: \
    $(SRC_FILES)/MPI_ISENDInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDLogicalModule.o: \
    $(SRC_FILES)/MPI_ISENDLogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDReal4Module.o: \
    $(SRC_FILES)/MPI_ISENDReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDReal4Module.f90

$(OBJ_FILES)/MPI_ISENDReal8Module.o: \
    $(SRC_FILES)/MPI_ISENDReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDCharacterModule0D.o: \
    $(SRC_FILES)/MPI_ISENDCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDInt4Module0D.o: \
    $(SRC_FILES)/MPI_ISENDInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDInt8Module0D.o: \
    $(SRC_FILES)/MPI_ISENDInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDLogicalModule0D.o: \
    $(SRC_FILES)/MPI_ISENDLogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDReal4Module0D.o: \
    $(SRC_FILES)/MPI_ISENDReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_ISENDReal8Module0D.o: \
    $(SRC_FILES)/MPI_ISENDReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_ISENDReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_PROBE----------------------------------------------

$(OBJ_FILES)/MPI_PROBEModule.o: \
    $(SRC_FILES)/MPI_PROBEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_PROBEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_RECV----------------------------------------------

$(OBJ_FILES)/MPI_RECVCharacterModule.o: \
    $(SRC_FILES)/MPI_RECVCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVInt4Module.o: \
    $(SRC_FILES)/MPI_RECVInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVInt8Module.o: \
    $(SRC_FILES)/MPI_RECVInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVLogicalModule.o: \
    $(SRC_FILES)/MPI_RECVLogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVReal4Module.o: \
    $(SRC_FILES)/MPI_RECVReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVReal8Module.o: \
    $(SRC_FILES)/MPI_RECVReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVCharacterModule0D.o: \
    $(SRC_FILES)/MPI_RECVCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVInt4Module0D.o: \
    $(SRC_FILES)/MPI_RECVInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVInt8Module0D.o: \
    $(SRC_FILES)/MPI_RECVInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVLogicalModule0D.o: \
    $(SRC_FILES)/MPI_RECVLogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVReal4Module0D.o: \
    $(SRC_FILES)/MPI_RECVReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_RECVReal8Module0D.o: \
    $(SRC_FILES)/MPI_RECVReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_RECVReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_SEND----------------------------------------------

$(OBJ_FILES)/MPI_SENDCharacterModule.o: \
    $(SRC_FILES)/MPI_SENDCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDInt4Module.o: \
    $(SRC_FILES)/MPI_SENDInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDInt8Module.o: \
    $(SRC_FILES)/MPI_SENDInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDLogicalModule.o: \
    $(SRC_FILES)/MPI_SENDLogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDReal4Module.o: \
    $(SRC_FILES)/MPI_SENDReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDReal8Module.o: \
    $(SRC_FILES)/MPI_SENDReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDCharacterModule0D.o: \
    $(SRC_FILES)/MPI_SENDCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDInt4Module0D.o: \
    $(SRC_FILES)/MPI_SENDInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDInt8Module0D.o: \
    $(SRC_FILES)/MPI_SENDInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDLogicalModule0D.o: \
    $(SRC_FILES)/MPI_SENDLogicalModule0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDReal4Module0D.o: \
    $(SRC_FILES)/MPI_SENDReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDReal8Module0D.o: \
    $(SRC_FILES)/MPI_SENDReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_SENDRECV----------------------------------------------

$(OBJ_FILES)/MPI_SENDRECVCharacterModule.o: \
    $(SRC_FILES)/MPI_SENDRECVCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVInt4Module.o: \
    $(SRC_FILES)/MPI_SENDRECVInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVInt8Module.o: \
    $(SRC_FILES)/MPI_SENDRECVInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVLogicalModule.o: \
    $(SRC_FILES)/MPI_SENDRECVLogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVReal4Module.o: \
    $(SRC_FILES)/MPI_SENDRECVReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVReal8Module.o: \
    $(SRC_FILES)/MPI_SENDRECVReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVCharacterModule0D.o: \
    $(SRC_FILES)/MPI_SENDRECVCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVInt4Module0D.o: \
    $(SRC_FILES)/MPI_SENDRECVInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVInt8Module0D.o: \
    $(SRC_FILES)/MPI_SENDRECVInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVLogicalModule0D.o: \
    $(SRC_FILES)/MPI_SENDRECVLogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVReal4Module0D.o: \
    $(SRC_FILES)/MPI_SENDRECVReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SENDRECVReal8Module0D.o: \
    $(SRC_FILES)/MPI_SENDRECVReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SENDRECVReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_SIZEOF--------------------------------------------

$(OBJ_FILES)/MPI_SIZEOFCharModule.o: \
    $(SRC_FILES)/MPI_SIZEOFCharModule.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SIZEOFCharModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SIZEOFInt4Module.o: \
    $(SRC_FILES)/MPI_SIZEOFInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SIZEOFInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SIZEOFInt8Module.o: \
    $(SRC_FILES)/MPI_SIZEOFInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SIZEOFInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SIZEOFLogicalModule.o: \
    $(SRC_FILES)/MPI_SIZEOFLogicalModule.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SIZEOFLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SIZEOFReal4Module.o: \
    $(SRC_FILES)/MPI_SIZEOFReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SIZEOFReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_SIZEOFReal8Module.o: \
    $(SRC_FILES)/MPI_SIZEOFReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_SIZEOFReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_TEST----------------------------------------------

$(OBJ_FILES)/MPI_TESTModule.o: \
    $(SRC_FILES)/MPI_TESTModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_TESTModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_TYPE_COMMIT----------------------------------------------

$(OBJ_FILES)/MPI_TYPE_COMMITModule.o: \
    $(SRC_FILES)/MPI_TYPE_COMMITModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_TYPE_COMMITModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_TYPE_CREATE_STRUCT----------------------------------------------

$(OBJ_FILES)/MPI_TYPE_CREATE_STRUCTModule.o: \
    $(SRC_FILES)/MPI_TYPE_CREATE_STRUCTModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_TYPE_CREATE_STRUCTModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_TYPE_FREE----------------------------------------------

$(OBJ_FILES)/MPI_TYPE_FREEModule.o: \
    $(SRC_FILES)/MPI_TYPE_FREEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_TYPE_FREEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GET_ADDRESS----------------------------------------------

$(OBJ_FILES)/MPI_GET_ADDRESSCharacterModule0D.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSCharacterModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSCharacterModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSInt4Module0D.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSInt4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSInt4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSInt8Module0D.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSInt8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSInt8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSLogicalModule0D.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSLogicalModule0D.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSLogicalModule0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSReal4Module0D.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSReal4Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSReal4Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSReal8Module0D.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSReal8Module0D.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSReal8Module0D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSCharacterModule.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSCharacterModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSCharacterModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSInt4Module.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSInt4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSInt4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSInt8Module.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSInt8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSInt8Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSLogicalModule.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSLogicalModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSLogicalModule.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSReal4Module.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSReal4Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSReal4Module.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/MPI_GET_ADDRESSReal8Module.o: \
    $(SRC_FILES)/MPI_GET_ADDRESSReal8Module.f90 \
    $(OBJ_FILES)/MPIKindDefs.o  \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_GET_ADDRESSReal8Module.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GET_CONTENTS----------------------------------------------

$(OBJ_FILES)/MPI_TYPE_GET_CONTENTSModule.o: \
    $(SRC_FILES)/MPI_TYPE_GET_CONTENTSModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_TYPE_GET_CONTENTSModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GET_ENVELOPE----------------------------------------------

$(OBJ_FILES)/MPI_TYPE_GET_ENVELOPEModule.o: \
    $(SRC_FILES)/MPI_TYPE_GET_ENVELOPEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_TYPE_GET_ENVELOPEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_GET_EXTENT----------------------------------------------

$(OBJ_FILES)/MPI_TYPE_GET_EXTENTModule.o: \
    $(SRC_FILES)/MPI_TYPE_GET_EXTENTModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_TYPE_GET_EXTENTModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_TYPE_SIZE----------------------------------------------

$(OBJ_FILES)/MPI_TYPE_SIZEModule.o: \
    $(SRC_FILES)/MPI_TYPE_SIZEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_TYPE_SIZEModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_WAIT----------------------------------------------

$(OBJ_FILES)/MPI_WAITModule.o: \
    $(SRC_FILES)/MPI_WAITModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_WAITModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_WTICK----------------------------------------------

$(OBJ_FILES)/MPI_WTICKModule.o: \
    $(SRC_FILES)/MPI_WTICKModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_WTICKModule.f90
	$(MV_OBJ); $(MV_MOD)

#----------------------MPI_WTIME----------------------------------------------

$(OBJ_FILES)/MPI_WTIMEModule.o: \
    $(SRC_FILES)/MPI_WTIMEModule.f90 \
    $(OBJ_FILES)/IncludeMPIImplementation.o 
	$(F95) $(F95FLAGS) $(INCLUDE_MODS) -I$(MPI_INCLUDE_LOCATION) $(SRC_FILES)/MPI_WTIMEModule.f90
	$(MV_OBJ); $(MV_MOD)

