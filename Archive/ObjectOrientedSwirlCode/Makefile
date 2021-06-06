SOURCE_FILES = ../SourceFiles
SOURCE_LOCATION = $(SOURCE_FILES)

SRC_OBJ     = srcObj 
SRC_ORIG    = srcF2008 
DIR_OBJ     = CodeRunObj
DIR_ORIG    = CodeRunOrig
DIR_COMPARE = SwirlComparison/L2NormComparison/
EXE = swirlF2008
MOVE_OUTPUT_EXE = moveGammasOnly.sh
COMPARE_EXE = outt
all:
	(cd $(SRC_OBJ); make)
	(cd $(SRC_ORIG); make)
	(cd $(DIR_OBJ);  ./$(EXE); ./$(MOVE_OUTPUT_EXE) )
	(cd $(DIR_ORIG); ./$(EXE); ./$(MOVE_OUTPUT_EXE) )
	(cd $(DIR_COMPARE); ./$(COMPARE_EXE))
