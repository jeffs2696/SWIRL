all:
	(cd SourceFiles; make)
clean:
	(cd SourceFiles; make clean)
plot:
	(cd CodeRun; make plot)
MMS:
	cd SourceFiles/PythonFiles/SymbolicManufacturedSolutions && make
