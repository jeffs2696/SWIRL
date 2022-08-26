all:
	@cd SourceFiles;  make --no-print-directory;
clean:
	(cd SourceFiles; make clean)
plot:
	(cd CodeRun; make plot)
MMS:
	cd SourceFiles/PythonFiles/SymbolicManufacturedSolutions && make
Thesis:
	cd Documentation/SeverinoWork/Thesis && make
