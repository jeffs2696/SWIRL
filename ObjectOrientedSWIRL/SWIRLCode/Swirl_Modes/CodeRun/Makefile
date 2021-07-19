OUTPUT_FILES = *.dat gam.acc
EXE = swirlF2020

run:
	./$(EXE)
clean:
	rm $(OUTPUT_FILES)
plot:
	(./plotMachDistribution.sh ./plotROC.sh ./plotL2OfSoundSpeed.sh);
	pdflatex test.tex;
	xdg-open test.pdf
