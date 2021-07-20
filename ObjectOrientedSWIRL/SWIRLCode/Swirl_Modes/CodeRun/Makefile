OUTPUT_FILES = *.dat gam.acc
EXE = swirlF2020
SHELL_EXT = sh
PLOT_GNUPLOT_DIR = plottingGNUconfig
PLOT_SHELL_SCRIPT_DIR = plottingScripts
PLOT_REPORT_DIR = plotReport
PLOT_NAMES = plotMachDistribution 	\
			 plotL2OfSoundSpeed 	\
			 plotROC 				\
			 plotSoundSpeed


PLOT_EXE = $(PLOT_NAMES:=.$(SHELL_EXT))

NUMBERS =  

# all: 
	# echo $(PLOT_EXE)

run SWIRL:
	./$(EXE)
clean:
	rm $(OUTPUT_FILES)
plot: all
	cd $(PLOT_SHELL_SCRIPT_DIR); \
	$(foreach var,$(PLOT_EXE),./$(var);)
	cd plottingScripts; mv *.tex ../plotReport/ ;
	cd plotReport; \
	pdflatex plotReport.tex; \
	xdg-open plotReport.pdf

.PHONY: all
