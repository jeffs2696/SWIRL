OUTPUT_FILES = \
			   *.dat \
			   01-mean-flow/*.dat \
			   02-method-of-manufactured-solutions/*.dat
EXE = swirlF2020
SHELL_EXT = sh
PLOT_GNUPLOT_DIR = plottingGNUconfig
PLOT_SHELL_SCRIPT_DIR = plottingScripts
PLOT_REPORT_DIR = plotReport
PLOT_NAMES = plotMachDistribution 	\
			 plotL2OfSoundSpeed 	\
			 plotL2OfSourceTerm 	\
			 plotROC 				\
			 plotROC_Source 		\
			 plotSoundSpeed         \
			 plotSourceTermData     \
			 plotSoundSpeedError


PLOT_EXE = $(PLOT_NAMES:=.$(SHELL_EXT))

NUMBERS =  

# all: 
	# echo $(PLOT_EXE)

run SWIRL:
	./$(EXE)
clean:
	(rm $(OUTPUT_FILES); rm *.log);
plot: all
	./PythonPlotReport.py; \
		cd 03-plotReport;\
		pdflatex -halt-on-error plotReport.tex | grep -A3 '^!'; \
		xdg-open plotReport.pdf;

.PHONY: all
# 	@cd $(PLOT_SHELL_SCRIPT_DIR); \
# 	$(foreach var,$(PLOT_EXE),./$(var);)
# 	@cd plottingScripts; mv *.tex ../plotReport/ ;
