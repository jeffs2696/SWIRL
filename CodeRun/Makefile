OUTPUT_FILES = \
			   01-mean-flow/*.dat \
			   02-method-of-manufactured-solutions/*.dat \
			   03-plotReport/tex-outputs/*.tex \
			   04-EVanalysis/*.dat \
			   04-EVanalysis/egv* \
			   04-EVanalysis/gam* \
			   fort.*

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

plot:
	@cd 03-plotReport; ./PythonPlotReport.py;

run SWIRL:
	./$(EXE)
clean:
	(rm $(OUTPUT_FILES));
report: all
	@cd 03-plotReport; ./PythonPlotReport.py; \
		pdflatex -halt-on-error plotReport.tex | grep -A3 '^!'; \
		xdg-open plotReport.pdf;

.PHONY: all
# 	@cd $(PLOT_SHELL_SCRIPT_DIR); \
# 	$(foreach var,$(PLOT_EXE),./$(var);)
# 	@cd plottingScripts; mv *.tex ../plotReport/ ;
