OUTPUT_FILES = \
			   01-mean-flow/*.dat \
			   03-EVanalysis/egv* \
			   03-EVanalysis/gam*

PLOT_REPORT_FILES = \
			   04-plotReport/tex-outputs/*.tex

MMS_OUTPUT_FILES = \
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

# NUMBERS =  

# all: 
	# echo $(PLOT_EXE)

run: 
	(./$(EXE));

plot:
	@cd 04-plotReport; ./plotReport.py;


clean:
	(rm $(OUTPUT_FILES));
report: all
	@cd 04-plotReport; ./plotReport.py; \
		pdflatex -halt-on-error plotReport.tex | grep -A3 '^!'; \
		xdg-open plotReport.pdf;
Kousen:
	@cd 04-plotReport; ./KousensTestCasePlots.py;
.PHONY: all
# 	@cd $(PLOT_SHELL_SCRIPT_DIR); \
# 	$(foreach var,$(PLOT_EXE),./$(var);)
# 	@cd plottingScripts; mv *.tex ../plotReport/ ;
