#!/usr/bin/env python3
from plotReportLib import myfunctions as fcn
from my_plot import set_size

NumericalDirectory = '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/SecondOrderDiff/256pts/'
NumericalWaveNumberData = fcn.importPlotData(NumericalDirectory + 'gam.nonconv_acc.0256')


