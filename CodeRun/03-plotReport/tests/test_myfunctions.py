import pandas
from plotReportLib import myfunctions
def test_import_valid():
    assert type(myfunctions.importPlotData('test_valid_import.dat')) == pandas.DataFrame
    
def test_import_invalid():
    assert type(myfunctions.importPlotData('test_invalid.dat')) == pandas.DataFrame
    
