import pandas
from plotReportLib import myfunctions
def test_import():
    assert type(myfunctions.importPlotData('test_valid_import.dat')) == pandas.DataFrame
    
