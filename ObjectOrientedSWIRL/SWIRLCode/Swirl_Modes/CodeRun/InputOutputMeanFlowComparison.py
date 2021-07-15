
# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import pygal       
import pandas as pd    # Used to import data

filename  = 'MeanFlowData'
extension = '.dat'


for i in range(5,5):
    fac = 1+2**i
    print(fac)
    
#   Using pandas read_csv method, we import the data

# sep - defines what will be used as the seperated value
# header - defines whether there is a header or not

    FlowData = pd.read_csv(filename + str(fac) + extension ,
                           sep=r"\s+", # use one or more space 
                           header=None)
    
    # Checking if the data was imported correctly
    print(FlowData.head())
    print(FlowData)
    
    # Saving Columns 
    Radius = FlowData[0]
    AxialMachData      = FlowData[1]
    ThetaMachData      = FlowData[2]
    SoundSpeedExpected = FlowData[3]
    SoundSpeedOut      = FlowData[4]
    
    
    line_chart = pygal.Line()
    line_chart.add = ('M_theta',ThetaMachData)
    line_chart.add = ('M_x'    ,AxialMachData)
    line_chart.render_to_file("test.svg")    
    # line_chart.title = 'Browser usage evolution (in %)'
    # line_chart.x_labels = map(str, range(2002, 2013))
    # line_chart.add('',Radius)
    
    # line_chart.add('',SoundSpeedExpected-SoundSpeedOut)
    # line_chart.add('',SoundSpeedOut)# line_chart.add('Firefox', [None, None,    0, 16.6,   25,   31, 36.4, 45.5, 46.3, 42.8, 37.1])
    # line_chart.add('Chrome',  [None, None, None, None, None, None,    0,  3.9, 10.8, 23.8, 35.3])
    # line_chart.add('IE',      [85.8, 84.6, 84.7, 74.5,   66, 58.6, 54.7, 44.8, 36.2, 26.6, 20.1])
    # line_chart.add('Others',  [14.2, 15.4, 15.3,  8.9,    9, 10.4,  8.9,  5.8,  6.7,  6.8,  7.5])
    # line_chart.render_to_file("SoundSpeedComparison" +str(fac) + ".svg")
                         
 