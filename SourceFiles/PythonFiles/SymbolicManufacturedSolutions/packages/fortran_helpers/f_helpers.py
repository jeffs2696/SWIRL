import re
# needs to get added to helper
def GetInputVariables(string):
    '''opening the file that has the inputs to the FORTRAN Code
    '''
    InputFile = open("../../FortranFiles/main-scripts/main-variables.f90", "r")

    flag = 0
    index = 0 
    
    for line in InputFile:
        index += 1
        
        # checking if the string is present in line or not
        if string in line:
            result = line
            flag = 1  
            break
    # checking condition for string found or not, uncomment else when debugging
    if flag == 0:
        print('String', string, 'Not Found')
    #else:
        #print('String', string, 'Found In Line', index)
        
    # using re.findall()
    # getting numbers from string 
    result = re.findall(r'\d*\.?\d+', result)
    
    return result

