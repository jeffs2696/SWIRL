#!/usr/bin/python3
import sympy as sp
import re

def SoundSpeedFortranFile(A_analytic,M_t_analytic,M_x_analytic):
    ## Creating files for SourceTermModule.f95
    
    # Using the expression for the tangential Mach number, M_t above ...
    f_A   = sp.fcode(A_analytic  ,source_format='free',standard=95)
    f_M_t = sp.fcode(M_t_analytic,source_format='free',standard=95)
    f_M_x = sp.fcode(M_x_analytic,source_format='free',standard=95)

    f_A   = "        SoundSpeedExpected(i) = " + f_A +"\n"
    f_M_t = "        thetaMachData(i)      = " + f_M_t + "\n"
    f_M_x = "        axialMachData(i)      = " + f_M_x + "\n"

    f_A = re.sub(r"r ","r(i)",f_A)
    f_A = re.sub(r"r\*","r(i)*",f_A)
    f_M_t = re.sub(r"r ","r(i)",f_M_t)
    f_M_t = re.sub(r"r\*","r(i)*",f_M_t)
    f_M_x = re.sub(r"r ","r(i)",f_M_x)
    f_M_x = re.sub(r"r\*","r(i)*",f_M_x)

    S_list = []
    S_list.append(f_A)
    S_list.append(f_M_t)
    S_list.append(f_M_x)
    S_list = ''.join(S_list)
    f_code_header1 = ''' 
    ! Returns M_theta and the corresponding sound speed as defined in
    ! SourceTermSymbolicSolver.ipynb
    
    SUBROUTINE CalcSoundSpeed(& 
    r                 , &
    kappa          , &
    SoundSpeedExpected, &
    thetaMachData     , &
    axialMachData)
           
    REAL(KIND=rDef)   , INTENT(IN) :: &
    kappa 
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData, axialMachData
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: &
    r
    
    
    ! Local variables 
    INTEGER :: &
    numberOfGridPoints, i!, j
    
    numberOfGridPoints = SIZE(SoundSpeedExpected)
    
    DO i = 1,numberOfGridPoints
    
    '''   
    
    f_code_footer1 = '''
    
    END DO
    
    END SUBROUTINE CalcSoundSpeed
    '''
    with open('../../FortranFiles/SoundSpeedMMS.f90','w') as f:
        f.write(f_code_header1)
        f.write(S_list)
        f.write(f_code_footer1) 
        return
