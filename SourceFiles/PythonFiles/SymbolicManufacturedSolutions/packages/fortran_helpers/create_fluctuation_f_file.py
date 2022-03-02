#!/usr/bin/python

import sympy as sp
import re
def fluctuation_fortran_file(v_r_analytic,v_t_analytic,v_x_analytic,p_analytic):
    fv_r  = sp.fcode(v_r_analytic,source_format='free',standard=95)
    fv_th = sp.fcode(v_t_analytic,source_format='free',standard=95)
    fv_x  = sp.fcode(v_x_analytic,source_format='free',standard=95)
    fp    = sp.fcode(p_analytic,source_format='free',standard=95)  
    
    fv_r  = "vR(i) =" + re.sub(r"\b[r]\b","r(i) ",fv_r) + "\n"
    fv_th = "vTh(i)=" + re.sub(r"\b[r]\b","r(i) ",fv_th)+ "\n"
    fv_x  = "vX(i) =" + re.sub(r"\b[r]\b","r(i) ",fv_x) + "\n"
    fp    = "Pr(i) =" + re.sub(r"\b[r]\b","r(i) ",fp)   + "\n"
    
    
    
    S_list = []
    S_list.append(fv_r)
    S_list.append(fv_th)
    S_list.append(fv_x)
    S_list.append(fp)
    S_list = ''.join(S_list)
    
    
    f_code_header3 = ''' 
    
     
        SUBROUTINE CalcPerturbationVariables(& 
        r    , &
        vR   , &
        vTh  , &
        vX   , &
        Pr)
        
    
        REAL(KIND=rDef)  ,DIMENSION(:), INTENT(IN)    :: r
        REAL(KIND=rDef)  ,DIMENSION(:), INTENT(INOUT) :: vR, vTh, vX, Pr
    
        
        ! Local variables
        INTEGER :: numberOfGridPoints, i
        
        numberOfGridPoints = SIZE(vR)
          DO i = 1, numberOfGridPoints
    '''
    
    f_code_footer3 = '''
          END DO
        END SUBROUTINE CalcPerturbationVariables
    '''
    
    with open('../../FortranFiles/CalcPerturbationVariablesMMS.f90','w') as f:
        f.write(f_code_header3)
        f.write(S_list)
        f.write(f_code_footer3)
    
