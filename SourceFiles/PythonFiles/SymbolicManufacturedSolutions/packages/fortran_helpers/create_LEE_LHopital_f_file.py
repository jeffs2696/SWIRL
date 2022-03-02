#!/usr/bin/env python
# coding: utf-8
import re
import sympy as sp
def LEE_LH_f_file(S,S_at_r):
    S_1 = S[0]
    S_2 = S[1]
    S_3 = S[2]
    S_4 = S[3]


    S_at_r_1 = S_at_r[0] 
    S_at_r_2 = S_at_r[1] 
    S_at_r_3 = S_at_r[2] 
    S_at_r_4 = S_at_r[2] 

    fS_1 = sp.fcode(S_1,source_format='free',standard=95)
    fS_2 = sp.fcode(S_2,source_format='free',standard=95)
    fS_3 = sp.fcode(S_3,source_format='free',standard=95)
    fS_4 = sp.fcode(S_4,source_format='free',standard=95)

    fS_at_r_1 = sp.fcode(S_at_r_1,source_format='free',standard=95)
    fS_at_r_2 = sp.fcode(S_at_r_2,source_format='free',standard=95)
    fS_at_r_3 = sp.fcode(S_at_r_3,source_format='free',standard=95)
    fS_at_r_4 = sp.fcode(S_at_r_4,source_format='free',standard=95)
    
    fS_1 = "S_1(jj) = " + re.sub(r"\b[r]\b","r(jj)",fS_1) + "\n"
    fS_2 = "S_2(jj) = " + re.sub(r"\b[r]\b","r(jj)",fS_2) + "\n"
    fS_3 = "S_3(jj) = " + re.sub(r"\b[r]\b","r(jj)",fS_3) + "\n"
    fS_4 = "S_4(jj) = " + re.sub(r"\b[r]\b","r(jj)",fS_4) + "\n"

    fS_at_r_1 = "S_1(1) = " + re.sub(r"\b[r]\b","r(1)",fS_at_r_1) + "\n"
    fS_at_r_2 = "S_2(1) = " + re.sub(r"\b[r]\b","r(1)",fS_at_r_2) + "\n"
    fS_at_r_3 = "S_3(1) = " + re.sub(r"\b[r]\b","r(1)",fS_at_r_3) + "\n"
    fS_at_r_4 = "S_4(1) = " + re.sub(r"\b[r]\b","r(1)",fS_at_r_4) + "\n"

#    fS_2 = re.sub(r"\*m\*","*mC*",fS_2)
#    fS_3 = re.sub(r"\*m\*","*mC*",fS_3)
#    fS_4 = re.sub(r"\*m\*","*mC*",fS_4)

#    fS_1 = re.sub(r"kappa","kappaC",fS_1)
#    fS_2 = re.sub(r"kappa","kappaC",fS_2)
#    fS_3 = re.sub(r"kappa","kappaC",fS_3)
#    fS_4 = re.sub(r"kappa","kappaC",fS_4)

    S_list = []
    S_at_r_list = []

    f_code_header1 = ''' 
    ! gam - axial wavenumber t
    ! ak  - reduced frequency
    ! kappa - ratio of specific heats
    ! i - imaginary number

    SUBROUTINE SourceCalc(& 
    r    , &
    S_1  , &
    S_2  , &
    S_3  , &
    S_4)

    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: r
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: S_1, S_2, S_3, S_4 

    ! Local variables
    COMPLEX(KIND=rDef) :: i   
    INTEGER :: jj, numberOfGridPoints 
    i = CMPLX(0.0, 1.0,KIND= rDef) 
    numberOfGridPoints = SIZE(r)

    DO jj = 2,numberOfGridPoints
    '''

    S_list.append(fS_1)
    S_list.append(fS_2)
    S_list.append(fS_3)
    S_list.append(fS_4)
    S_list = ''.join(S_list)
    f_code_footer2 = '''
    END DO
    ''' 

    f_code_footer_3 = '''

    '''


    S_at_r_list.append(fS_at_r_1)
    S_at_r_list.append(fS_at_r_2)
    S_at_r_list.append(fS_at_r_3)
    S_at_r_list.append(fS_at_r_4)
    S_at_r_list = ''.join(S_at_r_list)

    f_code_footer_4 ='''
    
    END SUBROUTINE SourceCalc
    '''
    #print(fS_1)
    #print(fS_2)
    #print(fS_3)
    #print(fS_4)
    
    #print(S_1)
    #print(S_2)
    #print(S_3)
    #print(S_4)
    with open('../../FortranFiles/SourceTermMMS.f90','w') as f:
        f.write(f_code_header1)
        f.write(S_list)
        f.write(f_code_footer2)
        f.write(f_code_footer_3)
        f.write(S_at_r_list)
        f.write(f_code_footer_4) 
        
        
     
