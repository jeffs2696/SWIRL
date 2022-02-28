#!/usr/bin/env python
# coding: utf-8
import re
import sympy as sp
def LEE_f_file(
        S_1,
        S_2,
        S_3,
        S_4):

    fS_1 = sp.fcode(S_1,source_format='free',standard=95)
    fS_2 = sp.fcode(S_2,source_format='free',standard=95)
    fS_3 = sp.fcode(S_3,source_format='free',standard=95)
    fS_4 = sp.fcode(S_4,source_format='free',standard=95)

    fS_1 = "S_1(jj) = " + re.sub(r"gamma",'gam',fS_1) + "\n"
    fS_2 = "S_2(jj) = " + re.sub(r"gamma",'gam',fS_2) + "\n"
    fS_3 = "S_3(jj) = " + re.sub(r"gamma",'gam',fS_3) + "\n"
    fS_4 = "S_4(jj) = " + re.sub(r"gamma",'gam',fS_4) + "\n"

    fS_1 = re.sub(r"\b[r]\b","r(jj)",fS_1)
    fS_2 = re.sub(r"\b[r]\b","r(jj)",fS_2)
    fS_3 = re.sub(r"\b[r]\b","r(jj)",fS_3)
    fS_4 = re.sub(r"\b[r]\b","r(jj)",fS_4)
#    fS_1 = re.sub(r"\*m\*","*mC*",fS_1)
#    fS_2 = re.sub(r"\*m\*","*mC*",fS_2)
#    fS_3 = re.sub(r"\*m\*","*mC*",fS_3)
#    fS_4 = re.sub(r"\*m\*","*mC*",fS_4)

#    fS_1 = re.sub(r"kappa","kappaC",fS_1)
#    fS_2 = re.sub(r"kappa","kappaC",fS_2)
#    fS_3 = re.sub(r"kappa","kappaC",fS_3)
#    fS_4 = re.sub(r"kappa","kappaC",fS_4)

    S_list = []
    S_list.append(fS_1)
    S_list.append(fS_2)
    S_list.append(fS_3)
    S_list.append(fS_4)
    S_list = ''.join(S_list)

    f_code_header2 = ''' 
    ! gam - axial wavenumber t
    ! ak  - reduced frequency
    ! kappa - ratio of specific heats
    ! i - imaginary number
    
    SUBROUTINE SourceCalc(& 
    gam  , &
    m    , & 
    r    , &
    S_1  , &
    S_2  , &
    S_3  , &
    S_4)
    
    INTEGER, INTENT(IN) :: m
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: r
    COMPLEX(KIND=rDef), INTENT(IN) ::  gam
    !COMPLEX(KIND=rDef), INTENT(INOUT) :: S_1, S_2, S_3, S_4
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: S_1, S_2, S_3, S_4
    
    ! Local variables
    COMPLEX(KIND=rDef) :: i   
    ! , rC,
    INTEGER :: jj, numberOfGridPoints
    i = CMPLX(0.0, 1.0,KIND= rDef)
    
    
    numberOfGridPoints = SIZE(r)
    DO jj = 1,numberOfGridPoints
    '''
    
    f_code_footer2 = '''
    END DO
    
    
    
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
        f.write(f_code_header2)
        f.write(S_list)
        f.write(f_code_footer2)
        
        
        
     
