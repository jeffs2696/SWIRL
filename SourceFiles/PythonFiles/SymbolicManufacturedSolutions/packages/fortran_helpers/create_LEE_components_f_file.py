#!/usr/bin/env python
# coding: utf-8
import re
import sympy as sp
# In[ ]:
def LEE_components_f_file(A_times_x,lambda_B_times_x): 
    fS_A = []
    fS_B = []
    lhs_A = []
    lhs_B = []
    S_list = []
    for i in range(len(A_times_x[:,0])):
        for j in range(len(A_times_x[0,:])):
            fS_A.append(
                sp.fcode(
                    A_times_x[i,j],
                    source_format='free',
                    standard =95 ,
               ),) 
            lhs_A.append('S_A'+str(i+1)+str(j+1))
            
            fS_B.append(
                sp.fcode(
                    lambda_B_times_x[i,j],
                    source_format='free',
                    standard =95 ,
                ) , )
            
            lhs_B.append('S_B'+str(i+1)+str(j+1))


    for i in range(len(fS_B)): 
        fS_A[i] = re.sub(r"gamma",'gam'     , fS_A[i])
        fS_A[i] = re.sub(r"\*m\*","*mC*"     , fS_A[i])
        fS_A[i] = re.sub(r"kappa","kappaC" , fS_A[i])
        fS_A[i] = re.sub(r"r_max","r_maxC" , fS_A[i])
        
        fS_B[i] = re.sub(r"gamma",'gam'     , fS_B[i])
        fS_B[i] = re.sub(r"\*m\*","*mC*"     , fS_B[i])
        fS_B[i] = re.sub(r"kappa","kappaC" , fS_B[i])
        fS_B[i] = re.sub(r"r_max","r_maxC" , fS_B[i])
        
    for i in range(len(fS_A)):
        S_list.append(lhs_A[i] + '=' + fS_A[i] + "\n")        
    for i in range(len(fS_B)):
        S_list.append(lhs_B[i] + '=' + fS_B[i] + "\n")        
    
    #S_list.append('if (r .ne. 1.0_rDef)')
    #S_list.append(fcode(\
    #                    A_times_x[0,3], \
    #                    source_format='free', \
    #                    standard =95 , \
    #                   ) ,) 
    #S_list.append('S_A'+str(1)+str(4))
    #S_list.append(fcode(\
    #                    A_times_x[3,0], \
    #                    source_format='free', \
    #                    standard =95 , \
    #                   )  , )
    #S_list.append('S_A'+str(4)+str(1))
    #            
    #S_list.append('else ')
    #S_list.append('S_A14 = CMPLX(0.0_rDef,0.0_rDef,KIND=rDef)')
    #S_list.append('S_A41 = CMPLX(0.0_rDef,0.0_rDef,KIND=rDef)')
    #S_list.append('endif')
    S_list = ''.join(S_list)
    
    f_code_header2 = ''' 
    ! gam - axial wavenumber t
    ! ak  - reduced frequency
    ! kappa - ratio of specific heats
    ! i - imaginary number
     
        SUBROUTINE SourceCalcComponents(& 
        gam  , &
        i    , &
        ak   , &
        kappa, &
        m    , & 
        r    , &
        r_max, &
        S_1  , &
        S_2  , &
        S_3  , &
        S_4  , &
        S_A11, &
        S_A12, &
        S_A13, &
        S_A14, &
        S_A21, &
        S_A22, &
        S_A23, &
        S_A24, &
        S_A31, &
        S_A32, &
        S_A33, &
        S_A34, &
        S_A41, &
        S_A42, &
        S_A43, &
        S_A44, &
        S_B11, &
        S_B12, &
        S_B13, &
        S_B14, &
        S_B21, &
        S_B22, &
        S_B23, &
        S_B24, &
        S_B31, &
        S_B32, &
        S_B33, &
        S_B34, &
        S_B41, &
        S_B42, &
        S_B43, &
        S_B44 )
        
        INTEGER, INTENT(IN) :: m
        REAL(KIND=rDef)   , INTENT(IN) :: kappa,r,r_max
        !REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: r_loc
        COMPLEX(KIND=rDef), INTENT(IN) :: i, gam, ak           
        COMPLEX(KIND=rDef), INTENT(INOUT) :: &
        S_1,S_2,S_3,S_4, &
        S_A11, &
        S_A12, &
        S_A13, &
        S_A14, &
        S_A21, &
        S_A22, &
        S_A23, &
        S_A24, &
        S_A31, &
        S_A32, &
        S_A33, &
        S_A34, &
        S_A41, &
        S_A42, &
        S_A43, &
        S_A44, &
        S_B11, &
        S_B12, &
        S_B13, &
        S_B14, &
        S_B21, &
        S_B22, &
        S_B23, &
        S_B24, &
        S_B31, &
        S_B32, &
        S_B33, &
        S_B34, &
        S_B41, &
        S_B42, &
        S_B43, &
        S_B44
        
    
        
        ! Local variables
        COMPLEX(KIND=rDef) :: mC, kappaC, rC, r_maxC
        
        REAL(KIND=rDef) :: one,two,three
    
        one = REAL(1.0,KIND=rDef)    
        two = REAL(2.0,KIND=rDef)    
        three = REAL(3.0,KIND=rDef)
    
        mC = CMPLX(m,KIND=rDef)
        kappaC = CMPLX(kappa,KIND=rDef)
        rC = CMPLX(r,KIND=rDef)
        r_maxC = CMPLX(r_max,KIND=rDef)
    '''
    
    
    f_code_footer2 = '''
    
    
        S_1 = &
        S_A11 +&
        S_A12 +&
        S_A13 +&
        S_A14 -(&
        S_B11 + &
        S_B12 + &
        S_B13 + &
        S_B14)
        
        S_2 = &
        S_A21 +&
        S_A22 +&
        S_A23 +&
        S_A24 -(&
        S_B21 + &
        S_B22 + &
        S_B23 + &
        S_B24)
        
        S_3 = &
        S_A31 +&
        S_A32 +&
        S_A33 +&
        S_A34 -(&
        S_B31 + &
        S_B32 + &
        S_B33 + &
        S_B34)
        
      S_4 = &
        S_A41 +&
        S_A42 +&
        S_A43 +&
        S_A44 -(&
        S_B41 + &
        S_B42 + &
        S_B43 + &
        S_B44)
        END SUBROUTINE SourceCalcComponents
    '''
    #print(fS_1)
    #print(fS_2)
    #print(fS_3)
    #print(fS_4)
    
    #print(S_1)
    #print(S_2)
    #print(S_3)
    #print(S_4)
    
    
    with open('../../FortranFiles/SourceTermComponentsMMS.f90','w') as f:
        f.write(f_code_header2)
        f.write(S_list)
        f.write(f_code_footer2)
        
        
        
    
    