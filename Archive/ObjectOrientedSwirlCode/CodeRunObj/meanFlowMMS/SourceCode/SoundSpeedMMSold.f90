PROGRAM MAIN  

    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64

    INTEGER :: i, numberOfGridPoints
    REAL(KIND=rDef) :: errorSum1,&
        errorSum2,&
        errorSum3,&
        errorSum4,&
        L2_1     ,&
        L2_2     ,&
        L2_3     ,&
        L2_4
    REAL(KIND=rDef), DIMENSION(3) :: error1
    REAL(KIND=rDef), DIMENSION(5) :: error2
    REAL(KIND=rDef), DIMENSION(9) :: error3
    REAL(KIND=rDef), DIMENSION(17) :: error4
    REAL(KIND=rDef), DIMENSION(1) :: alpha
    CHARACTER(100) :: file_place
    

    WRITE(file_place,*) "/home/jeff-severino/SWIRL/ObjectOrientedSwirlCode/CodeRunObj/"
    OPEN(UNIT = 266, file=TRIM(ADJUSTL(file_place))//"SoundSpeedMMS3.dat")
    OPEN(UNIT = 267, file=TRIM(ADJUSTL(file_place))//"SoundSpeedMMS5.dat")
    OPEN(UNIT = 268, file=TRIM(ADJUSTL(file_place))//"SoundSpeedMMS9.dat")
    OPEN(UNIT = 269, file=TRIM(ADJUSTL(file_place))//"SoundSpeedMMS17.dat")
    
    errorSum1 = 0.0_rDef
    errorSum2 = 0.0_rDef
    errorSum3 = 0.0_rDef
    errorSum4 = 0.0_rDef

    READ(266,*) numberOfGridPoints
    DO i = 1,numberOfGridPoints
      READ(266,*) error1(i+1)
      WRITE(6 ,*) error1(i)
      errorSum1 = errorSum1 + error1(i+1)
      
    ENDDO
    
    L2_1 = ((errorSum1**2.0_rDef)/numberOfGridPoints)**0.5_rDef
    WRITE(6,*) errorSum1, L2_1
    CLOSE(266)
    
    READ(267,*) numberOfGridPoints
    DO i = 1,numberOfGridPoints
      READ(267,*) error2(i)
      errorSum2 = errorSum2 + error2(i)
    ENDDO
    L2_2 = ((errorSum2**2.0_rDef)/numberOfGridPoints)**0.5_rDef
!    
    WRITE(6,*) errorSum2
    CLOSE(267)
    
    READ(268,*) numberOfGridPoints
    DO i = 1,numberOfGridPoints
      READ(268,*) error3(i)
      errorSum3 = errorSum3 + error3(i)
    ENDDO
!    
    L2_3 = ((errorSum3**2.0_rDef)/numberOfGridPoints)**0.5_rDef
    WRITE(6,*) errorSum3
    CLOSE(268)

    READ(269,*) numberOfGridPoints
    DO i = 1,numberOfGridPoints
      READ(269,*) error4(i)
      errorSum4 = errorSum4 + error4(i)
    ENDDO !    
    L2_4 = ((errorSum4**2.0_rDef)/numberOfGridPoints)**0.5_rDef
    WRITE(6,*) errorSum4
    CLOSE(269)
    alpha = (LOG(L2_4) - LOG(L2_3))/LOG(0.5_rDef)
    WRITE(6,*) alpha
END PROGRAM MAIN



