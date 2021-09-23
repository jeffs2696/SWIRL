dir_name = '03-method-of-manufactured-solutions/'
! file_name = TRIM(dir_name) // 'sound-speed-L2.dat'

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'sound-speed-L2-'      // &
    TRIM(ADJUSTL(file_id))   // &
    '.dat')

WRITE(UNIT,*) 'Grid Points' , 'L2 of Speed of Sound'

DO i = 1,numberOfIterations
    WRITE(UNIT,*) 1+2**i , SoundSpeedL2Array(i)
END DO

CLOSE(UNIT);



OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'sound-speed-ROC'      // &
    '.dat')

WRITE(0,FORMAT_ROC_HEADERS) 'Delta r' , 'ROC'

DO i = 1,numberOfIterations - 1
    WRITE(UNIT,FORMAT_ROC)  REAL(1+2**(i),KIND=rDef)/REAL(1+2**(i+1),KIND=rDef), &
        RateOfConvergence1(i) 
ENDDO

CLOSE(UNIT);

! Linearized Perturbation Equations

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'LEE-source-term-L2'      // &
    '.dat')

WRITE(UNIT,*) 'Grid Points' , 'L2 of Source Term'

DO i = 1,numberOfIterations
    WRITE(UNIT,*) 1+2**i , REAL(S_L2Array(i),KIND=rDef)
END DO

CLOSE(UNIT);

! file_name = TRIM(dir_name) // 'LPE-source-term-ROC.dat'

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'LEE-source-term-ROC'      // &
    '.dat')

! OPEN(NEWUNIT=UNIT,FILE=(file_name))

WRITE(UNIT,FORMAT_ROC_HEADERS) 'Delta r' , 'ROC'

DO i = 1,numberOfIterations - 1

     WRITE(UNIT,FORMAT_ROC) &
         REAL(1+2**(i),KIND=rDef)/REAL(1+2**(i+1),KIND=rDef), &
         ABS(REAL(RateOfConvergence2(i),KIND=rDef))

ENDDO

CLOSE(UNIT)

! ! IF (debug) THEN
     WRITE(0,*) 'Grid Points' , 'L2 of Speed of Sound'

     DO i = 1,numberOfIterations
         WRITE(0,*) 1+2**i , SoundSpeedL2Array(i)
     END DO

     WRITE(0,*) 'Grid Points' , 'L2 of Source Term'

     DO i = 1,numberOfIterations
         WRITE(0,*) 1+2**i , REAL(S_L2Array(i),KIND=rDef)
     END DO

     WRITE(0,FORMAT_ROC_HEADERS) 'Delta r' , 'ROC'
     DO i = 1,numberOfIterations - 1
         WRITE(0,FORMAT_ROC) &
             REAL(1+2**(i),KIND=rDef)/REAL(1+2**(i+1),KIND=rDef), &
             RateOfConvergence1(i)
     ENDDO

     WRITE(0,FORMAT_ROC_HEADERS) 'Delta r' , 'ROC'

     DO i = 1,numberOfIterations - 1
         WRITE(0,FORMAT_ROC) &
             REAL(1+2**(i),KIND=rDef)/REAL(1+2**(i+1),KIND=rDef), &
             RateOfConvergence2(i)
     ENDDO
! ! ELSE
! ! END IF

