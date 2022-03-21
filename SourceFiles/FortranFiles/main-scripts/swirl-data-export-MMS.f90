! Exporting the results from the Method of Manufactured Solutions

!dir_name = '02-method-of-manufactured-solutions/'
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'L2-sound_speed-'        // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_L2_HEADER) 'GridPoints ' , 'L2'
!
!DO i = 1,numberOfIterations
!    WRITE(UNIT,FORMAT_L2) numberOfGridPointsArray(i) !, SoundSpeedL2Array(i)
!END DO
!
!CLOSE(UNIT);
!
!
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'ROC-sound_speed'      // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_ROC_HEADER) 'Delta_r' , 'ROC'
!
!DO i = 1,numberOfIterations - 1
!    WRITE(UNIT,FORMAT_ROC)  &
!        numberOfGridPointsArray(i), &
!        RateOfConvergence1(i) 
!ENDDO
!
!CLOSE(UNIT);
!
!! Linearized Perturbation Equations
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'L2-LEE'      // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_L2_HEADER) 'GridPoints' , 'L2'
!
!DO i = 1,numberOfIterations
!    WRITE(UNIT,FORMAT_L2) &
!        numberOfGridPointsArray(i) , &
!        REAL(S_L2Array(i),KIND=rDef)
!END DO
!
!CLOSE(UNIT);
!
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'ROC-LEE'      // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_ROC_HEADER) 'Delta_r ' , 'ROC'
!
!DO i = 1,numberOfIterations - 1
!
!    WRITE(UNIT,FORMAT_ROC) &
!        numberOfGridPointsArray(i), &
!        ABS(REAL(RateOfConvergence2(i),KIND=rDef))
!
!ENDDO
!
!CLOSE(UNIT)
!
!IF (debug) THEN
!WRITE(0,FORMAT_L2_HEADER) 'Gridpoints' , 'L2-SoundSpeed'
!
!DO i = 1,numberOfIterations
!    WRITE(0,FORMAT_L2) 5+2**i !, SoundSpeedL2Array(i)
!END DO
!
!WRITE(0,FORMAT_L2_HEADER) 'Gridpoints' , 'L2-Source'
!
!DO i = 1,numberOfIterations
!    WRITE(0,FORMAT_L2) 5+2**i , REAL(S_L2Array(i),KIND=rDef)
!END DO
!
!WRITE(0,FORMAT_ROC_HEADER) 'Delta_r' , 'ROC'
!DO i = 1,numberOfIterations - 1
!    WRITE(0,FORMAT_ROC) &
!        numberOfGridPointsArray(i), &
!        RateOfConvergence1(i)
!ENDDO
!
!WRITE(0,FORMAT_ROC_HEADER) 'Delta_r' , 'ROC'
!
!DO i = 1,numberOfIterations - 1
!    WRITE(0,FORMAT_ROC) &
!        numberOfGridPointsArray(i), & 
!        RateOfConvergence2(i)
!ENDDO
!ELSE
!END IF
!
