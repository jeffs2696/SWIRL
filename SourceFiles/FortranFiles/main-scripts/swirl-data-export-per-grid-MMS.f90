! used to export data for swirl

! 01-mean-flow-data
WRITE(file_id, '(i0.4)') numberOfGridPoints
WRITE(FDfac_id, '(i0)') FDfac
WRITE(numIter, '(i0)') numberOfIterations

dir_name  = '01-mean-flow/'

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'mean-flow' // &
    TRIM(ADJUSTL(file_id))   // &
    '_FDmethod'  // &
    TRIM(ADJUSTL(FDfac_id))// &
    '.dat')

!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'mean-flow-MMS' // &
!    TRIM(ADJUSTL(file_id))   // &
!    'finite-diff-method' // &
!    TRIM(ADJUSTL(FDfac_id)) // &
!    '.dat')
!
 ! Write the resulting mean flow
WRITE(UNIT,*) &
    'radius '    , &
    'M_x '       , &
    'M_theta '   , &
    'A_expected ', &
    'A_actual '  , &
    'vR '        , &
    'vTh '       , &
    'vX '        , &
    'Pr '        , &
    'S_1_e '     , &
    'S_2_e '     , &
    'S_3_e '     , &
    'S_4_e '     , &
    'S_1_a '     , &
    'S_2_a '     , &
    'S_3_a '     , &
    'S_4_a '     

DO i = 1,numberOfGridPoints

    WRITE(UNIT,*) &
        rOut(i)                             , &
        axialMachDataMMS(i)                 , &
        thetaMachDataMMSOut(i)                 , &
        speedOfSoundMMS(i)               , &
        SoundSpeedOut(i)                    , &
        vR(i)                               , &
        vTh(i)                              , &
        vX(i)                               , &
        Pr(i)                               , &
        REAL(S_MMS(i),KIND=rDef)       , &
        REAL(S_MMS(i+1*numberOfGridPoints),KIND=rDef)  , &
        REAL(S_MMS(i+2*numberOfGridPoints),KIND=rDef)  , &
        REAL(S_MMS(i+3*numberOfGridPoints),KIND=rDef)  , &
        REAL(S_actual(i),KIND=rDef)                       , &
        REAL(S_actual(i+1*numberOfGridPoints),KIND=rDef)  , &
        REAL(S_actual(i+2*numberOfGridPoints) ,KIND=rDef) , &
        REAL(S_actual(i+3*numberOfGridPoints) ,KIND=rDef) 

    IF (debug) THEN
       ! WRITE(0,FORMAT_MEAN_FLOW) &
       !     rOut(i)                 , &
       !     axialMachDataMMSOut(i)     , &
       !     thetaMachDataMMSOut(i)     , &
       !     speedOfSoundMMS(i)   , &
       !     SoundSpeedOut(i)
   ELSE
    ENDIF
ENDDO

CLOSE(UNIT);

! LEE

dir_name = '02-method-of-manufactured-solutions/'

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'perturbation-variables' // &
    TRIM(ADJUSTL(file_id))   // &
    '.dat')

WRITE(UNIT,FORMAT_PERTURB_HEADER) &
    'radius ' , 'vR ', 'vTh ' ,'vX ', 'Pr '

DO i = 1,numberOfGridPoints
    WRITE(UNIT,FORMAT_PERTURB_VARS) r(i) , vR(i) , vTh(i) , vX(i), Pr(i)
END DO

CLOSE(UNIT);

! MMS

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'sound-speed-error'      // &
    TRIM(ADJUSTL(file_id))   // &
    '.dat')

WRITE(UNIT,FORMAT_ERROR_HEADER) 'radius ' , 'SpeedofSoundError ', 'Expected ', 'Actual '

DO i = 1,numberOfGridPoints
    WRITE(UNIT,FORMAT_ERROR) r(i) , SoundSpeedError(i), speedOfSoundMMS(i), SoundSpeedOut(i)
END DO

CLOSE(UNIT);

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'SourceTermData1_'      // &
    TRIM(ADJUSTL(file_id))   // &
    '_FDmethod'  // &
    TRIM(ADJUSTL(FDfac_id))// &
    '.dat')


WRITE(UNIT,FORMAT_SOURCE_TERMS_HEADER) &
    'radius ' , 'S_expected ' ,'S_actual ' ,'Error '

DO i = 1,numberOfGridPoints
    WRITE(UNIT,FORMAT_SOURCE_TERMS) &
        rOut(i), &
        REAL(S_MMS(i),KIND=rDef), &
        REAL(S_actual(i),KIND=rDef)  , &
        REAL(S_error(i),KIND=rDef)

END DO

CLOSE(UNIT);

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'SourceTermData2_'       // &
    TRIM(ADJUSTL(file_id))   // &
    '_FDmethod'  // &
    TRIM(ADJUSTL(FDfac_id))// &
    '.dat')

WRITE(UNIT,FORMAT_SOURCE_TERMS_HEADER) 'radius ' , 'S_expected ' ,'S_actual ' ,'Error '

DO i = 1,numberOfGridPoints
    WRITE(UNIT,FORMAT_SOURCE_TERMS) &
        rOut(i) , &
        REAL(S_MMS(i+numberOfGridPoints),KIND=rDef), &
        REAL(S_actual(i+numberOfGridPoints),KIND=rDef)  , &
        REAL(S_error(i+numberOfGridPoints),KIND=rDef)
END DO

CLOSE(UNIT);



OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'SourceTermData3_'       // &
    TRIM(ADJUSTL(file_id))   // &
    '_FDmethod'  // &
    TRIM(ADJUSTL(FDfac_id))// &
    '.dat')

WRITE(UNIT,FORMAT_SOURCE_TERMS_HEADER) 'radius ' , 'S_expected ' ,'S_actual ' ,'Error '

DO i = 1,numberOfGridPoints
    WRITE(UNIT,FORMAT_SOURCE_TERMS) &
        rOut(i), &
        REAL(S_MMS(i+numberOfGridPoints*2),KIND=rDef), &
        REAL(S_actual(i+numberOfGridPoints*2),KIND=rDef)  , &
        REAL(S_error(i+numberOfGridPoints*2),KIND=rDef)
END DO

CLOSE(UNIT);

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'SourceTermData4_'       // &
    TRIM(ADJUSTL(file_id))   // &
    '_FDmethod'  // &
    TRIM(ADJUSTL(FDfac_id))// &
    '.dat')

WRITE(UNIT,FORMAT_SOURCE_TERMS_HEADER) 'radius ' , 'S_expected ' ,'S_actual ' ,'Error '

DO i = 1,numberOfGridPoints
    WRITE(UNIT,FORMAT_SOURCE_TERMS) &
        rOut(i), &
        REAL(S_MMS(i+numberOfGridPoints*3),KIND=rDef), &
        REAL(S_actual(i+numberOfGridPoints*3),KIND=rDef)  , &
        REAL(S_error(i+numberOfGridPoints*3),KIND=rDef)
END DO

CLOSE(UNIT);

