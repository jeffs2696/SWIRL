! used to export data for swirl

! 01-mean-flow-data
WRITE(file_id, '(i0)') numberOfGridPoints

dir_name  = '01-mean-flow/'

OPEN(&
    NEWUNIT=UNIT,&
    FILE   =&
    TRIM(ADJUSTL(dir_name))  // &
    'mean-flow' // &
    TRIM(ADJUSTL(file_id))   // &
    '.dat')

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
        axialMachDataOut(i)                 , &
        thetaMachDataOut(i)                 , &
        SoundSpeedExpected(i)               , &
        SoundSpeedOut(i)                    , &
        vR(i)                               , &
        vT(i)                              , &
        vX(i)                               , &
        Pr(i)                               , &
        REAL(S_Expected(i),KIND=rDef)       , &
        REAL(S_Expected(i+1*numberOfGridPoints),KIND=rDef)  , &
        REAL(S_Expected(i+2*numberOfGridPoints),KIND=rDef)  , &
        REAL(S_Expected(i+3*numberOfGridPoints),KIND=rDef)  , &
        REAL(S_actual(i),KIND=rDef)                       , &
        REAL(S_actual(i+1*numberOfGridPoints),KIND=rDef)  , &
        REAL(S_actual(i+2*numberOfGridPoints) ,KIND=rDef) , &
        REAL(S_actual(i+3*numberOfGridPoints) ,KIND=rDef) 

    IF (debug) THEN
        WRITE(0,FORMAT_MEAN_FLOW) &
            rOut(i)                 , &
            axialMachDataOut(i)     , &
            thetaMachDataOut(i)     , &
            SoundSpeedExpected(i)   , &
            SoundSpeedOut(i)
    ELSE
    ENDIF
ENDDO

CLOSE(UNIT);

! LEE

!dir_name = '02-linearized-perturbation-equations/'
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'perturbation-variables' // &
!    TRIM(ADJUSTL(file_id))   // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_PERTURB_HEADER) &
!    'Radius ' , 'vR ', 'vTh ' ,'vX ', 'Pr '
!
!DO i = 1,numberOfGridPoints
!    WRITE(UNIT,FORMAT_PERTURB_VARS) r(i) , vR(i) , vT(i) , vX(i), Pr(i)
!END DO
!
!CLOSE(UNIT);

! MMS
!dir_name = '03-method-of-manufactured-solutions/'
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'sound-speed-error'      // &
!    TRIM(ADJUSTL(file_id))   // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_ERROR_HEADER) 'GridPoints ' , 'SpeedofSoundError '
!
!DO i = 1,numberOfGridPoints
!    WRITE(UNIT,FORMAT_ERROR) r(i) , SoundSpeedError(i)
!END DO
!
!CLOSE(UNIT);
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'SourceTermData1_'      // &
!    TRIM(ADJUSTL(file_id))   // &
!    '.dat')
!
!
!WRITE(UNIT,FORMAT_SOURCE_TERMS_HEADER) &
!    'GridPoints ' , 'S_expected ' ,'S_actual ' ,'Error '
!
!DO i = 1,numberOfGridPoints
!    WRITE(UNIT,FORMAT_SOURCE_TERMS) &
!        i, &
!        REAL(S_Expected(i),KIND=rDef), &
!        REAL(S_actual(i),KIND=rDef)  , &
!        REAL(S_error(i),KIND=rDef)
!END DO
!
!CLOSE(UNIT);
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'SourceTermData2_'       // &
!    TRIM(ADJUSTL(file_id))   // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_SOURCE_TERMS_HEADER) 'GridPoints ' , 'S_expected ' ,'S_actual ' ,'Error '
!
!DO i = numberOfGridPoints,numberOfGridPoints*2
!    WRITE(UNIT,FORMAT_SOURCE_TERMS) &
!        i, &
!        REAL(S_Expected(i),KIND=rDef), &
!        REAL(S_actual(i),KIND=rDef)  , &
!        REAL(S_error(i),KIND=rDef)
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
!    'SourceTermData3_'       // &
!    TRIM(ADJUSTL(file_id))   // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_SOURCE_TERMS_HEADER) 'GridPoints ' , 'S_expected ' ,'S_actual ' ,'Error '
!
!DO i = numberOfGridPoints*2,numberOfGridPoints*3
!    WRITE(UNIT,FORMAT_SOURCE_TERMS) &
!        i, &
!        REAL(S_Expected(i),KIND=rDef), &
!        REAL(S_actual(i),KIND=rDef)  , &
!        REAL(S_error(i),KIND=rDef)
!END DO
!
!CLOSE(UNIT);
!
!OPEN(&
!    NEWUNIT=UNIT,&
!    FILE   =&
!    TRIM(ADJUSTL(dir_name))  // &
!    'SourceTermData4_'       // &
!    TRIM(ADJUSTL(file_id))   // &
!    '.dat')
!
!WRITE(UNIT,FORMAT_SOURCE_TERMS_HEADER) 'GridPoints ' , 'S_expected ' ,'S_actual ' ,'Error '
!
!DO i = numberOfGridPoints*3,numberOfGridPoints*4
!    WRITE(UNIT,FORMAT_SOURCE_TERMS) &
!        i, &
!        REAL(S_Expected(i),KIND=rDef), &
!        REAL(S_actual(i),KIND=rDef)  , &
!        REAL(S_error(i),KIND=rDef)
!END DO
!
!CLOSE(UNIT);

