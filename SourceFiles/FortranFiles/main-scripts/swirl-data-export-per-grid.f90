! used to export data for swirl

! 01-mean-flow-data
WRITE(file_id, '(i0.4)') numberOfGridPoints

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
        axialMachData(i)                 , &
        thetaMachDataOut(i)                 , &
        speedOfSound(i)               , &
        SoundSpeedOut(i)                    

    IF (debug) THEN
!        WRITE(0,FORMAT_MEAN_FLOW) &
!            rOut(i)                 , &
!            axialMachDataOut(i)     , &
!            thetaMachDataOut(i)     , &
!            SoundSpeedExpected(i)   , &
!            SoundSpeedOut(i)
!    ELSE
    ENDIF
ENDDO

CLOSE(UNIT);
