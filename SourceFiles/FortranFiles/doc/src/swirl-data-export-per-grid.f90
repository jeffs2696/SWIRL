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
    'A_actual '  

DO i = 1,numberOfGridPoints

    WRITE(UNIT,*) &
        r(i)                             , &
        axialMachDataMMSOut(i)                 , &
        thetaMachDataMMSOut(i)                 

ENDDO

CLOSE(UNIT);
