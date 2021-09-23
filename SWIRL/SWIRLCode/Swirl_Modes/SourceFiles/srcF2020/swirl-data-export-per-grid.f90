! used to export data for swirl

! 01-mean-flow-data
        WRITE(file_id, '(i0)') numberOfGridPoints

        dir_name = '01-mean-flow/'
        file_name = TRIM(dir_name) // 'mean-flow-' // TRIM(ADJUSTL(file_id)) // '.dat'

         OPEN(&
             NEWUNIT=UNIT,&
             FILE   =&
             TRIM(ADJUSTL(dir_name))  // &
             'mean-flow' // &
             TRIM(ADJUSTL(file_id))   // &
             '.dat')

        ! Write the resulting mean flow
        WRITE(UNIT,FORMAT_MEAN_FLOW_HEADERS) &
            'radius','M_x','M_theta','A_expected','A_actual'

        DO i = 1,numberOfGridPoints

            WRITE(UNIT,FORMAT_MEAN_FLOW) &
                rOut(i)                 , &
                axialMachDataOut(i)     , &
                thetaMachDataOut(i)     , &
                SoundSpeedExpected(i)   , &
                SoundSpeedOut(i)

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

        ! 02-linearized-perturbation-equations

         dir_name = '02-linearized-perturbation-equations/'

         OPEN(&
             NEWUNIT=UNIT,&
             FILE   =&
             TRIM(ADJUSTL(dir_name))  // &
             'perturbation-variables' // &
             TRIM(ADJUSTL(file_id))   // &
             '.dat')

         WRITE(UNIT,*) 'Radius' , 'vR', 'vTh' ,'vX', 'Pr'

         DO i = 1,numberOfGridPoints
            WRITE(UNIT,*) r(i) , vR(i) , vT(i) , vX(i), Pr(i)
         END DO

         CLOSE(UNIT);

        dir_name = '03-method-of-manufactured-solutions/'
        ! file_name = TRIM(dir_name) // 'sound-speed-error' // TRIM(ADJUSTL(file_id)) // '.dat'

         OPEN(&
             NEWUNIT=UNIT,&
             FILE   =&
             TRIM(ADJUSTL(dir_name))  // &
             'sound-speed-error'      // &
             TRIM(ADJUSTL(file_id))   // &
             '.dat')

        !   OPEN(NEWUNIT=UNIT,FILE=(file_name))

        WRITE(UNIT,*) 'Grid Points' , 'Speed of Sound Error'

        DO i = 1,numberOfGridPoints
            WRITE(UNIT,*) r(i) , SoundSpeedError(i)
        END DO

        CLOSE(UNIT);

         OPEN(&
             NEWUNIT=UNIT,&
             FILE   =&
             TRIM(ADJUSTL(dir_name))  // &
             'SourceTermData1_'      // &
             TRIM(ADJUSTL(file_id))   // &
             '.dat')


        WRITE(UNIT,*) 'Grid Points' , 'S_Actual' ,'S_actual' ,'Source Term Error'

        DO i = 1,numberOfGridPoints
            WRITE(UNIT,*) &
                i, &
                REAL(S_Expected(i),KIND=rDef), &
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
             '.dat')

        WRITE(UNIT,*) 'Grid Points' , 'S_Actual' , 'S_actual' ,'Source Term Error'

        DO i = numberOfGridPoints,numberOfGridPoints*2
            WRITE(UNIT,*) &
                i, &
                REAL(S_Expected(i),KIND=rDef), &
                REAL(S_actual(i),KIND=rDef)  , &
                REAL(S_error(i),KIND=rDef)
        END DO

        CLOSE(UNIT);



         OPEN(&
             NEWUNIT=UNIT,&
             FILE   =&
             TRIM(ADJUSTL(dir_name))  // &
             'SourceTermData3_'       // &
             TRIM(ADJUSTL(file_id))   // &
             '.dat')

         WRITE(UNIT,*) 'Grid Points' , 'S_Actual' , 'S_actual' ,'Source Term Error'

         DO i = numberOfGridPoints*2,numberOfGridPoints*3
             WRITE(UNIT,*) &
                 i, &
                 REAL(S_Expected(i),KIND=rDef), &
                 REAL(S_actual(i),KIND=rDef)  , &
                 REAL(S_error(i),KIND=rDef)
         END DO

        CLOSE(UNIT);

         OPEN(&
             NEWUNIT=UNIT,&
             FILE   =&
             TRIM(ADJUSTL(dir_name))  // &
             'SourceTermData4_'       // &
             TRIM(ADJUSTL(file_id))   // &
             '.dat')

        WRITE(UNIT,*) 'Grid Points' , 'S_Actual' , 'S_actual' ,'Source Term Error'

        DO i = numberOfGridPoints*3,numberOfGridPoints*4
            WRITE(UNIT,*) &
                i, &
                REAL(S_Expected(i),KIND=rDef), &
                REAL(S_actual(i),KIND=rDef)  , &
                REAL(S_error(i),KIND=rDef)
        END DO

        CLOSE(UNIT);

