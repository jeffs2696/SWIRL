        facCount             = facCount + 1
        numberOfGridPoints   = 5+(2**fac)

        dr                   = &
            (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)

        ALLOCATE(&
            r(numberOfGridPoints)                                            , &
            rOut(numberOfGridPoints)                                         , &
            thetaMachData(numberOfGridPoints)                                , &
            thetaMachDataOut(numberOfGridPoints)                             , &
            thetaMachData_dr_Out(numberOfGridPoints)                         , &
            axialMachData(numberOfGridPoints)                                , &
            axialMachDataOut(numberOfGridPoints)                             , &
            axialMachData_dr_Out(numberOfGridPoints)                         , &
            totalMachData(numberOfGridPoints)                                , &
            SoundSpeedOut(numberOfGridPoints)                                , &
            SoundSpeed_dr_Out(numberOfGridPoints)                            , &
        !SoundSpeedExpected(numberOfGridPoints)                           , &
        !SoundSpeedError(numberOfGridPoints)                              , &
        !S_actual(numberOfGridPoints*4)                                      , &
            S_A_actual(numberOfGridPoints*4)                                      , &
            S_B_actual(numberOfGridPoints*4)                                 , &
        !S_Expected(numberOfGridPoints*4)                                 , &
            S_1(numberOfGridPoints)                                          , &
            S_2(numberOfGridPoints)                                          , &
            S_3(numberOfGridPoints)                                          , &
            S_4(numberOfGridPoints)                                          , &
            S_A11(numberOfGridPoints)                                        , &
            S_A12(numberOfGridPoints)                                        , &
            S_A13(numberOfGridPoints)                                        , &
            S_A14(numberOfGridPoints)                                        , &
            S_A21(numberOfGridPoints)                                        , &
            S_A22(numberOfGridPoints)                                        , &
            S_A23(numberOfGridPoints)                                        , &
            S_A24(numberOfGridPoints)                                        , &
            S_A31(numberOfGridPoints)                                        , &
            S_A32(numberOfGridPoints)                                        , &
            S_A33(numberOfGridPoints)                                        , &
            S_A34(numberOfGridPoints)                                        , &
            S_A41(numberOfGridPoints)                                        , &
            S_A42(numberOfGridPoints)                                        , &
            S_A43(numberOfGridPoints)                                        , &
            S_A44(numberOfGridPoints)                                        , &
            S_B11(numberOfGridPoints)                                        , &
            S_B12(numberOfGridPoints)                                        , &
            S_B13(numberOfGridPoints)                                        , &
            S_B14(numberOfGridPoints)                                        , &
            S_B21(numberOfGridPoints)                                        , &
            S_B22(numberOfGridPoints)                                        , &
            S_B23(numberOfGridPoints)                                        , &
            S_B24(numberOfGridPoints)                                        , &
            S_B31(numberOfGridPoints)                                        , &
            S_B32(numberOfGridPoints)                                        , &
            S_B33(numberOfGridPoints)                                        , &
            S_B34(numberOfGridPoints)                                        , &
            S_B41(numberOfGridPoints)                                        , &
            S_B42(numberOfGridPoints)                                        , &
            S_B43(numberOfGridPoints)                                        , &
            S_B44(numberOfGridPoints)                                        , &
            S_eig(numberOfGridPoints*4)              , &
            S_error(numberOfGridPoints*4)              , &
            vR(numberOfGridPoints)                  ,&
            vT(numberOfGridPoints)                  ,&
            vX(numberOfGridPoints)                  ,&
            Pr(numberOfGridPoints)                  ,&
            eigenVector(numberOfGridPoints*4)        , &
            eigenVectorMMS(numberOfGridPoints*4)          )

        DO i = 1, numberOfGridPoints

            r(i) = (r_min+REAL(i-1, rDef)*dr)/r_max

            axialMachData(i) = 0.2*COS(0.2*r(i))
            thetaMachData(i) = 0.2*COS(0.2*r(i))
        END DO

        !Create a swirlClassObj for a given flow
        CALL CreateObject(&
            object        = swirlClassObj(fac)  ,&
            radius        = r                    ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            axialMachData = axialMachData        ,&
            tangentialMachData = thetaMachData        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       )
        
        CALL runSwirlClassMethods(&
            object = swirlClassObj(fac))

        !Get mean flow data from swirlClassObj
        CALL GetMeanFlowData(&
            object          = swirlClassObj(fac), &
            axialMach       = axialMachDataOut, &
            thetaMach       = thetaMachDataOut, &
            axialMach_dr    = axialMachData_dr_Out, &
            thetaMach_dr    = thetaMachData_dr_Out, &
            SoundSpeed      = SoundSpeedOut, &
            SoundSpeed_dr   = SoundSpeed_dr_Out, &
            radialData      = rOut)
!
!        DO i = 1,numberOfGridPoints
!            SoundSpeedError(i) = ABS(SoundSpeedOut(i)-SoundSpeedExpected(i))
!        ENDDO
!
        ! Get Mode Data passes back the eigen values and modes for a given
        ! index. This is because the eigenValue in the swirlClassObj is 1xnp4 and
        ! the eigenVector is np4xnp4 long. Each column of the eigenVector
        ! corresponds to a single eigenVector. The user supplies the index
        ! that corresponds to n

        ! CALL GetModeData(&
        !     object     = swirlClassObj(fac) , &
        !     eigenValue = eigenValue         , &
        !     eigenVector= eigenVector        , &
        !     eigenIndex = eigenIndex)

        ! OPEN( NEWUNIT = UNIT, FILE = 'RadialModes.dat' )
        ! WRITE(0,*) 'Radial Mode' , eigenIndex, 'Axial Wavenumber', eigenValue
        ! DO i = 1,numberOfGridPoints
        !     WRITE(UNIT,*) r(i), REAL(eigenVector(i),KIND=rDef)
        ! ENDDO
        ! CLOSE(UNIT)
        include '/main-scripts/swirl-data-export-per-grid.f90'

!
        CALL DestroyObject(object = swirlClassObj(fac))
!
!        CALL CreateMMSObject(&
!            object = swirlClassObj(fac) , &
!            MMSflag = MMSflag)
!
!        CALL DestroyObject(object = swirlClassObj(fac))
!


        ! Export data

        DEALLOCATE(&
            r                     ,&
            rOut                  ,&
            thetaMachData         ,&
            thetaMachDataOut      ,&
            thetaMachData_dr_Out  ,&
            axialMachData         ,&
            axialMachDataOut      ,&
            axialMachData_dr_Out  ,&
            totalMachData         ,&
            SoundSpeedOut         ,&
            SoundSpeed_dr_Out     ,&
        !SoundSpeedExpected    ,&
        !SoundSpeedError       ,&
        !S_actual                 ,&
            S_A_actual                 ,&
            S_B_actual                 ,&
        !S_Expected                 ,&
            S_eig                 ,&
            S_error                 ,&
            S_1                      ,&
            S_2                      ,&
            S_3                      ,&
            S_4                      ,&
            S_A11                                        , &
            S_A12                                        , &
            S_A13                                        , &
            S_A14                                        , &
            S_A21                                        , &
            S_A22                                        , &
            S_A23                                        , &
            S_A24                                        , &
            S_A31                                        , &
            S_A32                                        , &
            S_A33                                        , &
            S_A34                                        , &
            S_A41                                        , &
            S_A42                                        , &
            S_A43                                        , &
            S_A44                                        , &
            S_B11                                        , &
            S_B12                                        , &
            S_B13                                        , &
            S_B14                                        , &
            S_B21                                        , &
            S_B22                                        , &
            S_B23                                        , &
            S_B24                                        , &
            S_B31                                        , &
            S_B32                                        , &
            S_B33                                        , &
            S_B34                                        , &
            S_B41                                        , &
            S_B42                                        , &
            S_B43                                        , &
            S_B44                                        , &
            vR                    ,&
            vT                    ,&
            vX                    ,&
            Pr                    ,&
            eigenVector           ,&
            eigenVectorMMS)

