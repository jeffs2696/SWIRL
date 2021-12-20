PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject               !Runs SWIRL for a given set of parameters
    USE mmsClassObject                 !Calculates L2Norm, L2Max, and Rate Of Convergence
    USE SourceTermModule               !Calculated the symbolic terms needed for MMS (used Python)

    IMPLICIT NONE

    INTEGER, PARAMETER :: &
        rDef = REAL64   , &
        numberOfIterations = 9

    include '/main-scripts/main-variables.f90'

! Code Starts Here!
    CONTINUE

    IF (debug) THEN

        WRITE(0, *) 'Number of Grid Study Iterations: ' , numberOfIterations

    ELSE
    ENDIF

    !local variables 
    include '/main-scripts/main-local-variables.f90'
    
    ALLOCATE(&
        k(7) , &
        S_L2Array(numberOfIterations)              , &
        SoundSpeedL2Array(numberOfIterations)       ,&
        RateOfConvergence1(numberOfIterations - 1) , &
        RateOfConvergence2(numberOfIterations - 1) )

    ! used for the following terms:
    k(1) = CMPLX(0.004, 0.0, rDef)
    k(2) = CMPLX(16.0, 0.0, rDef)
    k(3) = CMPLX(0.4, 0.0, rDef)  ! M_x 
    k(4) = CMPLX(1.80, 0.0, rDef)   !v_r  
    k(5) = CMPLX(1.200, 0.0, rDef)   !v_th
    k(6) = CMPLX(0.10, 0.0, rDef)   !v_X
    k(7) = CMPLX(.1, 0.0, rDef)   !p

    facCount = 0 ! initializer for fac count

    DO fac = 1, numberOfIterations

        facCount             = facCount + 1
        numberOfGridPoints   = 1+(2**fac)
        dr                   = &
            (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)

        IF (debug) THEN

            WRITE(0, *) '# Grid Points ',  numberOfGridPoints

        ELSE
        ENDIF

        ALLOCATE(&
            r(numberOfGridPoints)                    , &
            rOut(numberOfGridPoints)                 , &
            thetaMachData(numberOfGridPoints)        , &
            thetaMachDataOut(numberOfGridPoints)     , &
            thetaMachData_dr_Out(numberOfGridPoints) , &
            axialMachData(numberOfGridPoints)        , &
            axialMachDataOut(numberOfGridPoints)     , &
            axialMachData_dr_Out(numberOfGridPoints) , &
            totalMachData(numberOfGridPoints)        , &
            SoundSpeedOut(numberOfGridPoints)        , &
            SoundSpeed_dr_Out(numberOfGridPoints)    , & 
            SoundSpeedExpected(numberOfGridPoints)   , &
            SoundSpeedError(numberOfGridPoints)      , &
            S_actual(numberOfGridPoints*4)              , &
            S_A_actual(numberOfGridPoints*4)              , &
            S_B_actual(numberOfGridPoints*4)              , &
            S_Expected(numberOfGridPoints*4)            , &
            S_1(numberOfGridPoints)                  , &
            S_2(numberOfGridPoints)                  , &
            S_3(numberOfGridPoints)                  , &
            S_4(numberOfGridPoints)                  , &
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

        END DO

        ! from SourceTermModule
        CALL getSoundSpeed(&
            r                  = r                  , &
            k                  = k                  , &
            kappa              = gam                , &
            SoundSpeedExpected = SoundSpeedExpected , &
            thetaMachData      = thetaMachData      , &
            axialMachData      = axialMachData      )

        CALL getPerturbationVariables(&
            k     = k     , &
            r     = r    , &
            vR    = vR   , &
            vTh   = vT   , &
            vX    = vX   , &
            Pr    = Pr      )


        ! Calculating Total Mach Number and checking if it is greater than one anywhere
        DO i = 1, numberOfGridPoints
            totalMachData(i)  =&
                (&
                (axialMachData(i)**2.0_rDef+&
                thetaMachData(i)**2.0_rDef)**0.5_rDef)

            WRITE(0,*) i, axialMachData(i), thetaMachData(i), totalMachData(i)
            IF(totalMachData(i) > 1.0_rDef) THEN
                WRITE(0, *) i, 'ERROR: Total mach is greater than one at', i
                STOP
            ELSE
            ENDIF

            IF(SoundSpeedExpected(i) > 1.0_rDef) THEN
                WRITE(0, *) i, 'ERROR: The expected speend of sound is greater than one at', i
                STOP
            ELSE
            ENDIF

        ENDDO

        !Create a swirlClassObj for a given flow
        CALL CreateObject(&
            object        = swirlClassObj(fac)  ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            AxialMachData = axialMachData        ,&
            ThetaMachData = thetaMachData        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       )

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

        DO i = 1,numberOfGridPoints
            SoundSpeedError(i) = ABS(SoundSpeedOut(i)-SoundSpeedExpected(i))
        ENDDO

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

        CALL getL2Norm(&
            object    = SoundSpeedMMS_ClassObj ,& 
            L2        = SoundSpeedErrorL2      ,&
            dataSet1  = SoundSpeedExpected     ,&
            dataSet2  = SoundSpeedOut           )

        SoundSpeedL2Array(fac) = SoundSpeedErrorL2

        DO i = 1,numberOfGridPoints

            eigenVectorMMS(i) = &
                CMPLX(vR(i),KIND = rDef)

            eigenVectorMMS(i +   numberOfGridPoints) = &
                CMPLX(vT(i), KIND = rDef)

            eigenVectorMMS(i + 2*numberOfGridPoints) =&
                CMPLX(vX(i), KIND = rDef)

            eigenVectorMMS(i + 3*numberOfGridPoints) = &
                CMPLX(Pr(i), KIND = rDef)

        ENDDO

        axialWavenumberMMS = CMPLX(100.0_rDef,0.00_rDef,KIND=rDef)

        ! * Comparing Source Terms *

        ! from swirlClassObj
        CALL FindResidualData(&
            object      = swirlClassObj(fac),&
            eigenVector = eigenVectorMMS       ,&
            eigenValue  = -ci*axialWavenumberMMS        ,&
            S_A         = S_A_actual ,&
            S_B         = S_B_actual ,&
            S           = S_actual )
        ! 

        DO i = 1,numberOfGridPoints

             CALL getMMSSourceTerms( &
                 gam   = axialWavenumberMMS           ,& !WE NEED TO extract modal data to get the axial wavenumber here
                 i     = ci                      ,&
                 ak    = frequency               ,&
                 k     = k                       ,&
                 kappa = gam                     ,&
                 m     = azimuthalModeNumber     ,&
                 r     = r(i)                    ,&
                 r2    = r2                      ,&
                 r3    = r3                      ,&
                 r_max = r_max                   ,&
                 S_1   = S_1(i)                  ,&
                 S_2   = S_2(i)                  ,&
                 S_3   = S_3(i)                  ,&
                 S_4   = S_4(i)     )             
!
!            CALL getMMSSourceTermComponents( &
!                gam   = axialWavenumberMMS           ,& !WE NEED TO extract modal data to get the axial wavenumber here
!                i     = ci                      ,&
!                ak    = frequency               ,&
!                k     = k                       ,&
!                kappa = gam                     ,&
!                m     = azimuthalModeNumber     ,&
!                r     = r(i)                    ,&
!                r2    = r2                      ,&
!                r3    = r3                      ,&
!                r_max = r_max                   ,&
!                S_1   = S_1(i)                  ,&
!                S_2   = S_2(i)                  ,&
!                S_3   = S_3(i)                  ,&
!                S_4   = S_4(i)                  ,&
!                S_A11 = S_A11(i)                ,&
!                S_A12 = S_A12(i)                ,&
!                S_A13 = S_A13(i)                ,&
!                S_A14 = S_A14(i)                ,&
!                S_A21 = S_A21(i)                ,&
!                S_A22 = S_A22(i)                ,&
!                S_A23 = S_A23(i)                ,&
!                S_A24 = S_A24(i)                ,&
!                S_A31 = S_A31(i)                ,&
!                S_A32 = S_A32(i)                ,&
!                S_A33 = S_A33(i)                ,&
!                S_A34 = S_A34(i)                ,&
!                S_A41 = S_A41(i)                ,&
!                S_A42 = S_A42(i)                ,&
!                S_A43 = S_A43(i)                ,&
!                S_A44 = S_A44(i)                ,&
!                S_B11 = S_B11(i)                ,&
!                S_B12 = S_B12(i)                ,&
!                S_B13 = S_B13(i)                ,&
!                S_B14 = S_B14(i)                ,&
!                S_B21 = S_B21(i)                ,&
!                S_B22 = S_B22(i)                ,&
!                S_B23 = S_B23(i)                ,&
!                S_B24 = S_B24(i)                ,&
!                S_B31 = S_B31(i)                ,&
!                S_B32 = S_B32(i)                ,&
!                S_B33 = S_B33(i)                ,&
!                S_B34 = S_B34(i)                ,&
!                S_B41 = S_B41(i)                ,&
!                S_B42 = S_B42(i)                ,&
!                S_B43 = S_B43(i)                ,&
!                S_B44 = S_B44(i)                )                                      
!
            i1 = i + numberOfGridPoints
            i2 = i + 2*numberOfGridPoints
            i3 = i + 3*numberOfGridPoints

            S_Expected(i)  = S_1(i)
            S_Expected(i1) = S_2(i)
            S_Expected(i2) = S_3(i)
            S_Expected(i3) = S_4(i)

        ENDDO

        S_error = ABS(S_Expected - S_actual)

        CALL getL2Norm(&
            object    = SourceTermMMS_ClassObj,&
            L2        = S_L2 ,&
            dataSet1  = S_actual,&
            dataSet2  = S_Expected)

        S_L2Array(fac) = S_L2   

        include 'swirl-data-export-per-grid.f90'
        CALL DestroyObject(object = swirlClassObj(fac))

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
            SoundSpeedExpected    ,&
            SoundSpeedError       ,&
            S_actual                 ,&
            S_A_actual                 ,&
            S_B_actual                 ,&
            S_Expected                 ,&
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

    END DO
    !Construct spars A and B to check each term


    CALL getRateOfConvergence(&
        object            = SoundSpeedMMS_ClassObj , &
        RateOfConvergence = RateOfConvergence1 , &
        L2Array           = SoundSpeedL2Array)

    CALL getRateOfConvergence(&
        object            = SourceTermMMS_ClassObj, &
        RateOfConvergence = RateOfConvergence2 , &
        L2Array           = S_L2Array)

    ! Should I put the data writing in another script? : JS
    include 'swirl-data-export-MMS.f90'

    DEALLOCATE( &
        SoundSpeedL2Array ,&
        S_L2Array ,&
        RateOfConvergence1 ,&
        RateOfConvergence2)

END PROGRAM MAIN
