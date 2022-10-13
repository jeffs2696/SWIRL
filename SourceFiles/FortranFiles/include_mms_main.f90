    IF (MMSflag) THEN

        CALL getSoundSpeed(&
            r                  = r                  , &
            SoundSpeedExpected = speedOfSoundMMS , &
            thetaMachData      = thetaMachDataMMS, &
            axialMachData      = axialMachDataMMS      )
        CALL getPerturbationVariables(&
            r     = r    , &
            vR    = vR   , &
            vTh   = vTh   , &
            vX    = vX   , &
            Pr    = Pr      )

        DO i = 1,numberOfGridPoints

        eigenVectorMMS(i) = &
            CMPLX(vR(i), KIND = rDef)
        eigenVectorMMS(i + numberOfGridPoints) = &
            CMPLX(vTh(i), KIND = rDef)
        eigenVectorMMS(i + 2*numberOfGridPoints) = &
            CMPLX(vX(i), KIND = rDef)
        eigenVectorMMS(i + 3*numberOfGridPoints) = &
            CMPLX(Pr(i), KIND = rDef)

        ENDDO

        CALL getMMSSourceTerms( &
            r     = r                    ,&
            S_1   = S_1                  ,&
            S_2   = S_2                  ,&
            S_3   = S_3                  ,&
            S_4   = S_4     )


        DO i = 1,numberOfGridPoints

        S_MMS(i) = S_1(i)
        S_MMS(i+numberOfGridPoints) = S_2(i)
        S_MMS(i+2*numberOfGridPoints) = S_3(i)
        S_MMS(i+3*numberOfGridPoints) = S_4(i)
        ENDDO
        CALL CreateObject(&
            object        = swirlClassObjMMS(fac)  ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            axialMachData = axialMachDataMMS        ,&
            tangentialMachData = thetaMachDataMMS        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       ,&
            secondOrderSmoother = 0.0_rDef       ,&
            fourthOrderSmoother = 0.0_rDef       ,& 
            debugFlag     = debug                ,&
            test_name  ='MMS')

        CALL runSwirlClassMethods(&
            object    = swirlClassObjMMS(fac) , &
            debugFlag = debug)

        CALL GetMeanFlowData(&
            object        = swirlClassObjMMS(fac), &
            axialMach     = axialMachDataMMSOut  , &
            thetaMach     = thetaMachDataMMSOut  , &
            axialMach_dr  = axialMach_drMMSOut   , &
            thetaMach_dr  = thetaMach_drMMSOut   , &
            SoundSpeed    = SoundSpeedOut        , &
            SoundSpeed_dr = SoundSpeed_drOut     , &
            radialData    = rOut                 , &
            debugFlag     = debug)

        CALL FindResidualData(&
            object      = swirlClassObjMMS(fac), &
            eigenVector = eigenVectorMMS       , &
            eigenValue  =  eigenValueMMS       , &
            S           =  S_actual)

        DO i = 1,numberOfGridPoints

        SoundSpeedError(i) = &
            speedOfSoundMMS(i) - SoundSpeedOut(i)
        S_error(i) = &
            ABS(S_actual(i) - S_MMS(i))
        S_error(i + numberOfGridPoints) = &
            ABS(S_actual(i + numberOfGridPoints) - S_MMS(i + numberOfGridPoints))
        S_error(i + 2*numberOfGridPoints) = &
            ABS(S_actual(i + 2*numberOfGridPoints) - S_MMS(i + 2*numberOfGridPoints))
        S_error(i + 3*numberOfGridPoints) = &
            ABS(S_actual(i + 3*numberOfGridPoints) - S_MMS(i + 3*numberOfGridPoints))

        ENDDO

        ! INTERFACE - L2N
        CALL getL2Norm(&
            object   = SoundSpeedMMS_ClassObj , &
            L2       = SoundSpeedErrorL2 , &
            dataSet1 = speedOfSoundMMS , &
            dataSet2 = SoundSpeedOut)

        SoundSpeedL2Array(fac) = SoundSpeedErrorL2
        ! INTERFACE - L2N_2D 
        CALL getL2Norm(&
            object      = SourceTermMMS_ClassObj , &
            L2          = S_L2 , &
            dataSet1    = S_MMS, &
            dataSet2    = S_actual)

        S_L2Array(fac) = REAL(S_L2, KIND = rDef)

        CALL getL2Norm(&
            object      = SourceTermMMS_ClassObj , &
            L2          = S1_L2 , &
            dataSet1    = S_MMS(1:numberOfGridPoints), &
            dataSet2    = S_actual(1:numberOfGridPoints))

        S1_L2Array(fac) = REAL(S1_L2, KIND = rDef)

        CALL getL2Norm(&
            object      = SourceTermMMS_ClassObj , &
            L2          = S3_L2 , &
            dataSet1    = S_MMS((1 + 2*numberOfGridPoints):numberOfGridPoints*3), &
            dataSet2    = S_actual((1+2*numberOfGridPoints):numberOfGridPoints*3))

        S3_L2Array(fac) = REAL(S3_L2, KIND = rDef)


        CALL getL2Norm(&
            object      = SourceTermMMS_ClassObj , &
            L2          = S4_L2 , &
            dataSet1    = S_MMS((1 + 3*numberOfGridPoints):numberOfGridPoints*3), &
            dataSet2    = S_actual((1+3*numberOfGridPoints):numberOfGridPoints*3))

        S4_L2Array(fac) = REAL(S4_L2, KIND = rDef)

        include 'main-scripts/swirl-data-export-per-grid-MMS.f90'

        CALL DestroyObject(object = swirlClassObjMMS(fac))

    ELSE 

    ENDIF

    ! for test cases 
    include 'main-scripts/swirl-data-export-per-grid.f90'

    DEALLOCATE(&
        axialMachDataMMSOut, &
        thetaMachDataMMSOut, &
        r                     ,&
        rOut,&
        thetaMachData         ,&
        axialMachData         ,&
        thetaMachDataMMS         ,&
        axialMach_drMMSOut         ,&
        thetaMach_drMMSOut         ,&
        SoundSpeedOut         ,&
        SoundSpeedError   , &
        speedOfSoundMMS , &
        SoundSpeed_drOut         ,&
        axialMachDataMMS         ,&
        vR,vTh,vX,Pr, &
        eigenVectorMMS, &
        totalMachData , &
        S_MMS,S_actual, S_error, S_1, S_2, S_3, S_4)

    ENDDO

    IF (MMSflag) THEN 
        CALL getRateOfConvergence(&
            object            = SoundSpeedMMS_ClassObj , &
            ExpectedRateOfConvergence = ExpectedRateOfConvergenceSoundSpeed   , &
            RateOfConvergence = RateOfConvergence1 , &
            L2Array           = SoundSpeedL2Array, &
            gridSpacingRatio = gridSpacingRatio)

        CALL getRateOfConvergence(&
            object            = SourceTermMMS_ClassObj, &
            RateOfConvergence = RateOfConvergence2 , &
            L2Array           = S_L2Array          , &
            gridSpacingRatio  = gridSpacingRatio)

        ! include 'main-scripts/swirl-data-export-MMS.f90'
        ! include 'main-scripts/swirl-data-export-per-grid-MMS.f90'

    ELSE
    ENDIF

