PROGRAM MAIN
!! The main SWIRL code that generates the executable
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject               ! Runs SWIRL for a given set of parameters
    USE mmsClassObject                 ! Calculates L2Norm, L2Max, and Rate Of Convergence
    USE SourceTermModule               ! Calculated the symbolic terms needed for MMS (used Python)

    IMPLICIT NONE

    ! Variable Definitions
    CHARACTER(50) :: &
    !! headers, formatters, and filename variables
        dir_name,&
        file_id , &
        FDfac_id, &
        FORMAT_MEAN_FLOW         , &
        FORMAT_PERTURB_VARS      , &
        FORMAT_PERTURB_HEADER    , &
        FORMAT_MEAN_FLOW_HEADER  , &
        FORMAT_SOURCE_TERMS      , &
        FORMAT_SOURCE_TERMS_HEADER , &
        FORMAT_L2                , &
        FORMAT_L2_HEADER         , &
        FORMAT_ERROR             , &
        FORMAT_ERROR_HEADER      , &
        FORMAT_ROC               , &
        FORMAT_ROC_HEADER

    LOGICAL :: &
    !! Logical "flag" for debugging, set to .TRUE. to print WRITE statements in the directory
        debug = .TRUE.

    INTEGER, PARAMETER :: &
    !! Code parameters for double precision and number of iterations
        M_int = 3 , & 
        numberOfFiniteDifferenceSchemes = 1 , &
        rDef = REAL64   , &
        numberOfIterations = 8

    INTEGER  :: &
    !! Integers for flags and loop indicies
        UNIT , & ! for NEWUNIT
        finiteDiffFlag           ,& ! finite difference flag
        numericalIntegrationFlag ,& !  numerical integration flag
        numberOfGridPoints       ,& ! number of points
        azimuthalModeNumber      ,& ! mode order
        i                        ,& ! indexer for do loops
        fac                      ,& ! variable used for doubling grid points
        FDfac                    ,&
        facCount                    ! counts the outermost do loop

    INTEGER, DIMENSION(:), ALLOCATABLE :: &
    !! Integer arrays to store the number of grid points
        numberOfGridPointsArray, &
        gridSpacingArray

    REAL(KIND = REAL64) ::  &
        gam                                , &
        SoundSpeedErrorL2                  , &
        ExpectedRateOfConvergenceSoundSpeed, &
        ExpectedRateOfConvergenceSourceTerm, &
        r_min                              , &!radial grid locations
        r_max                              , &!radial grid locations
        secondOrderSmoother                , &!2nd order smoothing coefficient
        fourthOrderSmoother                , &!4th order smoothing coefficient
        dr                                 , &!radial grid spacing 
        gridSpacingRatio              ,& ! the difference between two grid spacing
        start_time                         , &
        end_time                           , &
        hubToTipRatio

    REAL(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
    !! real values arrays
        drArray             , &
        r                   , & !radial grid locations
        rOut                , &
        vR, vX, vTh, Pr     , &
        speedOfSoundMMS     , &
        SoundSpeedError     , &
        axialMachDataMMS    , & !M_x
        thetaMachDataMMS    , & !M_th
        axialMachDataMMSOut , & !M_x
        thetaMachDataMMSOut , & !M_th
        axialMach_drMMSOut  , &
        thetaMach_drMMSOut  , &
        SoundSpeedOut       , &
        SoundSpeedL2Array   , &
        SoundSpeed_drOut    , &
        axialMachData       , & !M_x
        thetaMachData       , & !M_th
        totalMachData       , & !M_total = sqrt(M_x^2+M_th^2)
        RateOfConvergence1  , &
        S_error

    COMPLEX(KIND=rDef) :: &
        S_L2            ,&
        eigenValueMMS   ,&
        hubAdmittance   ,&
        ductAdmittance  ,&
        frequency

    COMPLEX(KIND=rDef) , DIMENSION(:), ALLOCATABLE :: &
        S_1 , &
        S_2 , &
        S_3 , &
        S_4 , &
        S_MMS , &
        S_actual, &
        S_L2Array , &
        RateOfConvergence2  , &
        eigenVectorMMS

    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: &
    ! swirlClassObj, &
        swirlClassObjMMS

    TYPE(mmsClassType) :: SoundSpeedMMS_ClassObj, SourceTermMMS_ClassObj

    !                  !
    ! Code Starts Here !
    !                  !

    CONTINUE

    include 'InputVariables.f90'

    CALL CPU_TIME(start_time)
    FORMAT_MEAN_FLOW           = "(F15.12,F15.12,F15.12,F15.12,F15.12)"
    FORMAT_MEAN_FLOW_HEADER    = "(A15,A15,A15,A15,A15)"
    FORMAT_PERTURB_VARS        = "(F16.12,F16.12,F16.12,F16.12,F16.12)"
    FORMAT_PERTURB_HEADER      = "(A12,A12,A12,A12,A12)"
    FORMAT_SOURCE_TERMS        = "( F16.12, F16.12, F16.12,F16.12)"
    FORMAT_SOURCE_TERMS_HEADER = "( A6, A17, A17,A17)"
    FORMAT_L2                  = "(I10,F20.12)"
    FORMAT_L2_HEADER           = "(A10,A20)"
    FORMAT_ERROR               = "(F20.12,F20.12,F20.12,F20.12)"
    FORMAT_ERROR_HEADER        = "(A20, A20, A20,A20)"
    FORMAT_ROC                 = "(I10,F20.12)"
    FORMAT_ROC_HEADER          = ("(A10,A20)")

    finiteDiffFlag            = FDfac ! from FDfac loop
    !!include statements with inputs needed for SwirlClassType

    eigenValueMMS = CMPLX(0,0,KIND=rDef)!frequency*r_max

    facCount = 0 ! initializer for far count

    ALLOCATE( &
        RateOfConvergence1(numberOfIterations - 1) , &
        RateOfConvergence2(numberOfIterations - 1) , &
        S_L2Array(numberOfIterations)         , &
        SoundSpeedL2Array(numberOfIterations) , &
        drArray(numberOfIterations)                               , &
        numberOfGridPointsArray(numberOfIterations))

    DO FDfac = 1, numberOfFiniteDifferenceSchemes
        DO fac = 1, numberOfIterations
            WRITE(0,*) fac 

            finiteDiffFlag            = FDfac ! from FDfac loop

            IF (numericalIntegrationFlag.eq.1) THEN
                ExpectedRateOfConvergenceSoundSpeed = 2.0_rDef
            ELSEIF (numericalIntegrationFlag.eq.2) THEN
                ExpectedRateOfConvergenceSoundSpeed = 4.0_rDef
            ENDIF

            IF (finiteDiffFlag.eq.1) THEN
                ExpectedRateOfConvergenceSourceTerm = 2.0_rDef
            ELSEIF (finiteDiffFlag.eq.2) THEN
                ExpectedRateOfConvergenceSourceTerm = 4.0_rDef
            ENDIF

            facCount                     = facCount + 1
            numberOfGridPoints           = 1+(2**fac)*M_int
            numberOfGridPointsArray(fac) = numberOfGridPoints
            dr                           = (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)
            drArray(fac) = dr
            IF (facCount .gt. 1) THEN
                gridSpacingRatio =drArray(fac)/drArray(fac-1) 
            END IF

            ALLOCATE(&
                axialMachDataMMSOut(numberOfGridPoints)       ,& !M_x
                thetaMachDataMMSOut(numberOfGridPoints)     ,& !M_Th
                axialMach_drMMSOut(numberOfGridPoints), &
                thetaMach_drMMSOut(numberOfGridPoints), &
                SoundSpeedOut(numberOfGridPoints) , &
                SoundSpeed_drOut(numberOfGridPoints), &
                r(numberOfGridPoints)                                            , &
                rOut(numberOfGridPoints) , &
                thetaMachDataMMS(numberOfGridPoints)                                , &
                axialMachDataMMS(numberOfGridPoints)                                , &
                speedOfSoundMMS(numberOfGridPoints), &
                SoundSpeedError(numberOfGridPoints) , &
                vR(numberOfGridPoints), &
                vX(numberOfGridPoints), &
                vTh(numberOfGridPoints), &
                Pr(numberOfGridPoints), &
                S_1(numberOfGridPoints), &
                S_2(numberOfGridPoints), &
                S_3(numberOfGridPoints), &
                S_4(numberOfGridPoints), &
                S_MMS(numberOfGridPoints*4) , &
                S_error(numberOfGridPoints*4) , &
                S_actual(numberOfGridPoints*4) , &
                eigenVectorMMS(numberOfGridPoints*4),&
                thetaMachData(numberOfGridPoints)                                , &
                axialMachData(numberOfGridPoints)                                , &
                totalMachData(numberOfGridPoints))

            DO i = 1, numberOfGridPoints

                r(i)             = (r_min+REAL(i-1, rDef)*dr)/r_max

                ! Plug flow for T4.1
                axialMachData(i) = 0.3_rDef

                thetaMachData(i) = 0.0_rDef

                ! T.4.5 profile (sheared flow
!                axialMachData(i) = 0.3_rDef*(1.0_rDef - 2.0_rDef*ABS(&
!                    (r_min - r(i))/(1.0_rDef/7.0_rDef) + 0.5_rDef))**(1.0_rDef/7.0_rDef)
!                thetaMachData(i) = 0.0_rDef

            END DO
!
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
            !Create a swirl Class Obj for a given flow
            ! CALL CreateObject(&
            !     object        = swirlClassObj(fac)  ,&
            !     azimuthalMode = azimuthalModeNumber  ,&
            !     np            = numberOfGridPoints   ,&
            !     sig           = hubToTipRatio        ,&
            !     axialMachData = axialMachData        ,&
            !     tangentialMachData = thetaMachData        ,&
            !     ak            = frequency            ,&
            !     etah          = hubAdmittance        ,&
            !     etad          = ductAdmittance       ,&
            !     ifdff         = finiteDiffFlag       )

            ! CALL runSwirlClassMethods(&
            !     object = swirlClassObj(fac))

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
                ifdff         = finiteDiffFlag       )

            CALL runSwirlClassMethods(&
                object = swirlClassObjMMS(fac))

            CALL GetMeanFlowData(&
                object      = swirlClassObjMMS(fac),&
                axialMach   = axialMachDataMMSOut, &
                thetaMach    = thetaMachDataMMSOut, &
                axialMach_dr = axialMach_drMMSOut, &
                thetaMach_dr = thetaMach_drMMSOut, &
                SoundSpeed = SoundSpeedOut   , &
                SoundSpeed_dr = SoundSpeed_drOut, &
                radialData = rOut)

            CALL FindResidualData(&
                object = swirlClassObjMMS(fac),&
                eigenVector = eigenVectorMMS, &
                eigenValue =  eigenValueMMS, &
                S =  S_actual)


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

            CALL getL2Norm(&
                object   = SoundSpeedMMS_ClassObj , &
                L2       = SoundSpeedErrorL2 , &
                dataSet1 = speedOfSoundMMS , &
                dataSet2 = SoundSpeedOut)

            
            CALL getL2Norm(&
                object      = SourceTermMMS_ClassObj , &
                L2          = S_L2 , &
                dataSet1    = S_MMS, &
                dataSet2    = S_actual)

            S_L2Array(fac) = REAL(S_L2, KIND = rDef)
            SoundSpeedL2Array(fac) = SoundSpeedErrorL2

            include 'main-scripts/swirl-data-export-per-grid-MMS.f90'
            ! include 'main-scripts/swirl-data-export-per-grid.f90'

            CALL DestroyObject(object = swirlClassObjMMS(fac))

            ! CALL DestroyObject(object = swirlClassObj(fac))
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

        END DO

    CALL getRateOfConvergence(&
        object            = SoundSpeedMMS_ClassObj , &
        ExpectedRateOfConvergence = ExpectedRateOfConvergenceSoundSpeed   , &
        RateOfConvergence = RateOfConvergence1 , &
        L2Array           = SoundSpeedL2Array, &
        gridSpacingRatio = gridSpacingRatio)

     CALL getRateOfConvergence(&
         object            = SourceTermMMS_ClassObj, &
         RateOfConvergence = RateOfConvergence2 , &
         L2Array           = S_L2Array)

    include 'main-scripts/swirl-data-export-MMS.f90'

    END DO
    DEALLOCATE(&
        RateOfConvergence1 , &
        RateOfConvergence2 , &
        S_L2Array)

    CALL CPU_TIME(end_time)
    if ((end_time-start_time) .lt. 60.0_rDef) THEN
        WRITE(0,*) 'SWIRL''s run time:', (end_time-start_time), 'seconds'!/60.0_rDef
    ELSE
        WRITE(0,*) 'SWIRL''s run time:', (end_time-start_time)/60.0_rDef, 'minutes'
    endif
END PROGRAM MAIN
