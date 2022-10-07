PROGRAM MAIN
    !! The main SWIRL code that generates the executable

    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject               ! Runs SWIRL for a given set of parameters
    USE mmsClassObject                 ! Calculates L2Norm, L2Max, and Rate Of Convergence
    USE SourceTermModule               ! Calculated the symbolic terms needed for MMS (used Python)

    IMPLICIT NONE

    ! Variable Definitions
    !! headers, formatters, and filename variables
    CHARACTER(50) :: &
        dir_name,&
        file_id , &
        FDfac_id, &
        numIter, &
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

    !! Logical "flag" for debugging, set to .TRUE. to print WRITE statements in the directory
    !! MMS flag to get approximated order of accuracy

    LOGICAL :: &
        debug   = .FALSE. , &
        MMSflag = .FALSE.

    !! Code parameters for double precision and number of iterations
    INTEGER, PARAMETER :: &
        M_int = 2 , & 
        numberOfFiniteDifferenceSchemes = 1 , &
        rDef = REAL64   , &
        numberOfIterations = 1 

    !! Integers for flags and loop indicies
    INTEGER  :: &
        UNIT , & ! for NEWUNIT
        finiteDiffFlag           ,& ! finite difference flag
        numericalIntegrationFlag ,& !  numerical integration flag
        numberOfGridPoints       ,& ! number of points
        azimuthalModeNumber      ,& ! mode order
        i                        ,& ! indexer for do loops
        fac                      ,& ! variable used for doubling grid points
        FDfac                    ,&
        facCount                    ! counts the outermost do loop

    !! Integer arrays to store the number of grid points
    INTEGER, DIMENSION(:), ALLOCATABLE :: &
        numberOfGridPointsArray, &
        gridSpacingArray

    REAL(KIND = REAL64) ::  &
        power_law                          , &
        M_int_new                          , &
        gam                                , &
        SoundSpeedErrorL2                  , &
        ExpectedRateOfConvergenceSoundSpeed, &
        ExpectedRateOfConvergenceSourceTerm, &
        r_min                              , &!radial grid locations
        r_max                              , &!radial grid locations
        max_axial_mach_number              , &
        secondOrderSmoother                , &!2nd order smoothing coefficient
        fourthOrderSmoother                , &!4th order smoothing coefficient
        dr                                 , &!radial grid spacing 
        gridSpacingRatio              ,& ! the difference between two grid spacing
        start_time                         , &
        end_time                           , &
        r_shankar                          , &
        r_max_shankar                      , & 
        plsmns                             , &
        sgn                                , &
        hubToTipRatio

    !! real values arrays
    REAL(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        drArray             , &
        r                   , & !radial grid locations
        rOut                , &
        vR, vX, vTh, Pr     , &
        SoundSpeedError     , &
        SoundSpeedOut       , &
        SoundSpeedL2Array   , &
        SoundSpeed_drOut    , &
        axialMachData       , & !M_x
        thetaMachData       , & !M_th
        totalMachData       , & !M_total = sqrt(M_x^2+M_th^2) 
        RateOfConvergence1  , & !! MMS variables
        speedOfSoundMMS     , &
        axialMachDataMMS    , & !M_x
        thetaMachDataMMS    , & !M_th
        axialMachDataMMSOut , & !M_x
        thetaMachDataMMSOut , & !M_th
        axialMach_drMMSOut  , &
        thetaMach_drMMSOut  , &
        axialVelocity       , &
        diff_axialVelocity  , &
        S_error

    COMPLEX(KIND=rDef) :: &
        S_L2            ,&
        S1_L2            ,&
        S2_L2            ,&
        S3_L2            ,&
        S4_L2            ,&
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
        S1_L2Array , &
        S2_L2Array , &
        S3_L2Array , &
        S4_L2Array , &
        RateOfConvergence2  , &
        eigenVectorMMS

    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: &
        swirlClassObj, &
        swirlClassObjMMS

    TYPE(mmsClassType) :: SoundSpeedMMS_ClassObj, SourceTermMMS_ClassObj

    CONTINUE

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
    FORMAT_ROC                 = "(F20.12,F20.12)"
    FORMAT_ROC_HEADER          = ("(A10,A20)")

    ! include 'InputVariables_MMS1.f90'
    include 'InputVariables_AnalyticalSolution_2.f90'
    ! include 'InputVariables_KousenTable4_3.f90'
    ! include 'InputVariables_KousenTable4_4.f90'
    ! include 'InputVariables_KousenTable4_5.f90'
    ! include 'InputVariables_KousenTable4_6.f90'
    ! include 'InputVariables_KousenFigure4_5.f90'

    hubToTipRatio              =  r_min/r_max

    finiteDiffFlag            = FDfac ! from FDfac loop

    !!include statements with inputs needed for SwirlClassType

    eigenValueMMS = CMPLX(0,-1,KIND=rDef)*frequency*r_max

    facCount = 0 ! initializer for far count

    ALLOCATE( &
        RateOfConvergence1(numberOfIterations - 1) , &
        RateOfConvergence2(numberOfIterations - 1) , &
        S_L2Array(numberOfIterations)         , &
        S1_L2Array(numberOfIterations)         , &
        S2_L2Array(numberOfIterations)         , &
        S3_L2Array(numberOfIterations)         , &
        S4_L2Array(numberOfIterations)         , &
        SoundSpeedL2Array(numberOfIterations) , &
        drArray(numberOfIterations)                               , &
        numberOfGridPointsArray(numberOfIterations))

    M_int_new = M_int

    numberOfGridPoints = 33 

    DO FDfac = 1,1! numberOfFiniteDifferenceSchemes 

    DO fac = 1, numberOfIterations

    IF (debug) THEN
        WRITE(0,*) 'Iteration ', fac
    ENDIF
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

    ! IF (fac .lt. 8) THEN

    !     numberOfGridPoints           = (1+(2**fac)*2)

    ! ELSEIF (fac .eq. numberOfIterations) THEN 
    !     numberOfGridPoints           = (1+(2**fac)*2)
    ! ELSE
    !     numberOfGridPoints           = (1+(2**fac)*2)
    ! ENDIF
    ! numberOfGridPoints = (1+(2**fac)*2)
    ! to double the number of grid points each iteration...

    IF (fac.gt.1) THEN
        numberOfGridPoints = (numberOfGridPoints + numberOfGridPoints)
    ELSE
    ENDIF

    numberOfGridPointsArray(fac) = numberOfGridPoints
    dr                           = (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)
    drArray(fac) = dr

    IF (facCount .gt. 1) THEN
        gridSpacingRatio = drArray(fac)/drArray(fac-1) 
        WRITE(0,*)  'number of grid points 2: ', numberOfGridPointsArray(fac)   ,'delta r', drArray(fac)
        WRITE(0,*)  'number of grid points 1: ', numberOfGridPointsArray(fac-1) , 'delta r' ,drArray(fac-1)
        WRITE(0,*)  'grid refinement ratio:   ' ,gridSpacingRatio
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
        eigenVectorMMS(numberOfGridPoints*4))

    ! Code Body

    ! Allocatables for include files


    ! include 'InputMeanFlow_KousenTable4_3.f90'
    ! include 'InputMeanFlow_KousenTable4_4.f90'
    ! include 'InputMeanFlow_KousenTable4_5.f90'
    ! include 'InputMeanFlow_KousenTable4_6.f90'

    ! include 'InputMeanFlow_KousenFigure4_5_NoSwirl.f90'
    ! include 'InputMeanFlow_KousenFigure4_5_Swirl.f90'
    include 'InputMeanFlow_AnalyticalSolution_1.f90'
    ! WRITE(0,*) axialMachData
    !Create a swirl Class Obj for a given flow

    CALL CreateObject(&
        object        = swirlClassObj(fac)  ,&
        azimuthalMode = azimuthalModeNumber  ,&
        np            = numberOfGridPoints   ,&
        sig           = hubToTipRatio        ,&
        axialMachData = axialMachData        ,&
        tangentialMachData = thetaMachData        ,&
        ak            = frequency            ,&
        etah          = hubAdmittance        ,&
        etad          = ductAdmittance       ,&
        ifdff         = finiteDiffFlag       ,&
        secondOrderSmoother = 0.0_rDef       ,&
        fourthOrderSmoother = 0.0_rDef       ,& 
        debugFlag     = debug                ,&
        test_name = 'Test1')


    CALL runSwirlClassMethods(&
        object = swirlClassObj(fac), &
        debugFlag = debug)

    CALL DestroyObject(object = swirlClassObj(fac))

    !!----------------------- MMS ----------------------------------------------
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

    ENDDO
    DEALLOCATE(&
        RateOfConvergence1 , &
        RateOfConvergence2 , &
        S1_L2Array         , &
        S2_L2Array         , &
        S3_L2Array         , &
        S4_L2Array         , &
        S_L2Array)
    CALL CPU_TIME(end_time)
    if ((end_time-start_time) .lt. 60.0_rDef) THEN
        WRITE(0,*) 'SWIRL''s run time:', (end_time-start_time), 'seconds'!/60.0_rDef
    ELSE
        WRITE(0,*) 'SWIRL''s run time:', (end_time-start_time)/60.0_rDef, 'minutes'
    endif
END PROGRAM MAIN
