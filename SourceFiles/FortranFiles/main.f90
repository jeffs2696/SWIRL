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
        variableGridSpacing = .FALSE. , &
        debug   = .FALSE. , &
        MMSflag = .FALSE. , &
        MMSdebug_flag = .FALSE. 

    !! Code parameters for double precision and number of iterations
    INTEGER, PARAMETER :: &
        M_int = 60 , & 
        numberOfFiniteDifferenceSchemes = 1 , &
        rDef = REAL64   , &
        numberOfIterations = 2 

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
        facCount                 ,& ! counts the outermost do loop
        numberOfRadialModes

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
        S_eigcheck_L2,&
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

    ! Use include statements to define mean flow and test case parameters

    ! use this flow for propoer MMS calculation!
    ! include 'InputVariables_MMS1.f90'

    include 'InputVariables_AnalyticalSolution_1.f90'
    ! include 'InputVariables_KousenTable4_3.f90'
    ! include 'InputVariables_KousenTable4_4.f90'
    ! include 'InputVariables_KousenTable4_5.f90'
    ! include 'InputVariables_KousenTable4_6.f90'
    ! include 'InputVariables_KousenFigure4_5.f90'

    hubToTipRatio             = r_min/r_max


    !!include statements with inputs needed for SwirlClassType

    eigenValueMMS = CMPLX(0,-1,KIND=rDef)*frequency*r_max

    facCount = 0 ! initializer for fac count

    ALLOCATE( &
        ! S_eigcheck_L2(numberOfIterations - 1) , &
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

    !starting range
    ! what if the user didn't have to define this
    ! numberOfGridPoints =1+(2**fac)*M_int ! 

    numberOfGridPoints = 81 
    DO FDfac = 2,2! numberOfFiniteDifferenceSchemes 

    DO fac = 1, numberOfIterations

    IF (debug) THEN
        WRITE(0,*) 'Iteration ', fac
    ENDIF

    finiteDiffFlag            = FDfac ! from FDfac loop

    ! Can this be done from within the SwirlClassObject?

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


    ! numberOfGridPoints = (1+(2**fac)*M_int)

    facCount                     = facCount + 1

    ! to double the number of grid points each iteration...

    ! IF (numberOfIterations.gt.1) THEN
        IF (variableGridSpacing) THEN
            IF (facCount.gt.0) THEN
                ! it really should be if the comp time is larger than a second or something
                ! oR if the grid gets too big
                ! using base 2 has the effect of doubling
                numberOfGridPoints = 1 + (2**fac)*(M_int-(3+facCount))
                ! ELSEIF (fac.gt.2) THEN
                ! numberOfGridPoints = NINT((0.5**fac)*(M_int)) !+ numberOfGridPoints
                ! ELSEIF (fac.gt.4) THEN
                !     numberOfGridPoints = ((2**fac)*(M_int)) + numberOfGridPoints/3 
            ENDIF
        ELSE
            numberOfGridPoints = numberOfGridPoints + 40!1 + (2**fac)*M_int
        ENDIF
    ! ENDIF

    numberOfGridPointsArray(fac) = numberOfGridPoints
    dr                           = (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)
    drArray(fac) = dr

    IF (facCount .gt. 1) THEN
        gridSpacingRatio = drArray(fac)/drArray(fac-1) 
        WRITE(0,*)  'number of grid points 2: ', numberOfGridPointsArray(fac)   ,'delta r', drArray(fac)
        WRITE(0,*)  'number of grid points 1: ', numberOfGridPointsArray(fac-1) , 'delta r' ,drArray(fac-1)
        WRITE(0,*)  'grid refinement ratio:   ' ,gridSpacingRatio, 1/gridSpacingRatio
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
    !Create a swirl Class Obj for a given flow

    numberOfRadialModes = 3

    CALL CreateObject(&
        object        = swirlClassObj(fac)  ,&
        azimuthalMode = azimuthalModeNumber  ,&
        numberOfRadialModes = numberOfRadialModes ,&
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
        debugFlag = debug          , &
        MMSflag = MMSflag)

    CALL GetAnalyiticModeShape(&
        object = swirlClassObj(fac))

    CALL NormalizeModeShape(&
        object = swirlClassObj(fac))
    CALL CompareModeShapes(&
        object = swirlClassObj(fac))

    CALL CutOnResiduialCheck(&
        object = swirlClassObj(fac))!, &
        ! S_eigcheck_L2     = S_eigcheck_L2(fac))
    CALL DestroyObject(object = swirlClassObj(fac))

    !!----------------------- MMS ----------------------------------------------
    !! Uses the MMSflag variable to run MMS. Set MMSflag = .TRUE. to run
    include 'include_mms_main.f90'

    ENDDO
    WRITE(0,*) S_eigcheck_L2
    DEALLOCATE(&
        ! S_eigcheck_L2      , &
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
