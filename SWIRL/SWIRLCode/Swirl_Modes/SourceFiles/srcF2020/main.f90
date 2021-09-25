PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject               !Runs SWIRL for a given set of parameters
    USE mmsClassObject                 !Calculates L2Norm, L2Max, and Rate Of Convergence
    USE SourceTermModule               !Calculated the symbolic terms needed for MMS (used Python)

    IMPLICIT NONE

    INTEGER, PARAMETER :: &
        rDef = REAL64   , &
        numberOfIterations = 9


    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: &
        swirlClassObj

    TYPE(mmsClassType)  :: &
        SoundSpeedMMS_ClassObj , &
        SourceTermMMS_ClassObj

    INTEGER  :: &
        UNIT                ,& ! for NEWUNIT
        finiteDiffFlag      ,& ! finite difference flag
        azimuthalModeNumber ,& ! mode order
        numberOfGridPoints  ,& ! number of points
        i                   ,& ! indexer for do loops
        fac                 ,& ! variable used for doubling grid points
        eigenIndex          ,&
        facCount               ! counts the outermost do loop

    LOGICAL :: debug = .FALSE.!.TRUE.

    COMPLEX(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        k                                            , &
        S_eig                                        , &
        S_actual                                     , &
        S_1                                          , &
        S_2                                          , &
        S_3                                          , &
        S_4                                          , &
        S_Expected                                   , &
        S_error                                      , &
        S_L2Array                                    , &
        eigenVector                                  , &
        eigenVectorMMS

    COMPLEX(KIND = rDef) :: &
        frequency          ,& !non-dimensional frequency
        hubAdmittance      ,& !Liner Admittance At the Hub
        ductAdmittance     ,&
        ci                 ,&
        S_L2               ,& 
        axialWavenumberMMS              

    REAL(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        r                   ,& !radial grid locations
        axialMachData       ,& !M_x
        thetaMachData       ,& !M_th
        totalMachData       ,& !M_total = sqrt(M_x^2+M_th^2)
        SoundSpeedExpected  ,& !Based on Eqn 2.6 in Kousen's paper
        rOut                ,& !radial grid after it leaves swirlClassObj
        axialMachDataOut    ,& !M_x         after it leaves swirlClassObj
        thetaMachDataOut    ,& !M_th        after it leaves swirlClassObj 
        SoundSpeedOut       ,& !Sound Speed after it leaves swirlClassObj 
        axialMachData_dr_Out,& !dM_x/dr 
        thetaMachData_dr_Out,& !dM_th/dr
        SoundSpeed_dr_Out   ,& !dA/dr
        SoundSpeedError     ,& !eps_A
        SoundSpeedL2Array   ,& !array of L2norm (eps_A)
    ! Perturbation variables
        vR                  ,& !radial     velocity  
        vT                  ,& !tangential velocity
        vX                  ,& !axial velocity
        Pr                  ,& !pressure
        RateOfConvergence1  ,&
        RateOfConvergence2 

    REAL(KIND = REAL64) ::  &
        gam                      ,&
        gm1                      ,&
        secondOrderSmoother ,& !2nd order smoothing coefficient
        fourthOrderSmoother ,& !4th order smoothing coefficient
        boundingConstant    ,&
        dr                  ,&
        hubToTipRatio       ,&
        SoundSpeedErrorL2


    REAL(KIND = rDef), PARAMETER ::&
        r_min  = 0.20_rDef  ,&
        r_max  = 1.000_rDef  ,&
        r2     = 0.50_rDef  ,&
        r3     = 0.200_rDef

    CHARACTER(50) :: &
        file_name, &
        dir_name

    CHARACTER(50):: &
        FORMAT_MEAN_FLOW , &
        FORMAT_MEAN_FLOW_HEADERS , &
        FORMAT_L2        , &
        FORMAT_ROC       , &     
        FORMAT_ROC_HEADERS       

    CHARACTER(10):: file_id

! Code Starts Here!

    CONTINUE

    IF (debug) THEN

        WRITE(0, *) 'Number of Grid Study Iterations: ' , numberOfIterations

    ELSE
    ENDIF

    FORMAT_MEAN_FLOW = "(F15.12,F15.12,F15.12,F15.12,F15.12)" 
    FORMAT_MEAN_FLOW_HEADERS = "(A15,A15,A15,A15,A15)" 
    FORMAT_L2 = "(F12.5,F12.5,F12.5,F12.5,F12.5)" 
    FORMAT_ROC = "(F12.5,F12.5)" 
    FORMAT_ROC_HEADERS = "(A12,A12)" 

    ! inputs needed for SwirlClassType
    azimuthalModeNumber       = 1
    hubToTipRatio             = r_min/r_max
    frequency                 =  CMPLX(10.0, 0, rDef)
    hubAdmittance             =  CMPLX(0,0,rDef)!CMPLX(0.40, 0 ,rDef)
    ductAdmittance            =  CMPLX(0,0,rDef)!CMPLX(0.70,0,rDef)
    finiteDiffFlag            =  1
    secondOrderSmoother       =  0.0_rDef
    fourthOrderSmoother       =  0.0_rDef

    ! constants needed for calculations
    gam = 1.4_rDef               ! ratio of specific heats
    gm1 = gam-1.0_rDef

    ci  = CMPLX(0.0, 1.0, rDef)  !imaginary number

    ! constants for MMS module
    boundingConstant = 1.00_rDef
    eigenIndex = 1

    ALLOCATE(&
        k(7) , &
        S_L2Array(numberOfIterations)              , &
        SoundSpeedL2Array(numberOfIterations)       ,&
        RateOfConvergence1(numberOfIterations - 1) , &
        RateOfConvergence2(numberOfIterations - 1) )

    ! used for the following terms:
    k(1) = CMPLX(0.004, 0.0, rDef)
    k(2) = CMPLX(15.0, 0.0, rDef)
    k(3) = CMPLX(0.3, 0.0, rDef)  ! M_x 
    k(4) = CMPLX(1.0, 0.0, rDef)   !v_r  
    k(5) = CMPLX(0.0, 0.0, rDef)   !v_th
    k(6) = CMPLX(1.0, 0.0, rDef)   !v_X
    k(7) = CMPLX(0.001, 0.0, rDef)   !p

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
            S_Expected(numberOfGridPoints*4)            , &
            S_1(numberOfGridPoints)                  , &
            S_2(numberOfGridPoints)                  , &
            S_3(numberOfGridPoints)                  , &
            S_4(numberOfGridPoints)                  , &
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

        ! CALL getAxialMachData
        DO i = 1, numberOfGridPoints

            ! This is redundant! Include in python script
            axialMachData(i)  =&
                REAL(k(3), rDef)*&
                (boundingConstant)*&
                COS(REAL(k(3), rDef)*(r(i)-r_max))

        ENDDO

        ! from SourceTermModule
        CALL getSoundSpeed(&
            r                  = r                  , &
            r2                 = r2                 , &
            r3                 = r3                 , &
            r_max              = r_max              , &
            k                  = k                  , &
            kappa              = gam                , &
            SoundSpeedExpected = SoundSpeedExpected , &
            thetaMachData      = thetaMachData)

        CALL getPerturbationVariables(&
            k     = k     , &
            r     = r    , &
            r_max = r_max, &
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

        ! from swirlClassObj
        CALL FindResidualData(&
            object      = swirlClassObj(fac),&
            eigenVector = eigenVectorMMS       ,&
            eigenValue  = -ci*axialWavenumberMMS        ,&
            S           = S_actual )

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

            S_Expected(i)                      = S_1(i)
            S_Expected(i+numberOfGridPoints)   = S_2(i)
            S_Expected(i+2*numberOfGridPoints) = S_3(i)
            S_Expected(i+3*numberOfGridPoints) = S_4(i)

        ENDDO

        S_error = ABS(S_Expected - S_actual)

        CALL getL2Norm(&
            object    = SourceTermMMS_ClassObj,&
            L2        = S_L2 ,&
            dataSet1  = S_actual,&
            dataSet2  = S_Expected)

        S_L2Array(fac) = S_L2   

        CALL DestroyObject(object = swirlClassObj(fac))

        ! Export data 

        include 'swirl-data-export-per-grid.f90'
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
            S_Expected                 ,&
            S_eig                 ,&
            S_error                 ,&
            S_1                      ,&
            S_2                      ,&
            S_3                      ,&
            S_4                      ,&
            vR                    ,&
            vT                    ,&
            vX                    ,&
            Pr                    ,&
            eigenVector           ,&
            eigenVectorMMS)

    END DO

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
