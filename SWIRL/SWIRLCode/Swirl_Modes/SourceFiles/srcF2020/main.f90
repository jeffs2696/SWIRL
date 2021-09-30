PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject               !Runs SWIRL for a given set of parameters
    USE mmsClassObject                 !Calculates L2Norm, L2Max, and Rate Of Convergence
    USE SourceTermModule               !Calculated the symbolic terms needed for MMS (used Python)

    IMPLICIT NONE

    INTEGER, PARAMETER :: &
        rDef = REAL64   , &
        numberOfIterations = 2

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
        i ,&!j               ,& ! indexer for do loops
        i1,&!j1            ,& ! indexer for do loops
        i2,&!j2            ,& ! indexer for do loops
        i3,&!j3            ,& ! indexer for do loops
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

    ! COMPLEX(KIND = rDef),DIMENSION(:,:), ALLOCATABLE :: &
    !     S_A_expected , &
    !     S_B_expected , &
    !     S_A_actual   , &
    !     S_B_actual  


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
        dir_name

    CHARACTER(50):: &
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

    CHARACTER(10):: file_id

! Code Starts Here!

    CONTINUE

    IF (debug) THEN

        WRITE(0, *) 'Number of Grid Study Iterations: ' , numberOfIterations

    ELSE
    ENDIF

    FORMAT_MEAN_FLOW           = "(F15.12,F15.12,F15.12,F15.12,F15.12)" 
    FORMAT_MEAN_FLOW_HEADER    = "(A15,A15,A15,A15,A15)" 
    FORMAT_PERTURB_VARS        = "(F16.12,F16.12,F16.12,F16.12,F16.12)"
    FORMAT_PERTURB_HEADER      = "(A12,A12,A12,A12,A12)"
    FORMAT_SOURCE_TERMS        = "( I4, F16.12, F16.12,F16.12)" 
    FORMAT_SOURCE_TERMS_HEADER = "( A5, A17, A17,A17)" 
    FORMAT_L2                  = "(I4,F16.12)" 
    FORMAT_L2_HEADER           = "(A5,A13)" 
    FORMAT_ERROR               = "(F16.12,F16.12)" 
    FORMAT_ERROR_HEADER        = "(A17,A17)" 
    FORMAT_ROC                 = "(F16.12,F16.12)" 
    FORMAT_ROC_HEADER          = "(A17,A15)" 

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
    k(3) = CMPLX(0.4, 0.0, rDef)  ! M_x 
    k(4) = CMPLX(1.0, 0.0, rDef)   !v_r  
    k(5) = CMPLX(0.100, 0.0, rDef)   !v_th
    k(6) = CMPLX(0.0, 0.0, rDef)   !v_X
    k(7) = CMPLX(1.00, 0.0, rDef)   !p

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

        ! * Comparing Source Terms *

        ! from swirlClassObj
        CALL FindResidualData(&
            object      = swirlClassObj(fac),&
            eigenVector = eigenVectorMMS       ,&
            eigenValue  = -ci*axialWavenumberMMS        ,&
            S           = S_actual )
        ! 

        DO i = 1,numberOfGridPoints

            ! CALL getMMSSourceTerms( &
            !     gam   = axialWavenumberMMS           ,& !WE NEED TO extract modal data to get the axial wavenumber here
            !     i     = ci                      ,&
            !     ak    = frequency               ,&
            !     k     = k                       ,&
            !     kappa = gam                     ,&
            !     m     = azimuthalModeNumber     ,&
            !     r     = r(i)                    ,&
            !     r2    = r2                      ,&
            !     r3    = r3                      ,&
            !     r_max = r_max                   ,&
            !     S_1   = S_1(i)                  ,&
            !     S_2   = S_2(i)                  ,&
            !     S_3   = S_3(i)                  ,&
            !     S_4   = S_4(i)     )             

            CALL getMMSSourceTermComponents( &
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
                S_4   = S_4(i)                  ,&
                S_A11 = S_A11(i)                ,&
                S_A12 = S_A12(i)                ,&
                S_A13 = S_A13(i)                ,&
                S_A14 = S_A14(i)                ,&
                S_A21 = S_A21(i)                ,&
                S_A22 = S_A22(i)                ,&
                S_A23 = S_A23(i)                ,&
                S_A24 = S_A24(i)                ,&
                S_A31 = S_A31(i)                ,&
                S_A32 = S_A32(i)                ,&
                S_A33 = S_A33(i)                ,&
                S_A34 = S_A34(i)                ,&
                S_A41 = S_A41(i)                ,&
                S_A42 = S_A42(i)                ,&
                S_A43 = S_A43(i)                ,&
                S_A44 = S_A44(i)                ,&
                S_B11 = S_B11(i)                ,&
                S_B12 = S_B12(i)                ,&
                S_B13 = S_B13(i)                ,&
                S_B14 = S_B14(i)                ,&
                S_B21 = S_B21(i)                ,&
                S_B22 = S_B22(i)                ,&
                S_B23 = S_B23(i)                ,&
                S_B24 = S_B24(i)                ,&
                S_B31 = S_B31(i)                ,&
                S_B32 = S_B32(i)                ,&
                S_B33 = S_B33(i)                ,&
                S_B34 = S_B34(i)                ,&
                S_B41 = S_B41(i)                ,&
                S_B42 = S_B42(i)                ,&
                S_B43 = S_B43(i)                ,&
                S_B44 = S_B44(i)                )                                      

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
