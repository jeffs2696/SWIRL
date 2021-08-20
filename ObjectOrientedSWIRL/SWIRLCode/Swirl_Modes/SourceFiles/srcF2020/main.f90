PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject
    USE mmsClassObject
    USE L2NormModule
    USE SourceTermModule

    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64, &
        numberOfIterations = 7

    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: swirlClassObj
    TYPE(mmsClassType)  :: SoundSpeedMMS_ClassObj, SourceTermMMS_ClassObj

    INTEGER  :: &
        UNIT               ,& ! for NEWUNIT
        finiteDiffFlag      ,& ! finite difference flag
        azimuthalModeNumber ,& ! mode order
        numberOfGridPoints  ,& ! number of points
        i                   ,& ! indexer for do loops
        fac                 ,& ! variable used for doubling grid points
        eigenIndex          ,&
        facCount               ! counts the outermost do loop

    LOGICAL :: debug = .TRUE.!.FALSE.
    COMPLEX(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        k , &
        S_eig                                        , &
        S_MMS                                        , &
        S_1                                          , &
        S_2                                          , &
        S_3                                          , &
        S_4                                          , &
        S_array                                      , &
        S_L2Array                                   , &
        eigenVector                                  

    COMPLEX(KIND = rDef) :: &
        frequency          ,& !non-dimensional frequency
        hubAdmittance      ,& !Liner Admittance At the Hub
        ductAdmittance     ,&
        ci                 ,&
        eigL2              ,& 
        S_L2              ,& 
        eigenValue        

    REAL(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        r                   ,&
        axialMachData       ,&
        thetaMachData       ,&
        totalMachData       ,&
        SoundSpeedExpected  ,&
        rOut                ,&
        axialMachDataOut    ,&
        thetaMachDataOut    ,&
        SoundSpeedOut       ,&
        axialMachData_dr_Out    ,&
        thetaMachData_dr_Out    ,&
        SoundSpeed_dr_Out   ,&
        SoundSpeedError     ,&
        SoundSpeedL2Array   ,&
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
        r_max  = 1.00_rDef  

    CHARACTER(50) :: &
        file_name

    CHARACTER(50):: &
        FORMAT_MEAN_FLOW , &
        FORMAT_MEAN_FLOW_HEADERS , &
        FORMAT_L2        , &
        FORMAT_ROC       

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
    FORMAT_ROC = "(F12.5,F12.5,F12.5,F12.5,F12.5)" 


    ! inputs needed for SwirlClassType
    azimuthalModeNumber       = 2
    hubToTipRatio             = r_min/r_max
    frequency                 =  CMPLX(20.0, 0, rDef)
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
    boundingConstant = 0.1000_rDef
    eigenIndex = 3

    ALLOCATE(&
        k(7) , &
        S_L2Array(numberOfIterations)              , &
        SoundSpeedL2Array(numberOfIterations)       ,&
        RateOfConvergence1(numberOfIterations - 1) , &
        RateOfConvergence2(numberOfIterations - 1) )

    k(1) = CMPLX(0.1, 0.0, rDef)
    k(2) = CMPLX(0.1, 0.0, rDef)
    k(3) = CMPLX(0.1, 0.0, rDef)
    k(4) = CMPLX(0.2, 0.0, rDef)
    k(5) = CMPLX(0.1, 0.0, rDef)
    k(6) = CMPLX(0.1, 0.0, rDef)
    k(7) = CMPLX(0.1, 0.0, rDef)

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

        WRITE(file_id, '(i0)') numberOfGridPoints 

        file_name = 'MeanFlowData/MeanFlowData' // TRIM(ADJUSTL(file_id)) // '.dat'

        OPEN( NEWUNIT = UNIT, FILE = TRIM(file_name) )

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
            S_MMS(numberOfGridPoints*4)              , &
            S_array(numberOfGridPoints*4)            , &
            S_1(numberOfGridPoints)                  , &
            S_2(numberOfGridPoints)                  , &
            S_3(numberOfGridPoints)                  , &
            S_4(numberOfGridPoints)                  , &
            S_eig(numberOfGridPoints*4)              , &
            eigenVector(numberOfGridPoints*4)          ) 


        DO i = 1, numberOfGridPoints

            r(i) = (r_min+REAL(i-1, rDef)*dr)/r_max

        END DO

        DO i = 1, numberOfGridPoints

            axialMachData(i)  =&
                (boundingConstant)*&
                EXP(REAL(k(2), rDef)*(r(i)-r_max))

        ENDDO

        CALL getSoundSpeed(&
            r                  = r                  , &
            r_max              = r_max              , &
            k                  = k                  , &
            kappa              = gam                , &
            SoundSpeedExpected = SoundSpeedExpected , &
            thetaMachData      = thetaMachData)

        DO i = 1, numberOfGridPoints
            totalMachData(i)  =&
                ((axialMachData(i)**2.0_rDef+&
                thetaMachData(i)**2.0_rDef)**0.5_rDef)

            IF(totalMachData(i) > 1.0_rDef) THEN
                WRITE(0, *) i, 'ERROR: Total mach is greater than one'
                STOP
            ELSE

            ENDIF

        ENDDO

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

        ! get Mean Flow Data to Calculate MMS
        CALL GetMeanFlowData(&
            object          = swirlClassObj(fac), &
            axialMach       = axialMachDataOut, &
            thetaMach       = thetaMachDataOut, &
            axialMach_dr    = axialMachData_dr_Out, &
            thetaMach_dr    = thetaMachData_dr_Out, &
            SoundSpeed      = SoundSpeedOut, &
            SoundSpeed_dr   = SoundSpeed_dr_Out, &
            radialData      = rOut)

        CALL GetModeData(&
            object     = swirlClassObj(fac) , &
            eigenValue = eigenValue         , &
            eigenVector= eigenVector        , &
            eigenIndex = eigenIndex) 

        ! Write the resulting mean flow
        WRITE(UNIT,FORMAT_MEAN_FLOW_HEADERS) 'radius','M_x','M_theta','A_expected','A_actual'
        DO i = 1,numberOfGridPoints

            WRITE(UNIT,FORMAT_MEAN_FLOW) &
                rOut(i)                 , &
                axialMachDataOut(i)     , &
                thetaMachDataOut(i)     , &
                SoundSpeedExpected(i)   , &
                SoundSpeedOut(i)        

        ENDDO

        CALL getL2Norm(&
            object    = SoundSpeedMMS_ClassObj ,& 
            L2        = SoundSpeedErrorL2      ,&
            dataSet1  = SoundSpeedExpected     ,&
            dataSet2  = SoundSpeedOut           )

        SoundSpeedL2Array(fac) = SoundSpeedErrorL2

        IF (debug) THEN

            WRITE(6,*) 'SoundSpeedErrorL2' , SoundSpeedErrorL2

        ELSE
        ENDIF

        ! Sanity Check to make sure that the proper residual was calculated
        ! if S_MMS < Double Precision then the CALL was a success

        CALL FindResidualData(&
            object      = swirlClassObj(fac),&
            eigenValue  = eigenValue     , &
            eigenVector = eigenVector    , &
            S           = S_MMS )

        CALL FindResidualData(&
            object      = swirlClassObj(fac),&
            eigenValue  = eigenValue, &
            eigenVector = eigenVector, &
            S           = S_eig )

        do i = 1,numberOfGridPoints*4

            ! if the real component is less than machine precision . . .
            IF (REAL(S_MMS(i),rDef) < 1e-12_rDef) then

                S_MMS(i) = CMPLX(0.0_rDef,0.0_rDef,KIND=rDef)

            ELSE
            ENDIF
            WRITE(6,*) S_MMS(i),S_eig(i)

        enddo

        DO i = 1,numberOfGridPoints

            CALL getMMSSourceTerms( &
                gam   = eigenValue              ,& !WE NEED TO extract modal data to get the axial wavenumber here
                i     = ci                      ,&
                ak    = frequency               ,&
                k     = k                       ,&
                kappa = gam                     ,&
                m     = azimuthalModeNumber     ,&
                r     = r(i)                    ,&
                r_max = r_max                   ,&
                S_1   = S_1(i)                  ,&
                S_2   = S_2(i)                  ,&
                S_3   = S_3(i)                  ,&
                S_4   = S_4(i)     )             

            S_array(i)                      = S_1(i)
            S_array(i+numberOfGridPoints)   = S_2(i)
            S_array(i+2*numberOfGridPoints) = S_3(i)
            S_array(i+3*numberOfGridPoints) = S_4(i)

        ENDDO


        DO i = 1,numberOfGridPoints

            WRITE(6,*) (S_array(i)-S_MMS(i))

        ENDDO

        CALL getL2Norm(&
            L2        = eigL2,&
            dataSet  = S_MMS)

        CALL getL2Norm(&
            object    = SourceTermMMS_ClassObj,&
            L2        = S_L2 ,&
            dataSet1  = S_eig,&
            dataSet2  = S_array)

        S_L2Array(fac) = S_L2   

        IF (debug) THEN

            WRITE(6,*) S_L2

        ELSE
        ENDIF

        CALL DestroyObject(object = swirlClassObj(fac))

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
            S_MMS                 ,&
            S_array                 ,&
            S_eig                 ,&
            S_1                      ,&
            S_2                      ,&
            S_3                      ,&
            S_4                      ,&
            eigenVector           )

        CLOSE(UNIT)


    END DO

    ! Should I put the data writing in another script? : JS
    file_name ='L2OfSoundSpeed.dat'

    OPEN(NEWUNIT=UNIT,FILE=file_name)

    WRITE(UNIT,*) 'Grid Points' , 'L2 of Speed of Sound'

    DO i = 1,numberOfIterations
        WRITE(UNIT,*) 1+2**i , SoundSpeedL2Array(i)
    END DO

    CLOSE(UNIT);

    file_name = 'RateOfConvergenceForIntegration.dat'

    OPEN(NEWUNIT=UNIT,FILE=file_name)

    WRITE(UNIT,*) 'Rate Of Convergence'

    CALL getRateOfConvergence(&
        object            = SoundSpeedMMS_ClassObj , &
        RateOfConvergence = RateOfConvergence1 , &
        L2Array           = SoundSpeedL2Array)

    DO i = 1,numberOfIterations - 1

            WRITE(UNIT,*) (r_max - r_min)/REAL(1+2**i,KIND=rDef), RateOfConvergence1(i)
            WRITE(6,*) (r_max - r_min)/REAL(1+2**i,KIND=rDef), RateOfConvergence1(i)

    ENDDO

    DO i = 1,numberOfIterations - 1

        RateOfConvergence2(i) = &
            (&
            LOG(REAL(S_L2Array(i+1),KIND=rDef)) -&
            LOG(REAL(S_L2Array(i  ),KIND=rDef))&
            )&
            /&
            LOG(0.50_rDef) ! change 0.5 so that way the grid spacing doesnt have to half as big between iterations JS

        IF (debug) THEN 

            WRITE(6,*) (r_max - r_min)/REAL(1+2**i,KIND=rDef), RateOfConvergence2(i)

        ELSE
        ENDIF

    ENDDO

    CLOSE(UNIT)

    DEALLOCATE( &
        SoundSpeedL2Array ,&
        S_L2Array ,&
        RateOfConvergence1 ,&
        RateOfConvergence2)

END PROGRAM MAIN
