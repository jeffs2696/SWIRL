PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject
    USE mmsClassObject
    USE SourceTermModule

    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64, &
        numberOfIterations = 10 

    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: swirlClassObj
    TYPE(mmsClassType)  :: SoundSpeedMMS_ClassObj , SourceTermMMS_ClassObj

    INTEGER  :: &
        UNIT               ,& ! for NEWUNIT
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
        S_MMS                                        , &
        S_1                                          , &
        S_2                                          , &
        S_3                                          , &
        S_4                                          , &
        S_array                                      , &
        S_error                                      , &
        S_L2Array                                    , &
        eigenVector                                  , &
        eigenVectorMMS

    COMPLEX(KIND = rDef) :: &
        frequency          ,& !non-dimensional frequency
        hubAdmittance      ,& !Liner Admittance At the Hub
        ductAdmittance     ,&
        ci                 ,&
!        eigL2              ,& 
       S_L2              ,& 
        eigenValueMMS      !,& eigenValue        

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
        vR                  ,&
        vT                  ,&
        vX                  ,&
        Pr                  ,&
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
        file_name

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
    boundingConstant = 1.0_rDef
    eigenIndex = 1

    ALLOCATE(&
        k(7) , &
        S_L2Array(numberOfIterations)              , &
        SoundSpeedL2Array(numberOfIterations)       ,&
        RateOfConvergence1(numberOfIterations - 1) , &
        RateOfConvergence2(numberOfIterations - 1) )

    k(1) = CMPLX(0.0040, 0.0, rDef)
    k(2) = CMPLX(10.0, 0.0, rDef)
    k(3) = CMPLX(0.501, .0, rDef)
    k(4) = CMPLX(0.1, 0.0, rDef)
    k(5) = CMPLX(0.2, 0.0, rDef)
    k(6) = CMPLX(0.5, 0.0, rDef)
    k(7) = CMPLX(0.10, 0.0, rDef)

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

        DO i = 1, numberOfGridPoints

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

            vR = COS(REAL(k(4),KIND = rDef)*(r(i)-r_max))
            vT = COS(REAL(k(5),KIND = rDef)*(r(i)-r_max))
            vX = COS(REAL(k(6),KIND = rDef)*(r(i)-r_max))
            Pr = COS(REAL(k(7),KIND = rDef)*(r(i)-r_max))

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

        ! get Mean Flow Data that was used as input 
        ! (as a sanity check) and the results from
        ! SWIRL that required the mean flow.
        CALL GetMeanFlowData(&
            object          = swirlClassObj(fac), &
            axialMach       = axialMachDataOut, &
            thetaMach       = thetaMachDataOut, &
            axialMach_dr    = axialMachData_dr_Out, &
            thetaMach_dr    = thetaMachData_dr_Out, &
            SoundSpeed      = SoundSpeedOut, &
            SoundSpeed_dr   = SoundSpeed_dr_Out, &
            radialData      = rOut)

        ! Write the resulting mean flow
        WRITE(UNIT,FORMAT_MEAN_FLOW_HEADERS) &
            'radius','M_x','M_theta','A_expected','A_actual'

        DO i = 1,numberOfGridPoints

            SoundSpeedError(i) = ABS(SoundSpeedOut(i)-SoundSpeedExpected(i))

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

        CLOSE(UNIT)

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

        eigenValueMMS = CMPLX(0.0_rDef,00.00_rDef,KIND=rDef)

        CALL FindResidualData(&
            object      = swirlClassObj(fac),&
            eigenVector = eigenVectorMMS       ,&
            eigenValue  = eigenValueMMS        ,&
            S           = S_MMS )

        DO i = 1,numberOfGridPoints

            CALL getMMSSourceTerms( &
                gam   = eigenValueMMS              ,& !WE NEED TO extract modal data to get the axial wavenumber here
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

            S_array(i)                      = S_1(i)
            S_array(i+numberOfGridPoints)   = S_2(i)
            S_array(i+2*numberOfGridPoints) = S_3(i)
            S_array(i+3*numberOfGridPoints) = S_4(i)

        ENDDO

        S_error = ABS(S_array - S_MMS)

        CALL getL2Norm(&
            object    = SourceTermMMS_ClassObj,&
            L2        = S_L2 ,&
            dataSet1  = S_MMS,&
            dataSet2  = S_array)

        S_L2Array(fac) = S_L2   

        CALL DestroyObject(object = swirlClassObj(fac))

        file_name = 'SoundSpeedError.dat'

        OPEN(NEWUNIT=UNIT,FILE=file_name)

        WRITE(UNIT,*) 'Grid Points' , 'Speed of Sound Error'

        DO i = 1,numberOfGridPoints
            WRITE(UNIT,*) r(i) , SoundSpeedError(i)
        END DO

        CLOSE(UNIT);

        file_name = 'SourceTermData1.dat'

        OPEN(NEWUNIT=UNIT,FILE=file_name)

        WRITE(UNIT,*) 'Grid Points' , 'S_Actual' ,'S_MMS' ,'Source Term Error'

        DO i = 1,numberOfGridPoints
            WRITE(UNIT,*) &
                i, &
                REAL(S_array(i),KIND=rDef), &
                REAL(S_MMS(i),KIND=rDef)  , &
                REAL(S_error(i),KIND=rDef)
        END DO

        CLOSE(UNIT);
        file_name = 'SourceTermData2.dat'

        OPEN(NEWUNIT=UNIT,FILE=file_name)

        WRITE(UNIT,*) 'Grid Points' , 'S_Actual' , 'S_MMS' ,'Source Term Error'

        DO i = numberOfGridPoints,numberOfGridPoints*2
            WRITE(UNIT,*) &
                i, &
                REAL(S_array(i),KIND=rDef), &
                REAL(S_MMS(i),KIND=rDef)  , &
                REAL(S_error(i),KIND=rDef)
        END DO

        CLOSE(UNIT);

        file_name = 'SourceTermData3.dat'

        OPEN(NEWUNIT=UNIT,FILE=file_name)

        WRITE(UNIT,*) 'Grid Points' , 'S_Actual' , 'S_MMS' ,'Source Term Error'

        DO i = numberOfGridPoints*2,numberOfGridPoints*3
            WRITE(UNIT,*) &
            i, &
            REAL(S_array(i),KIND=rDef), &
            REAL(S_MMS(i),KIND=rDef)  , &
            REAL(S_error(i),KIND=rDef)
    END DO

        CLOSE(UNIT);

        file_name = 'SourceTermData4.dat'

        OPEN(NEWUNIT=UNIT,FILE=file_name)

        WRITE(UNIT,*) 'Grid Points' , 'S_Actual' , 'S_MMS' ,'Source Term Error'

        DO i = numberOfGridPoints*3,numberOfGridPoints*4
            WRITE(UNIT,*) &
                i, &
                REAL(S_array(i),KIND=rDef), &
                REAL(S_MMS(i),KIND=rDef)  , &
                REAL(S_error(i),KIND=rDef)
        END DO

        CLOSE(UNIT);

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

    ! Should I put the data writing in another script? : JS
    file_name ='L2OfSoundSpeed.dat'

    OPEN(NEWUNIT=UNIT,FILE=file_name)

    WRITE(UNIT,*) 'Grid Points' , 'L2 of Speed of Sound'

    DO i = 1,numberOfIterations
        WRITE(UNIT,*) 1+2**i , SoundSpeedL2Array(i)
    END DO

    CLOSE(UNIT);

    IF (debug) THEN
        WRITE(0,*) 'Grid Points' , 'L2 of Speed of Sound'

        DO i = 1,numberOfIterations
            WRITE(0,*) 1+2**i , SoundSpeedL2Array(i)
        END DO

    ENDIF

    file_name ='L2OfSourceTerm.dat'

    OPEN(NEWUNIT=UNIT,FILE=file_name)

    WRITE(UNIT,*) 'Grid Points' , 'L2 of Source Term'

    DO i = 1,numberOfIterations
        WRITE(UNIT,*) 1+2**i , REAL(S_L2Array(i),KIND=rDef)
    END DO

    CLOSE(UNIT);

    IF (debug) THEN
        WRITE(0,*) 'Grid Points' , 'L2 of Source Term'

    DO i = 1,numberOfIterations
        WRITE(0,*) 1+2**i , REAL(S_L2Array(i),KIND=rDef)
    END DO

    ELSE
    END IF
    file_name = 'RateOfConvergenceForIntegration.dat'

    OPEN(NEWUNIT=UNIT,FILE=file_name)

    WRITE(UNIT,*) 'Rate Of Convergence'

    CALL getRateOfConvergence(&
        object            = SoundSpeedMMS_ClassObj , &
        RateOfConvergence = RateOfConvergence1 , &
        L2Array           = SoundSpeedL2Array)

    WRITE(0,FORMAT_ROC_HEADERS) 'Delta r' , 'ROC'
    DO i = 1,numberOfIterations - 1

        WRITE(UNIT,FORMAT_ROC)  REAL(1+2**(i),KIND=rDef)/REAL(1+2**(i+1),KIND=rDef), RateOfConvergence1(i) 
        WRITE(0,FORMAT_ROC)     REAL(1+2**(i),KIND=rDef)/REAL(1+2**(i+1),KIND=rDef), RateOfConvergence1(i)

    ENDDO

    file_name = 'RateOfConvergenceForSourceTerm.dat'

    OPEN(NEWUNIT=UNIT,FILE=file_name)

    WRITE(UNIT,*) 'Rate Of Convergence'
    DO i = 1,numberOfIterations

        ! IF (S_L2Array(i) .eq. 0.0_rDef) THEN


        !     WRITE(6,*) 'Error: S_L2Array has a zero element at index',i
        !     STOP
        !     ELSE
        ! ENDIF
    ENDDO

    CALL getRateOfConvergence(&
        object            = SourceTermMMS_ClassObj, &
        RateOfConvergence = RateOfConvergence2 , &
        L2Array           = S_L2Array)

    WRITE(0,FORMAT_ROC_HEADERS) 'Delta r' , 'ROC'
    DO i = 1,numberOfIterations - 1

        WRITE(UNIT,FORMAT_ROC) &
            REAL(1+2**(i),KIND=rDef)/REAL(1+2**(i+1),KIND=rDef), &
            ABS(REAL(RateOfConvergence2(i),KIND=rDef))

        WRITE(0,FORMAT_ROC) &
            REAL(1+2**(i),KIND=rDef)/REAL(1+2**(i+1),KIND=rDef), &
            ABS(REAL(RateOfConvergence2(i),KIND=rDef))

    ENDDO

    CLOSE(UNIT)


    DEALLOCATE( &
        SoundSpeedL2Array ,&
        S_L2Array ,&
        RateOfConvergence1 ,&
        RateOfConvergence2)

END PROGRAM MAIN
