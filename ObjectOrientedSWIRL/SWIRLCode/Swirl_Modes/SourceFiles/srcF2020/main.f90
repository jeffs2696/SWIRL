PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject
    USE L2NormModule

    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64, &
        numberOfIterations = 9

    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: swirlClassObj

    INTEGER  :: &
        UNIT               ,& ! for NEWUNIT
        finiteDiffFlag      ,& ! finite difference flag
        azimuthalModeNumber ,& ! mode order
        numberOfGridPoints  ,& ! number of points
        i                   ,& ! indexer for do loops
        fac                 ,& ! variable used for doubling grid points
        facCount               ! counts the outermost do loop

    COMPLEX(KIND = rDef) :: &
        frequency                ,& !non-dimensional frequency
        hubAdmittance            ,& !Liner Admittance At the Hub
        ductAdmittance           ,&
        ci                       ,&
        k_1                      ,&
        k_2                      ,&
        k_3                      ,&
        k_4                      ,&
        k_5                      ,&
        k_6                      ,&
        k_7

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
        RateOfConvergence

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
        radMin  = 0.20_rDef  ,&
        radMax  = 1.00_rDef  

    CHARACTER(50) :: &
        file_name

    CHARACTER(30):: FORMAT

    CHARACTER(10):: file_id

!
! Code Starts Here!

    CONTINUE

    FORMAT = "(F12.5,F12.5,F12.5,F12.5)" ! General format for numerical

    ci  = CMPLX(0.0, 1.0, rDef)  ! imaginary number

    ! inputs needed for SwirlClassType
    azimuthalModeNumber       = 2
    hubToTipRatio             = radMin/radMax
    frequency                 =  CMPLX(20.0, 0, rDef)
    hubAdmittance             =  CMPLX(0.40, 0 ,rDef)
    ductAdmittance            =  CMPLX(0.70,0,rDef)
    finiteDiffFlag            =  1
    secondOrderSmoother       =  0.0_rDef
    fourthOrderSmoother       =  0.0_rDef

    ! constants needed for calculations
    gam = 1.4_rDef       ! ratio of specific heats
    gm1 = gam-1.0_rDef


    ! constants for MMS module
    boundingConstant = 0.1000_rDef
    k_1 = CMPLX(0.2, 0.0, rDef)
    k_2 = CMPLX(1.0, 0.0, rDef)
    k_3 = CMPLX(0.4, 0.0, rDef)
    k_4 = CMPLX(0.2, 0.0, rDef)
    k_5 = CMPLX(0.0, 0.0, rDef)
    k_6 = CMPLX(0.0, 0.0, rDef)
    k_7 = CMPLX(0.0, 0.0, rDef)

    ! Starting Grid DO LOOP


    WRITE(6, *) 'Number of Grid Study Iterations: ' , numberOfIterations

    ALLOCATE(&
        SoundSpeedL2Array(numberOfIterations)       ,&
        RateOfConvergence(numberOfIterations - 1) )

    facCount = 0 ! initializer for fac count

    DO fac = 1, numberOfIterations

        facCount = facCount + 1
        numberOfGridPoints   = 1+(2**fac)

        ! write integer into a string 
        WRITE(file_id, '(i0)') numberOfGridPoints
        ! Construct the file name 
        file_name = 'MeanFlowData' // TRIM(ADJUSTL(file_id)) // '.dat'

        OPEN(NEWUNIT = UNIT, FILE = TRIM(file_name) )

        
        WRITE(6, *) '# Grid Points ',  numberOfGridPoints

        ALLOCATE(&
            r(numberOfGridPoints)                                         ,&
            rOut(numberOfGridPoints)                                         ,&
            thetaMachData(numberOfGridPoints)                             ,&
            thetaMachDataOut(numberOfGridPoints)                             ,&
            thetaMachData_dr_Out(numberOfGridPoints)                             ,&
            axialMachData(numberOfGridPoints)                             ,&
            axialMachDataOut(numberOfGridPoints)                             ,&
            axialMachData_dr_Out(numberOfGridPoints)                             ,&
            totalMachData(numberOfGridPoints)                             ,&
            SoundSpeedOut(numberOfGridPoints)           ,&
            SoundSpeed_dr_Out(numberOfGridPoints)       ,&
            SoundSpeedExpected(numberOfGridPoints)                                ,&
            SoundSpeedError(numberOfGridPoints) ,&
            )

        dr = (radMax-radMin)/REAL(numberOfGridPoints-1, rDef)

        DO i = 1, numberOfGridPoints

            r(i) = (radMin+REAL(i-1, rDef)*dr)/radMax

        END DO

        ! WRITE(6,*) ' '
        ! WRITE(6,'(a)') 'Table 1: Input Flow Data'
        ! WRITE(6,'(12a12,12a5,12a12,12a12)') 'Radius', 'Mx' , 'M_theta', 'A_expected'

        DO i = 1, numberOfGridPoints

            ! this segment of the code is defining a flow that will allow
            ! us to know what the speed of sound is expected to be
            axialMachData(i)  =&
                (boundingConstant)*&
                EXP(REAL(k_2, rDef)*(r(i)-radMax))

            thetaMachData(i) = &
                SQRT(2.0_rDef)*&
                SQRT(-(REAL(k_3,rDef)*r(i)*&
                SIN(REAL(k_3,rDef)*(r(i)-radMax)))/&
                (REAL(gm1,rDef)*COS(REAL(k_3,rDef)*(r(i)-radMax))))

            SoundSpeedExpected(i) = &
                COS(REAL(k_3,rDef)*(r(i)-radMax))! EXP(REAL(k_3, rDef)*(r(i)-r(numberOfGridPoints)))
            ! thetaMachData(i)  = SQRT((r(i)*REAL(k_3, rDef)*2.0_rDef)/REAL(gm1, rDef))  ! EXP(k_2*r(i))
            ! thetaMachData(i)  = 0.0_rDef!EXP(k_2*r(i))
            ! the sound speed we expect given the M_theta (for MMS)

            totalMachData(i)  =&
                ((axialMachData(i)**2.0_rDef+&
                thetaMachData(i)**2.0_rDef)**0.5_rDef)

            IF(totalMachData(i) > 1.0_rDef) THEN
                WRITE(6, *) i, 'ERROR: Total mach is greater than one'
                STOP
            ELSE

                ! WRITE(6, FORMAT) r(i), axialMachData(i), thetaMachData(i), SoundSpeedExpected(i)
                ! WRITE(myunit, FORMAT) r(i), axialMachData(i), thetaMachData(i), SoundSpeedExpected(i)

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

        ! WRITE(6,*) 'Input-Output Comparsion'
        
        ! WRITE(6,*) 'radius,M_x,M_theta,A_expected,A_actual'
        DO i = 1,numberOfGridPoints

            WRITE(UNIT,*) &
                rOut(i)                 , &
                axialMachDataOut(i)     , &
                thetaMachDataOut(i)     , &
                SoundSpeedExpected(i)   , &
                SoundSpeedOut(i)        

            ! WRITE(6,*) &
            !     rOut(i)                 , &
            !     axialMachDataOut(i)     , &
            !     thetaMachDataOut(i)     , &
            !     SoundSpeedExpected(i)   , &
            !     SoundSpeedOut(i)        

            ! WRITE(6,FORMAT) &
            !     r(i)                 , &
            !     axialMachData(i)     , &
            !     thetaMachData(i)     , &
            !     ! axialMachData_dr_Out(i) , &
            !     ! thetaMachData_dr_Out(i) , &
            !     SoundSpeedExpected(i)   , &
            !     SoundSpeedOut(i)        
                ! SoundSpeed_dr_Out        

        ENDDO


        CALL getL2Norm(&
            L2        = SoundSpeedErrorL2  ,&
            dataSet1  = SoundSpeedExpected ,&
            dataSet2  = SoundSpeedOut      )

        SoundSpeedL2Array(fac) = SoundSpeedErrorL2

        WRITE(6,*) 'SoundSpeedErrorL2' , SoundSpeedErrorL2


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
            SoundSpeedError                   )

        CLOSE(UNIT)


    END DO

    ! WRITE(6,*) 'Grid Points' , 'L2 of Speed of Sound'
    OPEN(NEWUNIT=UNIT,FILE='RateOfConvergenceForIntegration')
    WRITE(UNIT,*) 'Grid Points' , 'L2 of Speed of Sound'

    DO i = 1,numberOfIterations
        WRITE(UNIT,*) 1+2**i , SoundSpeedL2Array(i)
    END DO

    WRITE(UNIT,*) 'Rate Of Convergence'
    DO i = 1,numberOfIterations - 1

        RateOfConvergence(i) = &
            (&
            LOG(SoundSpeedL2Array(i+1)) -&
            LOG(SoundSpeedL2Array(i  ))&
            )&
            /&
            LOG(0.5_rDef)

        WRITE(UNIT,*) RateOfConvergence(i)

    ENDDO

    CLOSE(UNIT)
    DEALLOCATE( &
        SoundSpeedL2Array ,&
        RateOfConvergence)


END PROGRAM MAIN
