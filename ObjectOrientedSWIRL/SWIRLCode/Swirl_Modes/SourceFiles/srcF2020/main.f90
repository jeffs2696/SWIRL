PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject

    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64
    TYPE(SwirlClassType) :: swirlClassObj

    INTEGER  :: &
        finiteDiffFlag      ,& ! finite difference flag
        azimuthalModeNumber ,& ! mode order
        numberOfGridPoints  ,& ! number of points
        i                   ,& ! indexer for do loops
        fac                 ,& ! variable used for doubling grid points
        facCount            ,& ! counts the outermost do loop
        First_fac           ,& ! starting fac integer
        Last_fac               ! ending fac integer

    COMPLEX(KIND = REAL64) :: &
        frequency                ,& !non-dimensional frequency
        hubAdmittance            ,& !Liner Admittance At the Hub
        ductAdmittance           ,&
        ! axialWavenumber          ,&
        axialWavenumberAnalytical,&
        ci                       ,&
        gam                      ,&
        gm1                      ,&
        ! alpha                    ,&
        k_1                      ,&
        k_2                      ,&
        k_3                      ,&
        k_4                      ,&
        k_5                      ,&
        k_6                      ,&
        k_7

    REAL(KIND = REAL64), DIMENSION(:), ALLOCATABLE :: &
        r                   ,&
        rOut                ,&
        ! drArray             ,&
        axialMachData       ,&
        axialMachDataOut    ,&
        axialMachData_dr_Out,&
        thetaMachData       ,&
        thetaMachDataOut    ,&
        thetaMachData_dr_Out,&
        totalMachData       ,&
        SoundSpeedExpected          ,&
        SoundSpeedOut       ,&
        SoundSpeed_dr_Out   ,&
        smachAnalytical     ,&
        vRPertubation       ,&
        vThPertubation      ,&
        vXPertubation       ,&
        pPertubation        ,&
        dp_dr               ,&
        dsmach_dr           ,&
        drmach_dr           ,&
        drvel_dr            ,&
        vRResidual          ,&
        vThResidual         ,&
        vXResidual          ,&
        pResidual           ,&
        vRSource            ,&
        vThSource           ,&
        vXSource            ,&
        pSource             ,&
        snd                 ,&
        ! sndL2Array          ,&
        sndError            ,&
        errorMMS

    REAL(KIND = REAL64) ::  &
        secondOrderSmoother ,& !2nd order smoothing coefficient
        fourthOrderSmoother ,& !4th order smoothing coefficient
        ! L2res               ,&
        ! sndL2res            ,&
        ! errorSum            ,&
        ! errorSquared        ,&
        boundingConstant    ,&
        dr                  ,&
        hubToTipRatio      ! hub-to-tip ratio

    COMPLEX(KIND = REAL64), DIMENSION(:), ALLOCATABLE :: &
        ! alphaArray              ,&
        radialModeData          ,&
        residualVector          ,&
        residualVectorAnalytical,&
        ! L2res_array             ,&
        S_1                     ,&
        S_2                     ,&
        S_3                     ,&
        S_4

    ! INTEGER:: nPts  = 201  ! indended for flow data only and not the grid

    REAL(KIND = rDef), PARAMETER ::&
        radMin  = 0.20_rDef  ,&
        radMax  = 1.00_rDef  ,&
        rVelMax = 0.00_rDef  ,&
        slope   = 0.0_rDef   ,&
        angom   = 0.00_rDef

    ! CHARACTER(50):: &
        ! file_name1 , &
        ! file_name2 , &
        ! file_name3 !, &
        ! file_name4 , &
        ! file_name5

    CHARACTER(30):: FORMAT

    CHARACTER(10):: file_id

!
! Code Starts Here!

!
    CONTINUE
!

    FORMAT = "(F12.5,F12.5,F12.5,F12.5)" ! General format for numerical

    WRITE(6,*) 'SWIRL STARTS HERE - main.f90'

    WRITE(6,*) '-------------------------------------------------------------------------------------------------'
    WRITE(6,*) '    Defining inputs needed for SwirlClassType class definition'
    WRITE(6,*) '    ALL INPUTS ARE NON DIMENSIONAL                            '
    WRITE(6,*) '    BE WEARY OF CONTRADICTIONS IN MAGNITUDE                   '
    WRITE(6,*) '-------------------------------------------------------------------------------------------------'
    WRITE(6,*) ' '

    ci  = CMPLX(0.0, 1.0, rDef)  ! imaginary number

    ! inputs needed for SwirlClassType
    azimuthalModeNumber       = 2
    hubToTipRatio             = radMin/radMax
    frequency                 =  CMPLX(20.0, 0, rDef)
    axialWavenumberAnalytical =  0.50_rDef
    hubAdmittance             =  0.40_rDef
    ductAdmittance            =  0.70_rDef
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

    WRITE(6,*) ' '
    WRITE(6,*) '-------------------------------------------------------------------------------------------------'
    WRITE(6,*) '    INPUT DECK: '
    WRITE(6,*) '        Azimuthal Mode Number' , azimuthalModeNumber
    WRITE(6,*) '        Hub to Tip Ratio     ' , hubToTipRatio
    WRITE(6,*) '        Frequency            ' , frequency
    WRITE(6,*) '        Hub Liner Admittance ' , hubAdmittance
    WRITE(6,*) '        Duct Liner Admittance' , ductAdmittance
    WRITE(6,*) '        Second Order Smoother' , secondOrderSmoother
    WRITE(6,*) '        Fourth Order Smoother' , fourthOrderSmoother
    IF (finiteDiffFlag.eq.0) THEN
        WRITE(6,*) '        Finite Difference Flag' , finiteDiffFlag
        WRITE(6,*) '        --> Spectral Differencing is used for the radial derivatives'
    ELSEIF (finiteDiffFlag.eq.1) THEN
        WRITE(6,*) '        Finite Difference Flag' , finiteDiffFlag
        WRITE(6,*) '        --> Second Order Differencing is used for the radial derivatives'
    ELSEIF (finiteDiffFlag.eq.2) THEN
        WRITE(6,*) '        Finite Difference Flag' , finiteDiffFlag
        WRITE(6,*) '       --> Fourth Order Differencing is used for the radial derivatives'
    ENDIF

    WRITE(6,*) '-------------------------------------------------------------------------------------------------'
    WRITE(6,*) ' '
    ! Starting Grid DO LOOP

    First_fac  = 1
    Last_fac   = 1

    facCount = 0 ! initializer for fac count
    ! file_name2 = 'L2vsDeltar.dat'
    ! file_name3 = 'SndSpeedRateOfConvergence.dat'
    ! OPEN(345, FILE = TRIM(file_name2))
    ! OPEN(344, FILE = TRIM(file_name3))


    WRITE(6, *) '       Number of Grid Study Iterations: ' , Last_fac-First_fac + 1

    DO fac = First_fac, Last_fac
        facCount = facCount + 1
        numberOfGridPoints   = 1+(2**fac)


        ! write integer into a string
        WRITE(file_id, '(i0)') numberOfGridPoints

        ! Construct the filename:

        ! file_name1 = 'SoundSpeedMMS' // trim(adjustl(file_id)) // '.dat'
        ! file_name4 = 'FlowDataInput' // trim(adjustl(file_id)) // '.dat'

        ! file_name5 = 'FlowDataOutput' // trim(adjustl(file_id)) // '.dat'

        !    OPEN(145, FILE = TRIM(file_name1))

        WRITE(6, *) '       # Grid Points:                   ',  numberOfGridPoints

        ! WRITE(6, *) '       ALLOCATING SwirlClassObj Arrays ...'
        ALLOCATE(&
             S_1(numberOfGridPoints)                         ,&
            S_2(numberOfGridPoints)                         ,&
            S_3(numberOfGridPoints)                         ,&
            S_4(numberOfGridPoints)                         ,&
            radialModeData(numberOfGridPoints*4)            ,&
            residualVector(numberOfGridPoints*4)            ,&
            residualVectorAnalytical(numberOfGridPoints*4)  ,&
            errorMMS(numberOfGridPoints*4)                  ,&
            sndError(numberOfGridPoints)                    ,&
            r(numberOfGridPoints)                                         ,&
            rOut(numberOfGridPoints)                                         ,&
            snd(numberOfGridPoints)                                       ,&
            thetaMachData(numberOfGridPoints)                             ,&
            thetaMachDataOut(numberOfGridPoints)                             ,&
            thetaMachData_dr_Out(numberOfGridPoints)                             ,&
            smachAnalytical(numberOfGridPoints)                           ,&
            axialMachData(numberOfGridPoints)                             ,&
            axialMachDataOut(numberOfGridPoints)                             ,&
            axialMachData_dr_Out(numberOfGridPoints)                             ,&
            totalMachData(numberOfGridPoints)                             ,&
            SoundSpeedExpected(numberOfGridPoints)                                ,&
            SoundSpeedOut(numberOfGridPoints)                             ,&
            SoundSpeed_dr_Out(numberOfGridPoints)                             ,&
            vRPertubation(numberOfGridPoints)                             ,&
            vThPertubation(numberOfGridPoints)                            ,&
            vXPertubation(numberOfGridPoints)                             ,&
            pPertubation(numberOfGridPoints)                              ,&
            vRResidual(numberOfGridPoints)                                ,&
            vThResidual(numberOfGridPoints)                               ,&
            vXResidual(numberOfGridPoints)                                ,&
            pResidual(numberOfGridPoints)                                 ,&
            vRSource(numberOfGridPoints)                                  ,&
            vThSource(numberOfGridPoints)                                 ,&
            vXSource(numberOfGridPoints)                                  ,&
            pSource(numberOfGridPoints)                                   ,&
            dp_dr(numberOfGridPoints)                                     ,&
            dsmach_dr(numberOfGridPoints)                       ,&
            drmach_dr(numberOfGridPoints)                       ,&
            drvel_dr(numberOfGridPoints) )


        ! WRITE(6, *) '       DONE ALLOCATING SwirlClassObj Arrays ...'

        WRITE(6,*) '        Defining Radial Domain ...'

        dr = (radMax-radMin)/REAL(numberOfGridPoints-1, rDef)

        DO i = 1, numberOfGridPoints
            r(i) = (radMin+REAL(i-1, rDef)*dr)/radMax
        END DO

        WRITE(6,*) '        Defining Flow Domain ...'

        ! Headers for Flow Domain Data'

        WRITE(6,*) ' '
        WRITE(6,'(a)') 'Table 1: Input Flow Data'
        WRITE(6,'(12a12,12a5,12a12,12a12)') 'Radius', 'Mx' , 'M_theta', 'A_expected'

        DO i = 1, numberOfGridPoints

            ! this segment of the code is defining a flow that will allow
            ! us to know what the speed of sound is expected to be
            axialMachData(i)  =&
                (boundingConstant)*&
                EXP(REAL(k_2, rDef)*(r(i)-1.0_rDef))



            thetaMachData(i) = &
            SQRT(2.0_rDef)*&
                SQRT(-(REAL(k_3,rDef)*r(i)*&
                SIN(REAL(k_3,rDef)*(r(i)-1.0_rDef)))/&
                (REAL(gm1,rDef)*COS(REAL(k_3,rDef)*(r(i)-1.0_rDef))))

            SoundSpeedExpected(i) = &
                COS(REAL(k_3,rDef)*(r(i)-1.0_rDef))! EXP(REAL(k_3, rDef)*(r(i)-r(numberOfGridPoints)))
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

                WRITE(6, FORMAT) r(i), axialMachData(i), thetaMachData(i), SoundSpeedExpected(i)

            ENDIF

!            vRPertubation(i)  = EXP(REAL(k_4, rDef)*r(i))
!            vThPertubation(i) = EXP(REAL(k_5, rDef)*r(i))
!            vXPertubation(i)  = EXP(REAL(k_6, rDef)*r(i))
!            pPertubation      = EXP(REAL(k_7, rDef)*r(i))
!            dp_dr             = REAL(k_7, rDef)*EXP(REAL(k_7)*r(i))
!            dsmach_dr(i)      = REAL(k_3, rDef)*EXP(REAL(k_3)*r(i))
!            drmach_dr(i)      = REAL(k_2, rDef)*EXP(REAL(k_2)*r(i))
!            drvel_dr (i)      = REAL(k_4, rDef)*EXP(REAL(k_4)*r(i))

        ENDDO

!        OPEN(12, FILE=file_name4)
!        DO i = 1,numberOfGridPoints
!            WRITE(12,*) r(i) , axialMachData(i) , thetaMachData(i), SoundSpeedExpected(i)
!        ENDDO
!        CLOSE(12)
!
        !------------------------------------------------------------------------------
        CALL CreateObject(&
            object        = swirlClassObj  ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            AxialMachData = axialMachData        ,&
            ThetaMachData = thetaMachData        ,&
            ! SoundSpeed    = SoundSpeedOut        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       ,&
            ed2           = secondOrderSmoother  ,&
            ed4           = fourthOrderSmoother)


        CALL DestroyObject(object = swirlClassObj)
        DEALLOCATE(&
            S_1                       ,&
            S_2                       ,&
            S_3                       ,&
            S_4                       ,&
            radialModeData            ,&
            residualVector            ,&
            residualVectorAnalytical  ,&
            errorMMS                  ,&
            sndError                  ,&
            r                         ,&
            rOut                         ,&
            snd                       ,&
            vRPertubation             ,&
            vThPertubation            ,&
            vXPertubation             ,&
            pPertubation              ,&
            vRSource                  ,&
            vThSource                 ,&
            vXSource                  ,&
            pSource                   ,&
            vRResidual                ,&
            vThResidual               ,&
            vXResidual                ,&
            pResidual                 ,&
            thetaMachData             ,&
            thetaMachDataOut             ,&
            thetaMachData_dr_Out             ,&
            smachAnalytical           ,&
            axialMachData             ,&
            axialMachDataOut             ,&
            axialMachData_dr_Out             ,&
            totalMachData             ,&
            SoundSpeedExpected                ,&
            SoundSpeedOut             ,&
            SoundSpeed_dr_Out             ,&
            dp_dr                     ,&
            drmach_dr                 ,&
            dsmach_dr                 ,&
            drvel_dr)

    END DO
END PROGRAM MAIN
