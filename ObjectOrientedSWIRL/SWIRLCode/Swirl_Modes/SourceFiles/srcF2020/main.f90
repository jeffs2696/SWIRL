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
        ci                       ,&
        gam                      ,&
        gm1                      ,&
        k_1                      ,&
        k_2                      ,&
        k_3                      ,&
        k_4                      ,&
        k_5                      ,&
        k_6                      ,&
        k_7

    REAL(KIND = REAL64), DIMENSION(:), ALLOCATABLE :: &
        r                   ,&
        axialMachData       ,&
        thetaMachData       ,&
        thetaMachDataOut    ,&
        totalMachData       ,&
        SoundSpeedExpected         

    REAL(KIND = REAL64) ::  &
        secondOrderSmoother ,& !2nd order smoothing coefficient
        fourthOrderSmoother ,& !4th order smoothing coefficient
        boundingConstant    ,&
        dr                  ,&
        hubToTipRatio      ! hub-to-tip ratio


    ! INTEGER:: nPts  = 201  ! indended for flow data only and not the grid

    REAL(KIND = rDef), PARAMETER ::&
        radMin  = 0.20_rDef  ,&
        radMax  = 1.00_rDef  ,&
        rVelMax = 0.00_rDef  ,&
        slope   = 0.0_rDef   ,&
        angom   = 0.00_rDef

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

    ! Starting Grid DO LOOP

    First_fac  = 1
    Last_fac   = 1

    facCount = 0 ! initializer for fac count

    WRITE(6, *) '       Number of Grid Study Iterations: ' , Last_fac-First_fac + 1

    DO fac = First_fac, Last_fac

        facCount = facCount + 1
        numberOfGridPoints   = 1+(2**fac)

        WRITE(file_id, '(i0)') numberOfGridPoints

        WRITE(6, *) '       # Grid Points:                   ',  numberOfGridPoints

        ALLOCATE(&
            r(numberOfGridPoints)                                         ,&
            thetaMachData(numberOfGridPoints)                             ,&
            thetaMachDataOut(numberOfGridPoints)                             ,&
            axialMachData(numberOfGridPoints)                             ,&
            totalMachData(numberOfGridPoints)                             ,&
            SoundSpeedExpected(numberOfGridPoints)                                ,&
            )

        dr = (radMax-radMin)/REAL(numberOfGridPoints-1, rDef)

        DO i = 1, numberOfGridPoints
            r(i) = (radMin+REAL(i-1, rDef)*dr)/radMax
        END DO


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

        ENDDO

        CALL CreateObject(&
            object        = swirlClassObj  ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            AxialMachData = axialMachData        ,&
            ThetaMachData = thetaMachData        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       )


        CALL DestroyObject(object = swirlClassObj)

        DEALLOCATE(&
            r                         ,&
            thetaMachData             ,&
            thetaMachDataOut             ,&
            axialMachData             ,&
            totalMachData             ,&
            SoundSpeedExpected                ,&
            )

    END DO
END PROGRAM MAIN
