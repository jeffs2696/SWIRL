! This code creates Swirl Class Objects that are iterations of SWIRL
PROGRAM OuterCode
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE SwirlClassObj
    USE L2NormModule
    USE sourceTermModule
    USE IEEE_ARITHMETIC
    IMPLICIT NONE

    INTEGER, PARAMETER:: rDef = REAL64
    TYPE(SwirlClassType):: swirlClassObject

    ! original inputs
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
        axialWavenumber          ,&
        axialWavenumberAnalytical,&
        ci                       ,&
        gam                      ,&
        gm1                      ,&
        alpha                    ,&
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
        drArray             ,&
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
        sndL2Array          ,&
        sndError            ,&
        errorMMS

    REAL(KIND = REAL64) ::  &
        secondOrderSmoother ,& !2nd order smoothing coefficient
        fourthOrderSmoother ,& !4th order smoothing coefficient
        L2res               ,&
        sndL2res            ,&
        errorSum            ,&
        errorSquared        ,&
        boundingConstant    ,&
        dr                  ,&
        hubToTipRatio      ! hub-to-tip ratio

    COMPLEX(KIND = REAL64), DIMENSION(:), ALLOCATABLE :: &
        alphaArray              ,&
        radialModeData          ,&
        residualVector          ,&
        residualVectorAnalytical,&
        L2res_array             ,&
        S_1                     ,&
        S_2                     ,&
        S_3                     ,&
        S_4

    INTEGER:: nPts  = 201  ! indended for flow data only and not the grid

    REAL(KIND = rDef), PARAMETER ::&
        radMin  = 0.20_rDef  ,&
        radMax  = 1.00_rDef  ,&
        rVelMax = 0.00_rDef  ,&
        slope   = 0.0_rDef   ,&
        angom   = 0.00_rDef

    CHARACTER(50):: &
        file_name1 , &
        file_name2 , &
        file_name3 , &
        file_name4 , &
        file_name5

    CHARACTER(30):: FORMAT

    CHARACTER(10):: file_id

!
! Code Starts Here!

!
    CONTINUE
!

    FORMAT = "(F12.5,F12.5,F12.5,F12.5)" ! General format for numerical 

    WRITE(6,*) 'SWIRL STARTS HERE - OuterCode.f90' 

    !OPEN(11, FILE="errorData.dat")
    !OPEN(13, FILE="SourceData.dat")
    !OPEN(144, FILE="CalcSourceData.dat")

    ! files for output
    ! open(12, file="flowdatainput.dat")
WRITE(6,*) '    Defining inputs needed for SwirlClassType class definition'
WRITE(6,*) '    ALL INPUTS ARE NON DIMENSIONAL                            '
WRITE(6,*) '    BE WEARY OF CONTRADICTIONS IN MAGNITUDE                   '

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
    boundingConstant = 0.000_rDef
    k_1 = CMPLX(0.2, 0.0, rDef)
    k_2 = CMPLX(1.0, 0.0, rDef)
    k_3 = CMPLX(0.4, 0.0, rDef)
    k_4 = CMPLX(0.2, 0.0, rDef)
    k_5 = CMPLX(0.0, 0.0, rDef)
    k_6 = CMPLX(0.0, 0.0, rDef)
    k_7 = CMPLX(0.0, 0.0, rDef)

    
    WRITE(6,*) '    Azimuthal Mode Number' , azimuthalModeNumber
    WRITE(6,*) '    Hub to Tip Ratio     ' , hubToTipRatio
    WRITE(6,*) '    Frequency            ' , frequency
    WRITE(6,*) '    Hub Liner Admittance ' , hubAdmittance
    WRITE(6,*) '    Duct Liner Admittance' , ductAdmittance
    WRITE(6,*) '    Second Order Smoother' , secondOrderSmoother
    WRITE(6,*) '    Fourth Order Smoother' , fourthOrderSmoother
 
    IF (FiniteDiffFlag.eq.0) THEN
        WRITE(6,*) '    Finite Difference Flag' , FiniteDiffFlag
        WRITE(6,*) '    --> Spectral Differencing is used for the radial derivatives'
    ELSEIF (FiniteDiffFlag.eq.1) THEN
        WRITE(6,*) '    Finite Difference Flag' , FiniteDiffFlag
        WRITE(6,*) '    --> Second Order Differencing is used for the radial derivatives'
    ELSEIF (FiniteDiffFlag.eq.2) THEN 
        WRITE(6,*) '    Finite Difference Flag' , FiniteDiffFlag
        WRITE(6,*) '    --> Fourth Order Differencing is used for the radial derivatives'
    ENDIF

    ! Starting Grid DO LOOP
    
    First_fac  = 1
    Last_fac   = 7

    facCount = 0 ! initializer for fac count 
    file_name2 = 'L2vsDeltar.dat'
    file_name3 = 'SndSpeedRateOfConvergence.dat'
    OPEN(345, FILE = TRIM(file_name2))
    OPEN(344, FILE = TRIM(file_name3))


    WRITE(6, *) '       Number of Grid Study Iterations: ' , Last_fac-First_fac

    ALLOCATE(&
        sndL2Array(Last_fac-First_fac+1),&
        drArray(Last_fac-First_fac+1)     ,&
        alphaArray(Last_fac-First_fac))

    DO fac = First_fac, Last_fac
        facCount = facCount + 1
        numberOfGridPoints   = 1+(2**fac)


        ! write integer into a string 
        WRITE(file_id, '(i0)') numberOfGridPoints

        ! Construct the filename:

        file_name1 = 'SoundSpeedMMS' // trim(adjustl(file_id)) // '.dat'
        ! file_name4 = 'FlowDataInput' // trim(adjustl(file_id)) // '.dat'

        ! file_name5 = 'FlowDataOutput' // trim(adjustl(file_id)) // '.dat'

        !    OPEN(145, FILE = TRIM(file_name1))

    WRITE(6, *) '       # Grid Points:                   ',  numberOfGridPoints

    WRITE(6, *) '       ALLOCATING SwirlClassObj Arrays ...' 
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
        

        WRITE(6, *) '       DONE ALLOCATING SwirlClassObj Arrays ...' 

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



            thetaMachData(i) = SQRT(2.0_rDef)*&
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
            object        = swirlClassObject     ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            AxialMachData = axialMachData        ,&
            ThetaMachData = thetaMachData        ,&
            SoundSpeed    = SoundSpeedOut        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       ,&
            ed2           = secondOrderSmoother  ,&
            ed4           = fourthOrderSmoother)

         CALL GetMeanFlowData(&
             object  = swirlClassObject, &
             axialMach = axialMachDataOut, &
             thetaMach = thetaMachDataOut, &
             axialMach_dr = axialMachData_dr_Out, &
             thetaMach_dr = thetaMachData_dr_Out, &
             SoundSpeed   = SoundSpeedOut, &
             SoundSpeed_dr = SoundSpeed_dr_Out, &
             radialData        = rOut)


        

        ! OPEN(122, FILE=TRIM(file_name5) )
        DO i = 1,numberOfGridPoints
            WRITE(6,FORMAT) &
                rOut(i) ,&
                axialMachDataOut(i) ,&
                thetaMachDataOut(i),&
                SoundSpeedOut(i)
        ENDDO
        ! CLOSE(122)


!         CALL FindResidualData(&
!             object              = swirlClassObject         ,&
!             axialWavenumber     = axialWavenumberAnalytical,&
!             vRPertubationData   = vRPertubation            ,&
!             vThPertubationData  = vThPertubation           ,&
!             vXPertubationData   = vXPertubation            ,&
!             pPertubationData    = pPertubation             ,&
!             vRResidual          = vRResidual               ,&
!             vThResidual         = vThResidual              ,&
!             vXResidual          = vXResidual               ,&
!             pResidual           = pResidual                ,&
!             S                   = residualVector)

!         CALL GetModeData(&
!             object          = swirlClassObject   ,&
!             modeNumber      = azimuthalModeNumber,&
!             axialWavenumber = axialWavenumber    ,&
!             radialModeData  = radialModeData )

        !               DO i = 1, numberOfGridPoints
        !               vRPertubation(i)  =  radialModeData(i)
        !               vThPertubation(i) =  radialModeData(numberOfGridPoints+i)
        !               vXPertubation(i)  =  radialModeData(2*numberOfGridPoints+i)
        !               pPertubation(i)   =  radialModeData(3*numberOfGridPoints+i)
        !               ENDDO


        !------------------------------------------------------------------------------
        CALL getL2Norm(L2 = sndL2res, &
            dataSet1 = SoundSpeedExpected, &
            dataSet2 = SoundSpeedOut, &
            numPoints = numberOfGridPoints)

        WRITE(345,* ) dr, REAL(sndL2res,rDef)

        drArray(facCount)    = dr
        sndL2Array(facCount) = sndL2res
        !------------------------------------------------------------------------------
        ! Call MMS
        !    WRITE(6, *) axialWavenumber

        !    CALL getSourceTerms(&
        !        np             =numberOfGridPoints        ,&
        !        mm             =azimuthalModeNumber       ,&
        !        gm1            =gm1                       ,&
        !        ak             =frequency*0.5_rDef        ,&
        !        axialWavenumber = axialWavenumberAnalytical, &
        !        r              =r                         ,&
        !        rmach          =axialMachData             ,&
        !        smach          =thetaMachData             ,&
        !        snd            =SoundSpeed                ,&
        !        rvel           =vRPertubation             ,&
        !        svel           =vThPertubation            ,&
        !        xvel           =vXPertubation             ,&
        !        p              =pPertubation              ,&
        !        dp_dr          =dp_dr                     ,&
        !        dsmach_dr      =dsmach_dr                 ,&
        !        drmach_dr      =drmach_dr                 ,&
        !        drvel_dr       =drvel_dr                  ,&
        !        S_1            =S_1                       ,&
        !        S_2            =S_2                       ,&
        !        S_3            =S_3                       ,&
        !        S_4            =S_4                       )
        !
        !    !    WRITE(6, *)  ' Numerical residual vector, S'
        !    DO i = 1, numberOfGridPoints
        !
        !
        !    residualVectorAnalytical(i) = S_1(i)
        !    residualVectorAnalytical(i+numberOfGridPoints) = S_2(i)
        !    residualVectorAnalytical(i+2*numberOfGridPoints) = S_3(i)
        !    residualVectorAnalytical(i+3*numberOfGridPoints) = S_4(i)
        !    !    WRITE(6, *) 'S_1 :', S_1(i)
        !    !    WRITE(6, *) 'S_2 :', S_2(i)
        !    !    WRITE(6, *) 'S_3 :', S_3(i)
        !    !    WRITE(6, *) 'S_4 :', S_4(i)
        !
        !    ENDDO
        !
        !    DO i = 1, numberOfGridPoints*4
        !    errorMMS(i)  =  REAL(ABS(residualVector(i) - residualVectorAnalytical(i)))
        !    errorSquared =  errorMMS(i)*errorMMS(i)
        !    errorSum     =  errorSum+errorSquared
        !    !    WRITE(6, *) errorMMS(i)
        !    !    WRITE(6, *) residualVector(i)
        !    END DO
        !    DO i = 1, numberOfGridPoints
        !    WRITE(11, *) &
        !        r(i)                                 ,&
        !        errorMMS(i)                          ,&
        !        errorMMS(numberOfGridPoints+i)     ,&
        !        errorMMS(2*numberOfGridPoints+i)   ,&
        !        errorMMS(3*numberOfGridPoints+i)
        !
        !    WRITE(144, *) &
        !        r(i)                                             ,&
        !        REAL(residualVector(i))                          ,&
        !        REAL(residualVector(numberOfGridPoints+i))     ,&
        !        REAL(residualVector(2*numberOfGridPoints+i))   ,&
        !        REAL(residualVector(3*numberOfGridPoints+i))
        !
        !    !    WRITE(6, *) r(i),  REAL(residualVector(i))          ,&
        !    !                       REAL(residualVector(numberOfGridPoints+i))     ,&
        !    !                       REAL(residualVector(2*numberOfGridPoints+i))   ,&
        !    !                       REAL(residualVector(3*numberOfGridPoints+i))
        !    !
        !    WRITE(13, *) &
        !        r(i)                                                        ,&
        !        REAL(residualVectorAnalytical(i))                           ,&
        !        REAL(residualVectorAnalytical(numberOfGridPoints+i))      ,&
        !        REAL(residualVectorAnalytical(2*numberOfGridPoints+i))    ,&
        !        REAL(residualVectorAnalytical(3*numberOfGridPoints+i))
        !
        !    END DO
        !    L2res = SQRT(errorSum/(numberOfGridPoints*4))
        !    WRITE(6, *)'L2Res: ', L2res

        CALL DestroyObject(object = swirlClassObject)
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

        !    L2res = 0.0_rDef
        errorSum = 0.0_rDef

        WRITE(6,*) sndL2Array(facCount)

        !    CLOSE(145)
    ENDDO

! Now that the code has finished iterating , lets calculate the
! asymotitic rate of convergence

    WRITE(6,'(A10,A10)') 'dr' ,'alpha'
    DO i = 1,SIZE(alphaArray)

        alphaArray(i) = (LOG(sndL2Array(i+1)) -&
            LOG(sndL2Array(i)))/LOG(0.5_rDef)
        WRITE(6,'(F10.5,F10.5)') drArray(i), REAL(alphaArray(i),rDef)
        WRITE(344,'(F10.5,F10.5)') drArray(i), REAL(alphaArray(i),rDef)

         ! WRITE(6,*) sndL2Array(i)/sndL2Array(i+1)

    ENDDO
    DEALLOCATE(&
        sndL2Array,&
        drArray   ,&
        alphaArray)
    CLOSE(344)
    CLOSE(345)
    !    CLOSE(144)
    !    CLOSE(11)
     ! CLOSE(12)
    !    CLOSE(13)
    ! CLOSE(122)

END PROGRAM
