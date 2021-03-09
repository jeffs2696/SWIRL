PROGRAM OuterCode
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE SwirlClassObj
    USE L2NormModule
    USE sourceTermModule
    USE IEEE_ARITHMETIC
    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64

    TYPE(SwirlClassType) :: swirlClassObject

    ! original inputs
    INTEGER  :: &
        finiteDiffFlag      ,& ! finite difference flag
        azimuthalModeNumber ,& ! mode order
        numberOfGridPoints  ,& ! number of points
        i                   ,& ! number of points
        gp                  ,& ! number of points
        fac                 ,&
        First_gp            ,& ! number of points
        Last_gp             ,& ! number of points
        Step_gp             ,& ! number of points
        numModes            ,& ! number of radial modes
        modeNumber 


    COMPLEX(KIND=REAL64) :: & 
        frequency                ,&
        hubAdmittance            ,& 
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

    REAL(KIND=REAL64), DIMENSION(:),ALLOCATABLE :: &
        r                   , &
        axialMachData       ,&
        thetaMachData       ,&
        SoundSpeed          ,&
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
        errorMMS      

    REAL(KIND=REAL64) ::  &
        secondOrderSmoother             ,& !2nd order smoothing coefficient
        fourthOrderSmoother             ,& !4th order smoothing coefficient
        L2res           ,&
        errorSum        ,&
        errorSquared    ,&
        dr              ,&
        hubToTipRatio      ! hub-to-tip ratio 


    COMPLEX(KIND=REAL64) , DIMENSION(:), ALLOCATABLE :: &
        radialModeData          ,&
        residualVector          ,&
        residualVectorAnalytical,&
        L2res_array             ,&
        S_1                     ,&
        S_2                     ,&
        S_3                     ,&
        S_4
    INTEGER :: nPts  = 201 !indended for flow data only and not the grid 

    REAL(KIND=rDef), PARAMETER :: radMin  = 0.20_rDef,  &
        radMax  = 1.00_rDef,  &
        rVelMax = 0.00_rDef, &
        slope   = 0.0_rDef,  &
        angom   = 0.00_rDef

    OPEN(11,FILE="errorData.dat")
    OPEN(13,FILE="SourceData.dat")
    OPEN(144,FILE="CalcSourceData.dat")
    OPEN(12,FILE="flowData.dat")

    ! inputs needed for SwirlClassType
    azimuthalModeNumber     = 2
    hubToTipRatio       = radMin/radMax
    frequency           = CMPLX(20.0,0,rDef)  
    hubAdmittance       =  0.40_rDef
    ductAdmittance      =  0.70_rDef
    finiteDiffFlag      =  1
    secondOrderSmoother =  0.0_rDef
    fourthOrderSmoother =  0.0_rDef

    ! constants needed for calculations
    gam = 1.4_rDef       ! ratio of specific heats
    gm1 = gam - 1.0_rDef 

    ci  = CMPLX(0.0,1.0,rDef) ! imaginary number
    ! constants for MMS module
    k_1 = CMPLX(0.2,0.0,rDef)
    k_2 = CMPLX(0.4,0.0,rDef)
    k_3 = CMPLX(0.8,0.0,rDef)
    k_4 = CMPLX(0.2,0.0,rDef)
    k_5 = CMPLX(0.0,0.0,rDef)
    k_6 = CMPLX(0.0,0.0,rDef)
    k_7 = CMPLX(0.0,0.0,rDef)
    axialWavenumberAnalytical = 0.50_rDef
    ! Starting Grid DO LOOP  
    DO fac = 3,3
    nPts   = 1+(2**fac)

    numberOfGridPoints     = nPts

    numModes = numberOfGridPoints

    modeNumber = azimuthalModeNumber

    WRITE(6,*) '# Grid Points: ',  numberOfGridPoints

    ALLOCATE(S_1(numberOfGridPoints)                ,&
             S_2(numberOfGridPoints)                ,&
             S_3(numberOfGridPoints)                ,&
             S_4(numberOfGridPoints)                ,&
             radialModeData(numberOfGridPoints*4)  ,&
             residualVector(numberOfGridPoints*4)  ,&
             residualVectorAnalytical(numberOfGridPoints*4)  ,&
             errorMMS(numberOfGridPoints*4)        ,&
             r(nPts)                               ,&
             snd(nPts)                             ,&
             thetaMachData(nPts)                   ,&
             smachAnalytical(nPts)                 ,&
             axialMachData(nPts)                   ,&
             SoundSpeed(nPts)                      ,&
             vRPertubation(nPts)                   ,&
             vThPertubation(nPts)                  ,&
             vXPertubation(nPts)                   ,&
             pPertubation(nPts)                    ,&
             vRResidual(nPts)                      ,&
             vThResidual(nPts)                     ,&
             vXResidual(nPts)                      ,&
             pResidual(nPts)                       ,&
             vRSource(nPts)                        ,&
             vThSource(nPts)                       ,&
             vXSource(nPts)                        ,&
             pSource(nPts)                         ,&
             dp_dr(nPts)                           ,&
             dsmach_dr(nPts)                       ,&
             drmach_dr(nPts)                       ,&
             drvel_dr(nPts) )
     

    dr = (radMax-radMin)/REAL(nPts-1,rDef)

    DO i=1,nPts
    r(i) = (radMin + REAL(i-1,rDef)*dr)/radMax
    END DO

    DO i = 1,nPts
      SoundSpeed(i)     = EXP(k_1*r(i))
      axialMachData(i)  = EXP(k_2*r(i)) 
      thetaMachData(i)  = EXP(k_3*r(i))
      vRPertubation(i)  = EXP(k_4*r(i))
      vThPertubation(i) = EXP(k_5*r(i))
      vXPertubation(i)  = EXP(k_6*r(i))
      pPertubation      = EXP(k_7*r(i))
      dp_dr             = k_7*EXP(k_7*r(i))
      dsmach_dr(i)      = k_3*EXP(k_3*r(i))
      drmach_dr(i)      = k_2*EXP(k_2*r(i))
      drvel_dr (i)      = k_4*EXP(k_4*r(i))
    WRITE(12,*) r(i),axialMachData(i), thetaMachData(i)
    ENDDO
!------------------------------------------------------------------------------
    CALL CreateObject(&
        object        = swirlClassObject ,&
        azimuthalMode = azimuthalModeNumber,      &
        np            = numberOfGridPoints,      &
        sig           = hubToTipRatio,     &
        AxialMachData = axialMachData,&
        ThetaMachData = thetaMachData,& 
        ak            = frequency,      &
        etah          = hubAdmittance,    &
        etad          = ductAdmittance,    &
        ifdff         = finiteDiffFlag,   &
        ed2           = secondOrderSmoother,     &
        ed4           = fourthOrderSmoother)     
    
    CALL FindResidualData(&
        object              = swirlClassObject,&
        axialWavenumber     = axialWavenumberAnalytical,&
        vRPertubationData   = vRPertubation,&
        vThPertubationData  = vThPertubation,&
        vXPertubationData   = vXPertubation,&
        pPertubationData    = pPertubation,&
        vRResidual          = vRResidual,&
        vThResidual         = vThResidual,&
        vXResidual          = vXResidual,&
        pResidual           = pResidual,&
        S                   = residualVector)

    CALL GetModeData(&
        object          = swirlClassObject,&
        modeNumber      = modeNumber      ,&
        axialWavenumber = axialWavenumber,&
        radialModeData  = radialModeData )
                 
  !               DO i = 1,numberOfGridPoints
  !               vRPertubation(i)  =  radialModeData(i)
  !               vThPertubation(i) =  radialModeData(numberOfGridPoints + i)
  !               vXPertubation(i)  =  radialModeData(2*numberOfGridPoints + i)
  !               pPertubation(i)   =  radialModeData(3*numberOfGridPoints + i)
  !               ENDDO
                 
    CALL DestroyObject(object = swirlClassObject)
!------------------------------------------------------------------------------
    ! Call MMS
!    WRITE(6,*) axialWavenumber

    CALL getSourceTerms(np             =numberOfGridPoints        ,&
                        mm             =azimuthalModeNumber       ,&
                        gm1            =gm1                       ,&
                        ak             =frequency*0.5_rDef        ,&
                        axialWavenumber=axialWavenumberAnalytical,&
                        r              =r                         ,&
                        rmach          =axialMachData             ,&
                        smach          =thetaMachData             ,& 
                        snd            =SoundSpeed                ,& 
                        rvel           =vRPertubation             ,& 
                        svel           =vThPertubation            ,&
                        xvel           =vXPertubation             ,&
                        p              =pPertubation              ,&
                        dp_dr          =dp_dr                     ,&
                        dsmach_dr      =dsmach_dr                 ,&
                        drmach_dr      =drmach_dr                 ,&
                        drvel_dr       =drvel_dr                  ,&
                        S_1            =S_1                       ,&
                        S_2            =S_2                       ,&
                        S_3            =S_3                       ,&
                        S_4            =S_4                       )
                    
                    
    
    !    WRITE(6,*)  ' Numerical residual vector, S'
    DO i = 1,numberOfGridPoints
!    S_1(i) = (EXP(-k_1*r(i))/r(i))*      &
!        (ci*EXP( k_4*r(i))*frequency*r(i)-    &
!        ci*EXP(r(i)*(k_1 + k_2 + k_4))*axialWavenumber*r(i)-&
!        ci*EXP(r(i)*(k_1 + k_3 + k_4))*azimuthalModeNumber-  &
!        EXP(r(i)*(k_1 + 2*k_3+k_7))*gam+  &
!        EXP(r(i)*(k_1 + 2*k_3+k_7))    +  &
!        2.0_rDef*EXP(r(i)*(k_1 +   k_3+k_5))) 


!    S_2(i) = (EXP(-k_1*r(i))/r(i))*      &
!        (ci*EXP( k_5*r(i))*frequency*r(i)-    &
!        ci*EXP(r(i)*(k_1 + k_2 + k_5))*axialWavenumber*r(i)-&
!        ci*EXP(r(i)*(k_1 +       k_7))*azimuthalModeNumber-  &
!        ci*EXP(r(i)*(k_1 +   k_3+k_5))*azimuthalModeNumber-  &
!        (EXP(r(i)*(k_3))/r(i)    +  &
!        gm1*EXP(3*k_3*r(i))/(2.0_rDef*r(i)))*&
!        EXP(r(i)*(k_1+k_4))*r(i))

!    S_3(i) = (EXP(-k_1*r(i))/r(i))*      &
!        (ci*EXP(r(i)*(k_1+k_2+k_6))*axialWavenumber*r(i)-&
!        ci*EXP(r(i)*(k_1+k_3+k_6))*azimuthalModeNumber-  &
!        ci*axialWavenumber*EXP(r(i)*(k_1 +k_7))*r(i)-  &
!        ci*(EXP(r(i)*(k_6))*frequency*r(i)    +  &
!        gm1*EXP(k_2 +  2*k_3*r(i) ) /(2.0_rDef*r(i)))*&
!        EXP(r(i)*(k_1+k_4))*r(i))


        
!    S_4(i) =-(EXP(-k_1*r(i))/r(i))*      &
!        (-ci*EXP(r(i)*(k_1+k_2+k_7))*axialWavenumber*r(i)+    &
!        ci*EXP(r(i)*(k_1+k_3+k_7))*azimuthalModeNumber*r(i)-&
!        (gm1*EXP(2*k_3*r(i))/(2.0_rDef*r(i)) + 1/r(i))*&
!        EXP(r(i)*(k_1+k_4))*r(i)) - &
!        ci*EXP(r(i)*(k_1 +       k_5))*azimuthalModeNumber+  &
!        ci*r(i)*(frequency*EXP(k_7*r(i)) - axialWavenumber*EXP(r(i)*(k_1 + k_6)))  


    residualVectorAnalytical(i) = S_1(i)
    residualVectorAnalytical(i + numberOfGridPoints) = S_2(i)
    residualVectorAnalytical(i + 2*numberOfGridPoints) = S_3(i)
    residualVectorAnalytical(i + 3*numberOfGridPoints) = S_4(i)
    !    WRITE(6,*) 'S_1 :', S_1(i)
    !    WRITE(6,*) 'S_2 :', S_2(i)
    !    WRITE(6,*) 'S_3 :', S_3(i)
    !    WRITE(6,*) 'S_4 :', S_4(i)

    ENDDO

    DO i=1,numberOfGridPoints*4
    errorMMS(i)  =  REAL(ABS(residualVector(i) - residualVectorAnalytical(i)))
    errorSquared =  errorMMS(i)*errorMMS(i)
    errorSum     =  errorSum + errorSquared
    !    WRITE(6,*) errorMMS(i)
    !    WRITE(6,*) residualVector(i)
    END DO
    DO i=1,numberOfGridPoints
    WRITE(11,*) r(i),  errorMMS(i)          ,&
                       errorMMS(numberOfGridPoints + i)     ,&
                       errorMMS(2*numberOfGridPoints + i)   ,&
                       errorMMS(3*numberOfGridPoints + i)
                   
    WRITE(144,*) r(i),  REAL(residualVector(i))          ,&
                       REAL(residualVector(numberOfGridPoints + i))     ,&
                       REAL(residualVector(2*numberOfGridPoints + i))   ,&
                       REAL(residualVector(3*numberOfGridPoints + i))
    WRITE(6,*) r(i),  REAL(residualVector(i))          ,&
                       REAL(residualVector(numberOfGridPoints + i))     ,&
                       REAL(residualVector(2*numberOfGridPoints + i))   ,&
                       REAL(residualVector(3*numberOfGridPoints + i))

    WRITE(13,*) r(i),  REAL(residualVectorAnalytical(i))          ,&
                       REAL(residualVectorAnalytical(numberOfGridPoints + i))     ,&
                       REAL(residualVectorAnalytical(2*numberOfGridPoints + i))   ,&
                       REAL(residualVectorAnalytical(3*numberOfGridPoints + i))
    END DO
    L2res = SQRT(errorSum/(numberOfGridPoints*4))
    WRITE(6,*)'L2Res: ', L2res

    DEALLOCATE(S_1,S_2,S_3,S_4         ,&
        radialModeData          ,&
        residualVector          ,&
        residualVectorAnalytical,&
        errorMMS                ,&
        r                       ,&
        snd                     ,&
        vRPertubation           ,&
        vThPertubation          ,&
        vXPertubation           ,&
        pPertubation            ,&
        vRSource                ,&
        vThSource               ,&
        vXSource                ,&
        pSource                 ,&
        vRResidual                ,&
        vThResidual               ,&
        vXResidual                ,&
        pResidual                 ,&
        thetaMachData           ,&
        smachAnalytical         ,&
        axialMachData           ,&
        SoundSpeed              ,&
        dp_dr                   ,&
        drmach_dr               ,&
        dsmach_dr               ,&
        drvel_dr)
    !    L2res = 0.0_rDef
    errorSum = 0.0_rDef

    ENDDO

    CLOSE(144)
    CLOSE(8)
    CLOSE(11)
    CLOSE(12)
    CLOSE(13)
END PROGRAM 
