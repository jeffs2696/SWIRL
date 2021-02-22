PROGRAM OuterCode
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE SwirlClassObj
    USE L2NormModule
    USE IEEE_ARITHMETIC
    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64
    ! defining a new derived data type

    TYPE(SwirlClassType) :: swirlClassObject

    ! original inputs
    INTEGER  :: &
        ifdff     ,& ! finite difference flag
        mm        ,& ! mode order
        np        ,& ! number of points
        i         ,& ! number of points
        gp        ,& ! number of points
        First_gp  ,& ! number of points
        Last_gp   ,& ! number of points
        Step_gp   ,& ! number of points
        numModes  ,& ! number of radial modes
        modeNumber 


    COMPLEX(KIND=REAL64) :: ak             ,&
        etah, etad     ,&
        axialWavenumber,&
        axialWavenumberAnalytical,&
        ci             ,&
        gam            ,&
        gm1            ,&
        alpha          ,&
        k_1            ,&
        k_2            ,&
        k_3            ,&
        k_4            ,&
        k_5            ,&
        k_6            ,&
        k_7            

    REAL(KIND=REAL64), DIMENSION(:),ALLOCATABLE :: r, &
        !                                                 rhoMean         ,&                                 
    !                                                 rhoVrMean       ,&
    !                                                 rhoVthetaMean   ,&
    !                                                 rhoVxMean       ,&
    !                                                 etotalMean      ,&
    rmach           ,&
        rmachAnalytical ,&
        smach           ,&
        smachAnalytical ,&
        rvel            ,&
        svel            ,&
        snd             ,&
        errorMMS      

    REAL(KIND=REAL64) ::  &
        ed2,   & !2nd order smoothing coefficient
        ed4,   & !4th order smoothing coefficient
        L2res ,&
        errorSum       ,&
        errorSquared   ,&
        dr             ,&
        sig      ! hub-to-tip ratio 


    COMPLEX(KIND=REAL64) , DIMENSION(:), ALLOCATABLE :: radialModeData,&
        residualVector,&
        residualVectorAnalytical,&
        L2res_array  ,&
        S_1,S_2,S_3,S_4



    INTEGER :: nPts  = 201 

    REAL(KIND=rDef), PARAMETER :: radMin  = 0.20_rDef,  &
        radMax  = 1.00_rDef,  &
        rVelMax = 0.00_rDef, &
        slope   = 0.0_rDef,  &
        angom   = 0.00_rDef

    OPEN(11,FILE="errorData.dat")
    OPEN(13,FILE="SourceData.dat")
    OPEN(14,FILE="CalcSourceData.dat")
    OPEN(12,FILE="flowData.dat")

    mm     = 0
    sig    = 0.20_rDef   
    ak     = CMPLX(20.0,0,rDef)  
    etah   =  0.40_rDef
    etad   =  0.70_rDef
    ifdff  =  1
    ed2    =  0.0_rDef
    ed4    =  0.0_rDef

    First_gp = 8 
    Last_gp  = 32
    Step_gp  = 1

    gam = 1.4_rDef
    gm1 = gam - 1.0_rDef

    ci  = CMPLX(0.0,1.0)
    k_1 = CMPLX(0.0,0.0)
    k_2 = CMPLX(0.4,0.0)
    k_3 = CMPLX(0.8,0.0)
    k_4 = CMPLX(0.2,0.0)
    k_5 = CMPLX(0.0,0.0)
    k_6 = CMPLX(0.0,0.0)
    k_7 = CMPLX(0.0,0.0)

    ! Starting Grid DO LOOP  
    DO gp =  First_gp,Last_gp,Step_gp
    nPts   = gp
    np     = nPts
    numModes = np
    modeNumber = 3

    !    Conditional statement to only run the first loop    
    IF (gp.GT.First_gp) THEN
        STOP
    ELSE
    ENDIF
    !!    
    WRITE(6,*) '# Grid Points: ',  np
    ALLOCATE(S_1(np),S_2(np),S_3(np),S_4(np),&
        radialModeData(np*4)  ,&
        residualVector(np*4)  ,&
        residualVectorAnalytical(np*4)  ,&
        errorMMS(np*4)        ,&
        r(nPts)               ,&
        snd(nPts)             ,&
        svel(nPts)            ,&
        smach(nPts)           ,&
        smachAnalytical(nPts) ,&
        rmach(nPts)           ,&
        rmachAnalytical(nPts) ,&
        rvel(nPts))

    sig = radMin/radMax

    dr = (radMax-radMin)/REAL(nPts-1,rDef)

    DO i=1,nPts
    r(i) = (radMin + REAL(i-1,rDef)*dr)/radMax
    END DO


    !    DO i=1,nPts
    !      snd(i)    =  1.0_rDef - gm1/2.0_rDef*angom*angom*(1.0_rDef - r(i)*r(i)) ! 
    !      snd(i)    =  sqrt(snd(i))
    !      svel(i)   =  angom*r(i)
    !      smach(i)  =  svel(i)/snd(i)
    !    END DO
    !    WRITE(6,*) ' linear shear in the axial flow'
    DO i=1,nPts
    IF (slope.ge.0.0_rDef) THEN
        rvel(i) = slope*(r(i) -1.0_rDef)+rVelMax
    ELSE
        rvel(i) = slope*(r(i) -sig)     +rVelMax
    ENDIF
    ENDDO

    DO i = 1,nPts
    rmach(i) = 0.40_rDef 
    smach(i) = 0.28*r(i) + 0.1/r(i)
    WRITE(12,*) r(i),rmach(i), smach(i)
    ENDDO

    !    WRITE(6,*) 'Creating Object' 
    CALL CreateObject(object         = swirlClassObject ,&
        mm            = mm,      &
        np            = np,      &
        sig           = sig,     &
        AxialMachData = rmach,&
        ThetaMachData = smach,& 
        ak            = ak,      &
        etah          = etah,    &
        etad          = etad,    &
        ifdff         = ifdff,   &
        ed2           = ed2,     &
        ed4           = ed4)     
    !    WRITE(6,*) 'Finished creating object' 
    CALL GetModeData(object          = swirlClassObject,&
        modeNumber      = modeNumber      ,&
        axialWavenumber = axialWavenumber,&
        radialModeData  = radialModeData )

    CALL FindResidualData(object     = swirlClassObject,&
        modeNumber = numModes        ,&
        S          = residualVector)

    CALL DestroyObject(object = swirlClassObject)

    !    WRITE(6,*)  ' Numerical residual vector, S'
    DO i = 1,np
    S_1(i) = (EXP(-k_1*r(i))/r(i))*      &
        (ci*EXP( k_4*r(i))*ak*r(i)-    &
        ci*EXP(r(i)*(k_1 + k_2 + k_4))*axialWavenumber*r(i)-&
        ci*EXP(r(i)*(k_1 + k_3 + k_4))*mm-  &
        EXP(r(i)*(k_1 + 2*k_3+k_7))*gam+  &
        EXP(r(i)*(k_1 + 2*k_3+k_7))    +  &
        2.0_rDef*EXP(r(i)*(k_1 +   k_3+k_5))) 

    residualVectorAnalytical(i) = S_1(i)

    !    WRITE(6,*) 'S_1 :', S_1(i)

    S_2(i) = (EXP(-k_1*r(i))/r(i))*      &
        (ci*EXP( k_5*r(i))*ak*r(i)-    &
        ci*EXP(r(i)*(k_1 + k_2 + k_5))*axialWavenumber*r(i)-&
        ci*EXP(r(i)*(k_1 +       k_7))*mm-  &
        ci*EXP(r(i)*(k_1 +   k_3+k_5))*mm-  &
        (EXP(r(i)*(k_3))/r(i)    +  &
        gm1*EXP(3*k_3*r(i))/(2.0_rDef*r(i)))*&
        EXP(r(i)*(k_1+k_4))*r(i))

    residualVectorAnalytical(i + np) = S_2(i)

    !    WRITE(6,*) 'S_2 :', S_2(i)
    S_3(i) = (EXP(-k_1*r(i))/r(i))*      &
        (ci*EXP(r(i)*(k_1+k_2+k_6))*axialWavenumber*r(i)-&
        ci*EXP(r(i)*(k_1+k_3+k_6))*mm-  &
        ci*axialWavenumber*EXP(r(i)*(k_1 +k_7))*r(i)-  &
        ci*(EXP(r(i)*(k_6))*ak*r(i)    +  &
        gm1*EXP(k_2 +  2*k_3*r(i) ) /(2.0_rDef*r(i)))*&
        EXP(r(i)*(k_1+k_4))*r(i))

    residualVectorAnalytical(i + 2*np) = S_3(i)

    !    WRITE(6,*) 'S_3 :', S_3(i)
    S_4(i) =-(EXP(-k_1*r(i))/r(i))*      &
        (-ci*EXP(r(i)*(k_1+k_2+k_7))*axialWavenumber*r(i)+    &
        ci*EXP(r(i)*(k_1+k_3+k_7))*mm*r(i)-&
        (gm1*EXP(2*k_3*r(i))/(2.0_rDef*r(i)) + 1/r(i))*&
        EXP(r(i)*(k_1+k_4))*r(i)) - &
        ci*EXP(r(i)*(k_1 +       k_5))*mm+  &
        ci*r(i)*(ak*EXP(k_7*r(i)) - axialWavenumber*EXP(r(i)*(k_1 + k_6)))  

    residualVectorAnalytical(i + 3*np) = S_4(i)
    !    WRITE(6,*) 'S_4 :', S_4(i)
    ENDDO

    DO i=1,np*4
    errorMMS(i)  =  REAL(ABS(residualVector(i) - residualVectorAnalytical(i)))
    errorSquared =  errorMMS(i)*errorMMS(i)
    errorSum     =  errorSum + errorSquared
    !    WRITE(6,*) errorMMS(i)
        WRITE(6,*) residualVector(i)
    END DO
    DO i=1,np
    WRITE(11,*) r(i),  errorMMS(i)          ,&
                       errorMMS(np + i)     ,&
                       errorMMS(2*np + i)   ,&
                       errorMMS(3*np + i)
                   
    WRITE(14,*) r(i),  REAL(residualVector(i))          ,&
                       REAL(residualVector(np + i))     ,&
                       REAL(residualVector(2*np + i))   ,&
                       REAL(residualVector(3*np + i))

    WRITE(13,*) r(i),  REAL(residualVectorAnalytical(i))          ,&
                       REAL(residualVectorAnalytical(np + i))     ,&
                       REAL(residualVectorAnalytical(2*np + i))   ,&
                       REAL(residualVectorAnalytical(3*np + i))
    END DO
    L2res = SQRT(errorSum/(np*4))
    WRITE(6,*)'L2Res: ', L2res

    DEALLOCATE(S_1,S_2,S_3,S_4         ,&
        radialModeData          ,&
        residualVector          ,&
        residualVectorAnalytical,&
        errorMMS                ,&
        r                       ,&
        snd                     ,&
        svel                    ,&
        smach                   ,&
        smachAnalytical         ,&
        rmach                   ,&
        rmachAnalytical         ,&
        rvel)
    !    L2res = 0.0_rDef
    errorSum = 0.0_rDef

    ENDDO

    CLOSE(8)
    CLOSE(11)
    CLOSE(12)
    CLOSE(13)
    CLOSE(14)
END PROGRAM 
