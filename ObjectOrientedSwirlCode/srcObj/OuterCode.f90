PROGRAM OuterCode
 USE, INTRINSIC  :: ISO_FORTRAN_ENV
 USE SwirlClassObj
 USE L2NormModule
 USE IEEE_ARITHMETIC
 IMPLICIT NONE

 INTEGER, PARAMETER :: rDef = REAL64
! defining a new derived data type

 TYPE(SwirlClassType) :: swirlClassObject
 TYPE(SwirlClassType) :: swirlClassObjectAnalytical

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
                          gam            ,&
                          gm1            ,&
                          alpha

  REAL(KIND=REAL64), DIMENSION(:),ALLOCATABLE :: r, &
                                                 rhoMean         ,&                                 
                                                 rhoVrMean       ,&
                                                 rhoVthetaMean   ,&
                                                 rhoVxMean       ,&
                                                 etotalMean      ,&
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
                       dr             ,&
                       sig      ! hub-to-tip ratio 
  
                      
  COMPLEX(KIND=REAL64) , DIMENSION(:), ALLOCATABLE :: radialModeData,&
                                                      residualVector,&
                                                      residualVectorAnalytical,&
                                                      L2res_array
  
                                                   
                                                  
  INTEGER :: nPts = 201 

  REAL(KIND=rDef), PARAMETER :: radMin  = 0.2_rDef,  &
                                radMax  = 1.0_rDef,  &
                                rVelMax = 0.00_rDef, &
                                slope   = 0.0_rDef,  &
                                angom   = 0.00_rDef
  OPEN(8,FILE="errorData.dat")

  mm     = 0
  sig    = 0.20_rDef   
  ak     = CMPLX(20.0,0,rDef)  
  etah   =  0.40_rDef
  etad   =  0.70_rDef
  ifdff  =  1
  ed2    =  0.0_rDef
  ed4    =  0.0_rDef

  First_gp = 16
  Last_gp  = 64 
  Step_gp  = 1

  gam = 1.4_rDef
  gm1 = gam - 1.0_rDef


  ! Starting Grid DO LOOP  
  DO gp =  First_gp,Last_gp,Step_gp
    nPts   = gp
    np     = nPts
    numModes = np
    modeNumber = 2

!    Conditional statement to only run the first loop    
   ! IF (gp.GT.First_gp) THEN
   !     STOP
   ! ELSE
   ! ENDIF

    ALLOCATE(radialModeData(np*4)  ,&
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
  
!------------------------------
     
 
!  nPts = nPts*2
  WRITE(6,*)'Number of nodes', nPts 
  WRITE(6,*) 'generating the grid'

! hub - to - tip ratio
    sig = radMin/radMax
    
    dr = (radMax-radMin)/REAL(nPts-1,rDef)
    WRITE(6,*) 'Setting radial grid'
    DO i=1,nPts
      r(i) = (radMin + REAL(i-1,rDef)*dr)/radMax
    END DO
    

  WRITE(6,*) 'Setting up the flow variables'
    DO i=1,nPts
      snd(i)    =  1.0_rDef - gm1/2.0_rDef*angom*angom*(1.0_rDef - r(i)*r(i)) ! 
      snd(i)    =  sqrt(snd(i))
      svel(i)   =  angom*r(i)
      smach(i)  =  svel(i)/snd(i)
      smachAnalytical(i)  = SIN(0.1_rDef*r(i)) 
      IF (smachAnalytical(i).GE.1.0_rDef) THEN
          WRITE(6,*) 'the smachAnalytical is grater than one'
      ELSE
      ENDIF
    END DO
!   linear shear in the axial flow
    DO i=1,nPts
      IF (slope.ge.0.0_rDef) THEN
        rvel(i) = slope*(r(i) -1.0_rDef)+rVelMax
      ELSE
        rvel(i) = slope*(r(i) -sig)     +rVelMax
      ENDIF
    ENDDO

    DO i = 1,nPts
      rmach(i) = rvel(i)/snd(i) 
      rmachAnalytical(i)  = SIN(0.1_rDef*r(i)) 
      IF (rmachAnalytical(i).GE.1.0_rDef) THEN
          WRITE(6,*) 'the smachAnalytical is grater than one'
      ELSE
      ENDIF
    ENDDO

! Calculated 
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
    
    CALL GetModeData(object          = swirlClassObject,&
                     modeNumber      = modeNumber      ,&
                     axialWavenumber = axialWavenumber,&
                     radialModeData  = radialModeData )
    
    CALL FindResidualData(object     = swirlClassObject,&
                          modeNumber = numModes        ,&
                          S          = residualVector)
    
    CALL DestroyObject(object = swirlClassObject)
    
    
    ! Calculated 
    CALL CreateObject(object    = swirlClassObjectAnalytical ,&
                       mm       = mm,      &
                       np       = np,      &
                       sig      = sig,     &
                       AxialMachData = rmachAnalytical,&
                       ThetaMachData = smachAnalytical,& 
                       ak       = ak,      &
                       etah     = etah,    &
                       etad     = etad,    &
                       ifdff    = ifdff,   &
                       ed2      = ed2,     &
                       ed4      = ed4)     
    
    CALL GetModeData(object          = swirlClassObjectAnalytical,&
                     modeNumber      = modeNumber      ,&
                     axialWavenumber = axialWavenumber,&
                     radialModeData  = radialModeData )
    
    CALL FindResidualData(object     = swirlClassObjectAnalytical,&
                          modeNumber = numModes        ,&
                          S          = residualVectorAnalytical)
    
    
    CALL DestroyObject(object = swirlClassObjectAnalytical)
   
    DO i = 1,nPts*4
      errorMMS(i) = REAL(ABS(residualVector(i) - residualVectorAnalytical(i)),rDef)
      errorSum    = errorSum +  errorMMS(i)**2
    ENDDO 
  
    L2res = SQRT((errorSum)/(nPts*4.0_rDef))
  
  !  WRITE(8,*)  REAL(dr), REAL(L2res)
    WRITE(6,*)  (dr), (L2res)
  
    IF (L2res.NE.L2res) THEN
        WRITE(6,*) 'L2res is NaN'
        STOP
    ELSE
    ENDIF
  
    DEALLOCATE(radialModeData          ,&
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
  
    L2res    = REAL(0.0_rDef,KIND=REAL64)
    errorSum = REAL(0.0_rDef,KIND=REAL64)
  
ENDDO

CLOSE(8)
END PROGRAM 
