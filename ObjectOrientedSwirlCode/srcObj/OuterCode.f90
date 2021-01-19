PROGRAM OuterCode
 USE, INTRINSIC  :: ISO_FORTRAN_ENV
 USE SwirlClassObj
 IMPLICIT NONE

   INTEGER, PARAMETER :: rDef = REAL64
! defining a new derived data type

TYPE(SwirlClassType) :: swirlClassObject

! original inputs
INTEGER  :: &
           ifdff,   & ! finite difference flag
           mm,      & ! mode order
           np,      & ! number of points
           i,      & ! number of points
           numModes,& ! number of radial modes
           modeNumber 


COMPLEX(KIND=REAL64) :: ak             ,&
                        etah, etad     ,&
                        axialWavenumber,&
                        dr             ,&
                        gam            ,&
                        gm1            

REAL(KIND=REAL64), DIMENSION(:),ALLOCATABLE :: r, &
                                                rhoMean       ,&                                 
                                                rhoVrMean     ,&
                                                rhoVthetaMean ,&
                                                rhoVxMean     ,&
                                                etotalMean    ,&
                                                rmach         ,&
                                                smach         ,&
                                                rvel          ,&
                                                svel          ,&
                                                snd


REAL(KIND=REAL64) ::  &
                     ed2,   & !2nd order smoothing coefficient
                     ed4,   & !4th order smoothing coefficient
                     sig      ! hub-to-tip ratio 

                    
COMPLEX(KIND=REAL64) , DIMENSION(:), ALLOCATABLE :: radialModeData,&
                                                    residualVector


  INTEGER, PARAMETER :: nPts = 201
  REAL(KIND=rDef), PARAMETER :: radMin  = 0.2_rDef,  &
                                radMax  = 1.0_rDef,  &
                                rVelMax = 0.00_rDef, &
                                slope   = 0.0_rDef,  &
                                angom   = 0.00_rDef

!
mm     = 0
np     = 16
sig    = 0.20_rDef   
ak     = CMPLX(20.0,0,rDef)  
etah   =  0.40_rDef
etad   =  0.70_rDef
ifdff  =  1
ed2    =  0.0_rDef
ed4    =  0.0_rDef
numModes = np
modeNumber = 2

ALLOCATE(radialModeData(np*4),&
         residualVector(np*4),r(np),&
         snd(np),svel(np),smach(np),rmach(np),rvel(np)

!------------------------------
     

! generate the grid

  sig = radMin/radMax

  dr = (radMax-radMin)/REAL(nPts-1,rDef)

  DO i=1,np
   r(i) = (radMin + REAL(i-1,rDef)*dr)/radMax
  END DO

! solid-body swirl

  gam = 1.4_rDef
  gm1 = gam - 1.0_rDef

  DO i=1,np
   snd(i)  =  1.0_rDef -gm1/2.0_rDef*angom*angom*(1.0_rDef -r(i)*r(i))
   snd(i)  =  sqrt(snd(i))

   svel(i)   =  angom*r(i)
   smach(i)  =  svel(i)/snd(i)

  END DO

! linear shear in the axial flow

  DO i=1,np
   if (slope.ge.0.0_rDef) then
    rvel(i) = slope*(r(i) -1.0_rDef)+rVelMax
   else
    rvel(i) = slope*(r(i) -sig)     +rVelMax
   endif
  enddo

  do i = 1,np
   rmach(i) = rvel(i)/snd(i)
  enddo



CALL CreateObject(object    = swirlClassObject ,&
                   mm       = mm,      &
                   np       = np,      &
                   sig      = sig,     &
                   ak       = ak,      &
                   etah     = etah,    &
                   etad     = etad,    &
                   ifdff    = ifdff,   &
                   ed2      = ed2,     &
                   ed4      = ed4)     

CALL GetModeData(object          = swirlClassObject,&
                 modeNumber      = modeNumber      ,&
                 axialWavenumber = axialWavenumber,&
                 radialModeData  = radialModeData )

CALL FindResidualData(object     = swirlClassObject,&
                      modeNumber = numModes        ,&
                      S          = residualVector)



CALL DestroyObject(object = swirlClassObject)

!WRITE(6,*) swirlClassObject



END PROGRAM 
