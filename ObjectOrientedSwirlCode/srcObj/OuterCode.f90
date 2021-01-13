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
           numModes,&
           modeNumber 
                              
REAL(KIND=REAL64) ::  &
                     ed2,   &
                     ed4,   &
                     sig   

COMPLEX(KIND=REAL64) :: ak, etah, etad,&
                        axialWavenumber
COMPLEX(KIND=REAL64) , DIMENSION(:), ALLOCATABLE :: radialModeData,&
                                                    residualVector



CHARACTER(16) :: swrlFileName 
CHARACTER(16) :: machFileName

swrlFileName = 'swrl.input'
machFileName = 'mach.input'
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
ALLOCATE(radialModeData(np*4),residualVector(np*4))
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
                      modeNumber = numModes)

CALL DestroyObject(object = swirlClassObject)

!WRITE(6,*) swirlClassObject



END PROGRAM 
