MODULE swirlClassObj

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE analysisModule
  USE boundaryModule
  USE derivsModule
  USE fdgridModule
  USE fdrivsModule
  USE globalModule
  USE gridModule
  USE inputModule
  USE interpModule
  USE machoutModule
  USE outputModule
  USE rmachModule
  USE smachAndSndspdModule
  USE FindResidualVectorModule

  USE L2NormModule

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CreateObject, DestroyObject, SwirlClassType, GetModeData,FindResidualData
! Interfaces 

  INTERFACE CreateObject
    MODULE PROCEDURE CreateSwirlClassObject
  END INTERFACE CreateObject
  INTERFACE FindResidualData
    MODULE PROCEDURE FindResidualVector 
  END INTERFACE FindResidualData


  INTERFACE GetModeData
    MODULE PROCEDURE GetRadialModeData
  END INTERFACE GetModeData

  INTERFACE DestroyObject
    MODULE PROCEDURE DestroySwirlClassObject
  END INTERFACE DestroyObject

  INTEGER, PARAMETER :: rDef = REAL64
! Type Declaration
  TYPE SwirlClassType
    PRIVATE

    LOGICAL :: isInitialized = .FALSE.
    INTEGER :: azimuthalMode              , &
               numberOfRadialPoints       , &
               numberOfPropagatingModes   , &
               FiniteDifferenceFlag     

    REAL(KIND=REAL64) :: hubTipRatio ,&
                         secondOrderSmoother,&
                         fourthOrderSmoother
  
    REAL(KIND=REAL64), DIMENSION(:), ALLOCATABLE :: y,     &
                                                    rwork, &
                                                    r,     &
                                                    rmx,   &
                                                    drm,   &
                                                    rmt,   &
                                                    drt,   &
                                                    snd,   &
                                                    dsn,   &
                                                    rho,   &
                                                    akap     
  
    REAL(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: dl1
  
    COMPLEX(KIND=REAL64) :: frequency        , &
                            hubLinerAdmittance,&
                            ductLinerAdmittance,&
                            L2N
    
    COMPLEX(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: aa,bb
    COMPLEX(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: aa_before,bb_before
  
    COMPLEX(KIND=REAL64), DIMENSION(:), ALLOCATABLE :: alpha, &
                                                       beta,  &
                                                       work,  &
                                                       vph,   &
                                                       S_MMS,&
                                                       wvn
  
    COMPLEX(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: S_MMS_Array,&
                                                         vl, &
                                                         vr



  END TYPE SwirlClassType
! Local Variable Declaration

  CHARACTER :: jobvl = 'N' ,&
             jobvr = 'V'

! additional variables, implicitly defined in the original code

  INTEGER :: icomp   = 0,  &
             ir      = 2,  &
             irepeat = 0,  &
             is      = 5,  &
             itest   = 0,  &
             ix      = 0
!
  REAL(KIND=REAL64) :: angom  = 0.00_rDef, &
                       gam    = 0.00_rDef,   &
                       rxmax  = 0.00_rDef, &
                       slope  = 0.00_rDef

! "sig" is the hub/duct ratio.
! "ak"  is the reduced frequency omega r_d/c.
! "azimuthalMode"  is the circumferential mode number.
! "rmx" is the axial Mach number.
! "drm" is the derivative of the axial Mach number.
! "r" is the physical space vector, from sigma to 1.
! "y" is the mapping into -1 to 1.
!
! 100  continue
!
! Get input quantities.
  CONTAINS

  SUBROUTINE CreateSwirlClassObject(object ,& 
                                    mm     ,&
                                    np     ,&
                                    sig    ,&
                                    ak     ,&
                                    etah   ,&
                                    etad   ,&
                                    ifdff  ,&
                                    ed2    ,&
                                    ed4)

    TYPE(SwirlClassType), INTENT(INOUT) :: object
    
    INTEGER, INTENT(INOUT) :: ifdff,   &
                              mm,&
                              np!,      &
                    
    REAL(KIND=REAL64),INTENT(INOUT) :: ed2,   & 
                                       ed4,   &
                                       sig
    !
    
    COMPLEX(KIND=REAL64),INTENT(IN) :: etah,etad,ak
    
    INTEGER ::                np4                              
    
    ! Set user input to the object 'properties';
    object%azimuthalMode        = mm
    object%numberOfRadialPoints = np
    object%hubTipRatio          = sig
    object%frequency            = ak
    object%hubLinerAdmittance   = etah
    object%ductLinerAdmittance  = etad
    object%FiniteDifferenceFlag = ifdff
    object%secondOrderSmoother  = ed2
    object%fourthOrderSmoother  = ed4
    
    np4 = np*4
    
    
    
    ALLOCATE(object%dl1(object%numberOfRadialPoints,object%numberOfRadialPoints),   &
               object%y(object%numberOfRadialPoints),        &
               object%rwork(8*np4)                          ,&
               object%r(object%numberOfRadialPoints),        &
               object%rmx(object%numberOfRadialPoints),      &
               object%drm(object%numberOfRadialPoints),      &
               object%rmt(object%numberOfRadialPoints),      &
               object%drt(object%numberOfRadialPoints),      &
               object%snd(object%numberOfRadialPoints),      &
               object%dsn(object%numberOfRadialPoints),      &
               object%rho(object%numberOfRadialPoints),      &
               object%akap(np4),    & ! was np in code, but may go to np4 in analysisMod
               object%alpha(np4),   &
               object%beta(np4),    &
               object%work(2*np4),  &
               object%S_MMS(np4),   &
               object%S_MMS_Array(np4,np4),   &
               object%vph(np4),     &
               object%wvn(np4),     &
              object%aa(np4,np4),  &
              object%bb(np4,np4),  &
               object%vl(np4,np4),  &
               object%vr(np4,np4))

!
! Set up Gauss-Lobatto grid and compute Chebyshev derivative matrix.

!     If the swirl flow is not provided ... 
      if (is .ne. -1) then


!       If  
        if (ifdff.eq.0) then
          CALL grid(np  = object%numberOfRadialPoints,    &
                    sig = object%hubTipRatio,             &! sig, &
                    x   = object%y,                       &
                    r   = object%r)

          CALL derivs(np  = object%numberOfRadialPoints,  &
                      sig = object%hubTipRatio,           &
                      dl1 = object%dl1,                   &
                      ed2 = object%secondOrderSmoother,   &
                      ed4 = object%fourthOrderSmoother)
        else

          CALL fdgrid(np  = object%numberOfRadialPoints,  &
                      sig = object%hubTipRatio,           &
                      x   = object%y,                     &
                      r   = object%r)

          CALL fdrivs(np     = object%numberOfRadialPoints,    &
                      sig    = object%hubTipRatio,             &
                      dl1    = object%dl1,                     &
                      iorder = object%FiniteDifferenceFlag,    &!ifdff, &
                      ed2    = object%secondOrderSmoother,     &
                      ed4    = object%fourthOrderSmoother)

        endif


        CALL smachAndSndspd(npts  = object%numberOfRadialPoints,    &
                            rr    = object%r,     &
                            rmsw  = object%rmt,   &
                            rmswp = object%drt,   &
                            snd   = object%snd,   &
                            dsn   = object%dsn,   &
                            dd    = object%dl1,   &
                            rhob  = object%rho,   &
                            angom = angom, &
                            gam   = gam,   &
                            sig   = object%hubTipRatio,   &
                            is    = is)

        CALL rmach(npts  = object%numberOfRadialPoints,    &
                   rr    = object%r,     &
                   rmch  = object%rmx,   &
                   drm   = object%drm,   &
                   snd   = object%snd,   &
                   dsn   = object%dsn,   &
                   dd    = object%dl1,   &
                   rxmax = rxmax, &
                   slope = slope, &
                   rro   = object%hubTipRatio,   &
                   ir    = ir)

      else
        CALL interp(np    = object%numberOfRadialPoints,    &
                    sig   = object%hubTipRatio,   &
                    rr    = object%r,     &
                    rmx   = object%rmx,   &
                    drm   = object%drm,   &
                    rmt   = object%rmt,   &
                    drt   = object%drt,   &
                    snd   = object%snd,   &
                    dsn   = object%dsn,   &
                    dd    = object%dl1,   &
                    ifdff = ifdff, &
                    ed2   = object%secondOrderSmoother,   &
                    ed4   = object%fourthOrderSmoother)
      endif

! output the mean flow data.

      CALL machout(npts  = object%numberOfRadialPoints,  &
                   rr    = object%r,   &
                   rmch  = object%rmx, &
                   rmchp = object%drm, &
                   rmsw  = object%rmt, &
                   rmswp = object%drt, &
                   snd   = object%snd, &
                   dsn   = object%dsn, &
                   rhob  = object%rho)
!
! Set up global matrices.

      CALL globalM(np   = object%numberOfRadialPoints,  &
                   np4  = np4, &
                   sig  = object%hubTipRatio, &
                   mode = object%azimuthalMode,  &
                   om   = object%frequency,  &
                   snd  = object%snd, &
                   dd   = object%dl1, &
                   rr   = object%r,   &
                   rx   = object%rmx, &
                   dr   = object%drm, &
                   rt   = object%rmt, &
                   dt   = object%drt, &
                   aa   = object%aa,  &
                   bb   = object%bb)

      CALL boundary(np   = object%numberOfRadialPoints,   &
                    sig  = object%hubTipRatio,  &
                    ak   = ak,   &
                    etah = etah, &
                    etad = etad, &
                    rmx  = object%rmx,  &
                    rmt  = object%rmt,  &
                    dd   = object%dl1,  &
                    aa   = object%aa,   &
                    bb   = object%bb)

      object%aa_before = object%aa
      object%bb_before = object%bb

      !CALL getL2Norm(L2      =object%L2N,&
      !               dataset1= object%aa,&
      !               dataset2= object%aa_before,&
      !               numPoints=object%numberOfRadialPoints)

                
      CALL analysis(np    = object%numberOfRadialPoints,    &
                    np4   = np4,   &
                    ak    = object%frequency,    &
                    rr    = object%r,     &
                    snd   = object%snd,   &
                    rmx   = object%rmx,   &
                    rmt   = object%rmt,   &
                    aa    = object%aa,    &
                    bb    = object%bb,    &
                    alpha = object%alpha, &
                    beta  = object%beta,  &
                    vl    = object%vl,           &
                    vr    = object%vr,           &
                    work  = object%work,         &
                    rwork = object%rwork, &
                    gam   = object%wvn,   &
                    jobvl = jobvl, &
                    jobvr = jobvr, &
                    mm    = object%azimuthalMode,    &
                    ir    = ir,    &
                    is    = is,    &
                   slp   = slope, &
                    vphi  = object%vph,   &
                    akap  = object%akap,&
                    S_MMS = object%S_MMS_Array)


      !CALL getL2Norm(L2      =object%L2N,&
      !               dataset1= object%aa,&
      !               dataset2= object%aa_before,&
      !               numPoints=object%numberOfRadialPoints)
!
!      CALL getSvector( A      = object%aa_before     ,&
!                       B      = object%bb_before    ,&
!                       x      = object%vr           ,&   
!!                       lambda = object%alpha/object%beta  ,&    
!                       np4    = np4,   &
!                       S_MMS  = object%S_MMS)
!     
!
     object%isInitialized = .TRUE.
 
  IF (object%isInitialized) THEN

    print *, 'the Object is initialized'
  ELSE
    print *, 'The object is not initialized'
    CONTINUE
  ENDIF  
      CALL output(np     = object%numberOfRadialPoints,    &
                  np4    = np4,   &
                  mode   = object%azimuthalMode,    &
                  rho    = object%hubTipRatio,   &
                  omega  = ak,    &
                  rmax   = rxmax, &
                  slp    = slope, &
                  ang    = angom, &
                  gam    = gam,   &
                  egv    = jobvr, &
                  attenh = object%hubLinerAdmittance,  &
                  attend = object%ductLinerAdmittance,  &
                  rmx    = object%rmx,   &
                  drm    = object%drm,   &
                  rmt    = object%rmt,   &
                  drt    = object%drt,   &
                  snd    = object%snd,   &
                  rr     = object%r,     &
                  wvn    = object%wvn,   &
                  vrm    = object%vr,    &
                  vphi   = object%vph,   &
                  is     = is,    &
                  icomp  = icomp)
!
!      if (irepeat.eq.1) goto 100


! passing variables to the swirl object

!object%egvOut = vr

!WRITE(6,*) object%egvOut
!
!
! NEW: deallocate data arrays
!

!
!
  END SUBROUTINE CreateSwirlClassObject

  SUBROUTINE FindResidualVector(object    ,& 
                                modeNumber,&
                                S            )
  
  TYPE(SwirlClassType)                             ,INTENT(INOUT) :: object
  
  INTEGER                                          ,INTENT(INOUT) :: modeNumber

  COMPLEX(KIND=REAL64),DIMENSION(object%numberOfRadialPoints*4),INTENT(OUT) :: S

       CALL getSvector(  A      = object%aa_before                                  ,&
                         B      = object%bb_before                                  ,&
                         x      = object%vr(:,modeNumber)                           ,&   
                         lambda = object%alpha(modeNumber)/object%beta(modeNumber)  ,&     
                         np4    = object%numberOfRadialPoints*4                     ,&
                         S_MMS  = object%S_MMS)
   
        S =  object%S_MMS

  END SUBROUTINE FindResidualVector
  SUBROUTINE GetRadialModeData(object,&
                               modeNumber,&
                               axialWavenumber,&
                               radialModeData)
  ! Defining inputs and outputs
  TYPE(SwirlClassType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: modeNumber
  COMPLEX(KIND=REAL64), DIMENSION(object%numberOfRadialPoints*4), INTENT(INOUT) :: radialModeData
  COMPLEX(KIND=REAL64), INTENT(INOUT) :: axialWavenumber
  CONTINUE
  IF (object%isInitialized.eqv..TRUE.) THEN
      axialWavenumber = object%alpha(modeNumber)/object%beta(modeNumber)
      radialModeData  = object%vr(:,modeNumber)
  ELSE
  ENDIF

  END SUBROUTINE GetRadialModeData 
  SUBROUTINE DestroySwirlClassObject(object)

      TYPE(SwirlClassType), INTENT(INOUT) :: object
      
      DEALLOCATE(object%dl1,   &
                 object%y,     &
                 object%rwork, &
                 object%r,     &
                 object%rmx,   &
                 object%drm,   &
                 object%rmt,   &
                 object%drt,   &
                 object%snd,   &
                 object%dsn,   &
                 object%rho,   &
                 object%akap,  &
                 object%alpha, &
                 object%beta,  &
                 object%work,  &
                 object%vph,   &
                 object%wvn,   &
                 object%aa,    &
                 object%bb,    &
                 object%S_MMS, &
                 object%vl,    &
                 object%vr)
     
  END SUBROUTINE DestroySwirlClassObject  
END MODULE swirlClassObj
