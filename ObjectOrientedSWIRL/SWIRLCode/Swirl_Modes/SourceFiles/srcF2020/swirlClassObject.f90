      MODULE swirlClassObject
!
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

          IMPLICIT NONE

          PRIVATE
          PUBLIC ::&
              CreateObject   ,&
              DestroyObject  ,&
              SwirlClassType!, & GetModeData    ,& GetMeanFlowData, & FindResidualData
! Interfaces

! Creates a derived data type containing SWIRL's essential modules
          INTERFACE CreateObject
              MODULE PROCEDURE CreateSwirlClassObject
          END INTERFACE CreateObject

! Finds S = [A]{x} - i*eigVal*[B]{x}, x \equiv eigen vector
!          INTERFACE FindResidualData
!              MODULE PROCEDURE FindResidualVector
!          END INTERFACE FindResidualData
!
!          INTERFACE GetModeData
!              MODULE PROCEDURE GetRadialModeData
!          END INTERFACE GetModeData
!
!          INTERFACE GetMeanFlowData
!              MODULE PROCEDURE GetMeanData
!          END INTERFACE GetMeanFlowData

! Deallocates any arrays
          INTERFACE DestroyObject
              MODULE PROCEDURE DestroySwirlClassObject
          END INTERFACE DestroyObject

          INTEGER, PARAMETER:: rDef = REAL64
! Type Declaration and necessary variables
          TYPE SwirlClassType
              PRIVATE  ! prevents user from accessing any of the following variables

              LOGICAL :: &
                  isInitialized = .FALSE.        ! flag to identify if object exists

              INTEGER :: &
                  azimuthalMode            ,&    ! m, Circumfirential mode number
                  numberOfRadialPoints     ,&    ! npts, number of radial mesh points
                  numberOfPropagatingModes, &    ! number of modes that the user wants
                  FiniteDifferenceFlag     ,&    ! Use central FD for derivatives, ! 1 = 2nd Order, 2 = 4th Order
                  PrintToggle                    ! Turns on print to screen

              REAL(KIND = REAL64) :: &
                  hubTipRatio        ,&
                  secondOrderSmoother, &!derivative "smoothers"
                  fourthOrderSmoother  !"          "

              REAL(KIND = REAL64), DIMENSION(:), ALLOCATABLE :: &
                  y,     &! used to map grid on a-1 to 1 scale
                  rwork, &! needed for ZGGEV
                  r,     &! radial locations
                  rmx,   &! axial mach number
                  drm,   &! derivative of the axial mach number
                  rmt,   &! theta mach number
                  drt,   &! derivative of the theta mach number
                  snd,   &! speed of sound
                  dsn,   &! derivative of the speed of sound, back calculated from mach data
                  rho,   &! flow densityas a function of snd
                  akap

              REAL(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE ::&
                  dl1

              COMPLEX(KIND = REAL64) ::&
                  frequency        , &
                  hubLinerAdmittance, &
                  ductLinerAdmittance!, & L2N

              COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE ::&
                  aa, bb
              COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE ::&
                  aa_before, bb_before

              COMPLEX(KIND = REAL64), DIMENSION(:), ALLOCATABLE ::&
                  alpha, &
                  beta,  &
                  work,  &
                  vph,   &
                  S_MMS, &
                  wvn

              COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE :: &
                  S_MMS_Array, &
                  vl, &
                  vr


          END TYPE SwirlClassType

! Local Variable Declaration

          CHARACTER :: &
              jobvl = 'N' ,&
              jobvr = 'V'

! additional variables, implicitly defined in the original code

          INTEGER ::&
          ! icomp   = 0,  &
              ir      = 2,  &
          ! irepeat = 0,  &
              is      = 5!,  &
          ! itest   = 0,  &
          ! ix      = 0,  &
          ! PrintToggle = 6 ! set this variable to 6 to see progress in terminal

! JS: added i and j to zero out matricies

          ! INTEGER :: i , j
          REAL(KIND = REAL64) ::&
              angom  = 0.00_rDef, &
              gam    = 0.00_rDef,   &
              rxmax  = 0.00_rDef, &
              slope  = 0.00_rDef
! 100       continue
      CONTAINS

          SUBROUTINE CreateSwirlClassObject(&
              object          , &
              azimuthalMode   , &
              np              , &
              sig             , &
              AxialMachData   , &
              ThetaMachData   , &
          ! SoundSpeed      , &
              ak              , &
              etah            , &
              etad            , &
              ifdff           , &
              ed2             , &
              ed4)

              TYPE(SwirlClassType), INTENT(INOUT) ::&
                  object

              INTEGER, INTENT(INOUT) :: &
                  ifdff,   &
                  azimuthalMode, &
                  np

              REAL(KIND = REAL64), INTENT(INOUT) :: &
                  ed2,   &
                  ed4,   &
                  sig

              REAL(KIND = REAL64), DIMENSION(:), INTENT(INOUT) :: &
                  AxialMachData, &
                  ThetaMachData!, &
              ! SoundSpeed

!

              COMPLEX(KIND = REAL64), INTENT(IN) ::&
                  etah, etad, ak

              INTEGER ::&
                  np4

! Set user input to the object 'properties';
              object%azimuthalMode        = azimuthalMode
              object%numberOfRadialPoints = np
              object%hubTipRatio          = sig
              object%frequency            = ak
              object%hubLinerAdmittance   = etah
              object%ductLinerAdmittance  = etad
              object%FiniteDifferenceFlag = ifdff
              object%secondOrderSmoother  = ed2
              object%fourthOrderSmoother  = ed4
              object%rmx                  = AxialMachData
              object%rmt                  = ThetaMachData

              np4 = object%numberOfRadialPoints*4

!
! Get input quantities.
!              CALL input(&
!                  mode   = object%mm,      &
!                  np     = object%np,      &
!                  np4    = object%np4,     &
!                  rho    = object%sig,     &
!                  freq   = object%ak,      &
!                  ixp    = object%ix,      &
!                  nxp    = object%nx,      &
!                  irr    = object%ir,      &
!                  rxmax  = object%rxmax,   &
!                  slp    = object%slope,   &
!                  iss    = object%is,      &
!                  ang    = object%angom,   &
!                  gm     = object%gam,     &
!                  vlchar = object%jobvl,   &
!                  vrchar = object%jobvr,   &
!                  itst   = object%itest,   &
!                  etah   = object%etah,    &
!                  etad   = object%etad,    &
!                  irpt   = object%irepeat, &
!                  ifdff  = object%ifdff,   &
!                  eps2   = object%ed2,     &
!                  eps4   = object%ed4,     &
!                  icomp  = object%icomp)
!
! NEW: allocate data arrays
!

              ALLOCATE(&
                  object%dl1(object%numberOfRadialPoints,object%numberOfRadialPoints),   &
                  object%y(object%numberOfRadialPoints),        &
                  object%rwork(8*np4), &
                  object%r(object%numberOfRadialPoints),        &
              ! object%rmx(object%numberOfRadialPoints),      &
                  object%drm(object%numberOfRadialPoints),      &
              ! object%rmt(object%numberOfRadialPoints),      &
                  object%drt(object%numberOfRadialPoints),      &
                  object%snd(object%numberOfRadialPoints),      &
                  object%dsn(object%numberOfRadialPoints),      &
                  object%rho(object%numberOfRadialPoints),      &
                  object%akap(np4),    & ! was np in code, but may go to np4 in analysisModule
                  object%alpha(np4),   &
                  object%beta(np4),    &
                  object%work(2*np4),  &
                  object%vph(np4),     &
                  object%wvn(np4),     &
                  object%aa(np4,np4),  &
                  object%bb(np4,np4),  &
                  object%vl(np4,np4),  &
                  object%vr(np4,np4))

!
! Set up Gauss-Lobatto grid and compute Chebyshev derivative matrix.
              if (is .ne. -1) then

                  if (object%FiniteDifferenceFlag.eq.0) then
                      CALL grid(&
                          np  = object%numberOfRadialPoints,  &
                          sig = object%hubTipRatio, &
                          x   = object%y,   &
                          r   = object%r)

                      CALL derivs(&
                          np  = object%numberOfRadialPoints,  &
                          sig = object%hubTipRatio, &
                          dl1 = object%dl1, &
                          ed2 = object%secondOrderSmoother, &
                          ed4 = object%fourthOrderSmoother)
                  else

                      CALL fdgrid(&
                          np  = object%numberOfRadialPoints,  &
                          sig = object%hubTipRatio, &
                          x   = object%y,   &
                          r   = object%r)

                      CALL fdrivs(&
                          np     = object%numberOfRadialPoints,    &
                          sig    = object%hubTipRatio,   &
                          dl1    = object%dl1,   &
                          iorder = object%FiniteDifferenceFlag, &
                          ed2    = object%secondOrderSmoother,   &
                          ed4    = object%fourthOrderSmoother)

                  endif
!
! Speed of sound and Mach number distributions and output.

!       CALL sndspd(np    = np,    &
!                   rr    = r,     &
!                   rmsw  = rmt,   &
!                   asnd  = snd,   &
!                   dsnd  = dsn,   &
!                   dd    = dl1,   &
!                   rhob  = rho,   &
!                   angom = angom, &
!                   gam   = gam,   &
!                   sig   = sig,   &
!                   is    = is)

!       CALL smach(npts  = np,    &
!                  rr    = r,     &
!                  rmsw  = rmt,   &
!                  rmswp = drt,   &
!                  snd   = snd,   &
!                  dsn   = dsn,   &
!                  dd    = dl1,   &
!                  angom = angom, &
!                  gam   = gam,   &
!                  rro   = sig,   &
!                  is    = is)

                  CALL smachAndSndspd(&
                      npts  = object%numberOfRadialPoints,    &
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

                  CALL rmach(&
                      npts  = object%numberOfRadialPoints,    &
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
                  CALL interp(&
                      np    = object%numberOfRadialPoints,    &
                      sig   = object%hubTipRatio,   &
                      rr    = object%r,     &
                      rmx   = object%rmx,   &
                      drm   = object%drm,   &
                      rmt   = object%rmt,   &
                      drt   = object%drt,   &
                      snd   = object%snd,   &
                      dsn   = object%dsn,   &
                      dd    = object%dl1,   &
                      ifdff = object%FiniteDifferenceFlag, &
                      ed2   = object%secondOrderSmoother,   &
                      ed4   = object%fourthOrderSmoother)
              endif

! output the mean flow data.

              CALL machout(&
                  npts  = object%numberOfRadialPoints,  &
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

              CALL globalM(&
                  np   = object%numberOfRadialPoints,  &
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

              CALL boundary(&
                  np   = object%numberOfRadialPoints,   &
                  sig  = object%hubTipRatio,  &
                  ak   = object%frequency,   &
                  etah = object%hubLinerAdmittance, &
                  etad = object%ductLinerAdmittance, &
                  rmx  = object%rmx,  &
                  rmt  = object%rmt,  &
                  dd   = object%dl1,  &
                  aa   = object%aa,   &
                  bb   = object%bb)

              CALL analysis(&
                  np    = object%numberOfRadialPoints,    &
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
                  vl    = object%vl,    &
                  vr    = object%vr,    &
                  work  = object%work,  &
                  rwork = object%rwork, &
                  gam   = object%wvn,   &
                  jobvl = jobvl, &
                  jobvr = jobvr, &
                  mm    = object%azimuthalMode,    &
                  ir    = ir,    &
                  is    = is,    &
                  slp   = slope, &
                  vphi  = object%vph,   &
                  akap  = object%akap)

              object%isInitialized = .TRUE.

              IF (object%isInitialized) THEN

              ELSE
                  WRITE(6,*) 'ERROR: The object is not initialized'
                  CONTINUE
              ENDIF
          END SUBROUTINE CreateSwirlClassObject
          ! CALL output(&
          !     np     = object%numberOfRadialPoints,    &
          !     np4    = object%np4,   &
          !     mode   = object%mm,    &
          !     rho    = object%hubTipRatio,   &
          !     omega  = object%ak,    &
          !     rmax   = object%rxmax, &
          !     slp    = object%slope, &
          !     ang    = object%angom, &
          !     gam    = object%gam,   &
          !     egv    = object%jobvr, &
          !     attenh = object%etah,  &
          !     attend = object%etad,  &
          !     rmx    = object%rmx,   &
          !     drm    = object%drm,   &
          !     rmt    = object%rmt,   &
          !     drt    = object%drt,   &
          !     snd    = object%snd,   &
          !     rr     = object%r,     &
          !     wvn    = object%wvn,   &
          !     vrm    = object%vr,    &
          !     vphi   = object%vph,   &
          !     is     = object%is,    &
          !     icomp  = object%icomp)
!
          ! if (irepeat.eq.1) goto 100
!
!
! NEW: deallocate data arrays
!
          SUBROUTINE DestroySwirlClassObject(&
              object)

              TYPE(SwirlClassType), INTENT(INOUT) ::&
                  object

              DEALLOCATE(&
                  object%dl1,   &
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
                  ! object%aa_before,    &
                  object%bb,    &
                  ! object%bb_before,    &
                  ! object%S_MMS, &
                  ! object%S_MMS_Array, &
                  object%vl,    &
                  object%vr)
          END SUBROUTINE DestroySwirlClassObject
!
      END MODULE swirlClassObject
