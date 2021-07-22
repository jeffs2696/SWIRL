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
      ! USE inputModule
      ! USE interpModule
      ! USE machoutModule
      USE rmachModule
      USE smachAndSndspdModule
      USE FindResidualVectorModule
      USE L2NormModule

      IMPLICIT NONE

      PRIVATE
      PUBLIC ::&
          CreateObject   ,&
          DestroyObject  ,&
          SwirlClassType ,&
          GetMeanFlowData, & 
          FindResidualData, & 
          GetModeData

! Interfaces

! Creates a derived data type containing SWIRL's essential modules
      INTERFACE CreateObject
          MODULE PROCEDURE CreateSwirlClassObject
      END INTERFACE CreateObject

! Finds S = [A]{x} - i*eigVal*[B]{x}, x \equiv eigen vector
         INTERFACE FindResidualData
             MODULE PROCEDURE GetResidualVector
         END INTERFACE FindResidualData

          INTERFACE GetModeData
              MODULE PROCEDURE GetRadialModeData
          END INTERFACE GetModeData

      INTERFACE GetMeanFlowData
          MODULE PROCEDURE GetMeanData
      END INTERFACE GetMeanFlowData

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
              ductLinerAdmittance 

          COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE ::&
              aa, bb
          COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE ::&
              aa_before, bb_before

          COMPLEX(KIND = REAL64), DIMENSION(:), ALLOCATABLE ::&
              alpha, &
              beta,  &
              work,  &
              vph,   &
              S_MMS ,&
              wvn

          COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE :: &
              ! S_MMS , &
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
      is      = 5,  &
      ! itest   = 0,  &
      ! ix      = 0,  &
      PrintToggle = 6 ! set this variable to 6 to see progress in terminal

! JS: added i and j to zero out matricies

      ! INTEGER :: i , j
      REAL(KIND = REAL64) ::&
          ! angom  = 0.00_rDef, &
          ! gam    = 1.400_rDef,   & !not used in smachModule
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
              ak              , &
              etah            , &
              etad            , &
              ifdff           )

          TYPE(SwirlClassType), INTENT(INOUT) ::&
              object

          INTEGER, INTENT(INOUT) :: &
              ifdff,   &
              azimuthalMode, &
              np

          REAL(KIND = REAL64), INTENT(INOUT) :: &
              sig

          REAL(KIND = REAL64), DIMENSION(:), INTENT(INOUT) :: &
              AxialMachData, &
              ThetaMachData!, &
          ! SoundSpeed

!

          INTEGER ::&
              np4

          REAL(KIND = REAL64) :: &
              ed2 ,   &
              ed4

          COMPLEX(KIND = REAL64), INTENT(IN) ::&
              etah, etad, ak


! Set user input to the object 'properties';
          ed2 = 0.0_rDef
          ed4 = 0.0_rDef
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
              object%S_MMS(np4)        ,&
              object%vph(np4),     &
              object%wvn(np4),     &
              object%aa(np4,np4),  &
              object%aa_before(np4,np4),  &
              object%bb(np4,np4),  &
              object%bb_before(np4,np4),  &
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

                  WRITE(PrintToggle,*) 'Entering fdgrid CALL'
                  CALL fdgrid(&
                      np  = object%numberOfRadialPoints,  &
                      sig = object%hubTipRatio, &
                      x   = object%y,   &
                      r   = object%r)
                  WRITE(PrintToggle,*) 'Leaving fdgrid CALL'

                  WRITE(PrintToggle,*) 'Entering fdrivs CALL'
                  CALL fdrivs(&
                      np     = object%numberOfRadialPoints,    &
                      sig    = object%hubTipRatio,   &
                      dl1    = object%dl1,   &
                      iorder = object%FiniteDifferenceFlag, &
                      ed2    = object%secondOrderSmoother,   &
                      ed4    = object%fourthOrderSmoother)
                  WRITE(PrintToggle,*) 'Leaving fdrivs CALL'

              endif

              WRITE(PrintToggle,*) 'Entering smachAndSndspd CALL'
              CALL smachAndSndspd(&
                  npts  = object%numberOfRadialPoints,    &
                  rr    = object%r,     &
                  rmsw  = object%rmt,   &
                  rmswp = object%drt,   &
                  snd   = object%snd,   &
                  dsn   = object%dsn,   &
                  dd    = object%dl1   )
              WRITE(PrintToggle,*) 'Leaving smachAndSndspd CALL'

              WRITE(PrintToggle,*) 'Entering rmach CALL'
              CALL rmach(&
                  npts  = object%numberOfRadialPoints,    &
                  rmch  = object%rmx,   &
                  drm   = object%drm,   &
                  dd    = object%dl1    &
                  )
              WRITE(PrintToggle,*) 'Leaving rmach CALL'

          else
              ! CALL interp(&
              !     np    = object%numberOfRadialPoints,    &
              !     sig   = object%hubTipRatio,   &
              !     rr    = object%r,     &
              !     rmx   = object%rmx,   &
              !     drm   = object%drm,   &
              !     rmt   = object%rmt,   &
              !     drt   = object%drt,   &
              !     snd   = object%snd,   &
              !     dsn   = object%dsn,   &
              !     dd    = object%dl1,   &
              !     ifdff = object%FiniteDifferenceFlag, &
              !     ed2   = object%secondOrderSmoother,   &
              !     ed4   = object%fourthOrderSmoother)
          endif

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

          object%aa_before = object%aa
          object%bb_before = object%bb

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
      SUBROUTINE GetMeanData(&
              object   ,&
              axialMach, &
              thetaMach, &
              axialMach_dr, &
              thetaMach_dr, &
              SoundSpeed  , &
              SoundSpeed_dr, &
              radialData)

! The goal of this subroutine is to extract the mean flow data that we input plus the rest of the mean flow parameters
! that SWIRL generated
! 
!

          TYPE(SwirlClassType), INTENT(IN) ::&
              object
          REAL(KIND = rDef), DIMENSION(object%numberOfRadialPoints), INTENT(OUT) :: &
              axialMach, &
              thetaMach, &
              axialMach_dr, &
              thetaMach_dr, &
              SoundSpeed  , &
              SoundSpeed_dr, &
              radialData

          ! the data we originally sent in
          axialMach     = object%rmx
          axialMach_dr  = object%drm
          thetaMach     = object%rmt
          thetaMach_dr  = object%drt
          SoundSpeed    = object%snd
          SoundSpeed_dr = object%dsn
          radialData    = object%r

          ! the other mean flow data from SWIRL


      END SUBROUTINE GetMeanData

      SUBROUTINE GetRadialModeData(&
          object        , &
          eigenValue    , &
          eigenVector   , &
          eigenIndex)

          TYPE(SwirlClassType), INTENT(IN) ::&
              object

          INTEGER, INTENT(IN) :: &
              eigenIndex 

          COMPLEX(KIND = rDef), INTENT(INOUT) :: &
              eigenValue

          COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4), INTENT(INOUT) :: &
              eigenVector

          eigenValue = object%wvn(eigenIndex)
          eigenVector= object%vr(:,eigenIndex)

          ! WRITE(6,*) (eigenValue), SIZE(eigenVector), (eigenIndex)


      END SUBROUTINE GetRadialModeData 

      SUBROUTINE GetResidualVector(&
          object                  ,&
          eigenValue              ,&
          eigenVector             ,&
          S)

          TYPE(SwirlClassType), INTENT(IN) ::&
              object

          COMPLEX(KIND = rDef), INTENT(INOUT) :: &
              eigenValue

          COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4), INTENT(INOUT) :: &
              eigenVector

          COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4), INTENT(INOUT) :: &
          S

          INTEGER :: SZ

          IF (object%isInitialized.eqv..TRUE.) then

              SZ = SIZE(S)
              ! WRITE(6,*) eigenValue,eigenVector
              CALL getSvector( &
                  A      = object%aa_before ,   &
                  B      = object%bb_before ,   &
                  x      = eigenVector        ,   &
                  lambda = eigenValue       ,   &
                  np4    = SZ              ,   &
                  S_MMS  = S ) 

          ENDIF
      ! S = object%S_MMS
      ! write(6,*) 

      END SUBROUTINE GetResidualVector
      SUBROUTINE DestroySwirlClassObject(&
              object)

          TYPE(SwirlClassType), INTENT(INOUT) ::&
              object

          object%isInitialized = .FALSE.

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
              object%aa_before,    &
              object%bb,    &
              object%bb_before,    &
              object%vl,    &
              object%vr,    &
              object%S_MMS)
      END SUBROUTINE DestroySwirlClassObject
!
  END MODULE swirlClassObject
