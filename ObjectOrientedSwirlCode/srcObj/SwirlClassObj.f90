MODULE swirlClassObj

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE analysisModule          ! Solves the eigenvalue problem
  USE boundaryModule          ! fills [A] and [B] matricies with appropriate BCs
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
  PUBLIC :: CreateObject   ,&
            DestroyObject  ,&    
            SwirlClassType ,&
            GetModeData    ,&
            GetMeanFlowData,&
            FindResidualData
! Interfaces 

  ! Creates a derived data type containing SWIRL's essential modules
  INTERFACE CreateObject
    MODULE PROCEDURE CreateSwirlClassObject
  END INTERFACE CreateObject

  ! Finds S = [A]{x} - i*eigVal*[B]{x}, x \equiv eigen vector
  INTERFACE FindResidualData
    MODULE PROCEDURE FindResidualVector 
  END INTERFACE FindResidualData

  ! Extracts the axial wavenumbers (eigenvalues) and 
  ! perturbation variables (eigenvectors)
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

  INTEGER, PARAMETER :: rDef = REAL64

! Type Declaration and necessary variables
  TYPE SwirlClassType
    PRIVATE ! prevents user from accessing any of the following variables

    LOGICAL :: &
    isInitialized = .FALSE.        ! flag to identify if object exists 

    INTEGER :: &
    azimuthalMode            ,&    ! m, Circumfirential mode number
    numberOfRadialPoints     ,&    ! npts, number of radial mesh points
    numberOfPropagatingModes ,&    ! number of modes that the user wants
    FiniteDifferenceFlag     ,&    ! Use central FD for derivatives,
                                   ! 1 = 2nd Order, 2 = 4th Order  
    PrintToggle                    ! Turns on print to screen

    REAL(KIND=REAL64) :: &
    hubTipRatio        ,&  
    secondOrderSmoother,&!derivative "smoothers"
    fourthOrderSmoother  !"          "
  
    REAL(KIND=REAL64), DIMENSION(:), ALLOCATABLE :: &
    y,     &! used to map grid on a -1 to 1 scale
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

! Needed to decide which eigenvectors to get (See ZGGEV in 
  CHARACTER :: jobvl = 'N' ,&
               jobvr = 'V'

! additional variables, implicitly defined in the original code

  INTEGER :: icomp   = 0,  &
             ir      = 2,  &
             irepeat = 0,  &
             is      = 5,  &
             itest   = 0,  &
             ix      = 0,  &
             PrintToggle = 50
         
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
                                    azimuthalMode     ,&
                                    np     ,&
                                    sig    ,&
                                    AxialMachData,&
                                    ThetaMachData,&
!                                    SoundSpeed   ,&
                                    ak     ,&
                                    etah   ,&
                                    etad   ,&
                                    ifdff  ,&
                                    ed2    ,&
                                    ed4)

    TYPE(SwirlClassType), INTENT(INOUT) :: &
    object
    
    INTEGER, INTENT(INOUT) :: &
    ifdff,   &
    azimuthalMode,&
    np
                    
    REAL(KIND=REAL64),INTENT(INOUT) :: &
    ed2,   & 
    ed4,   &
    sig

    REAL(KIND=REAL64),DIMENSION(:), INTENT(INOUT) :: &
    AxialMachData,&
    ThetaMachData!,&
!    SoundSpeed

    !
    
    COMPLEX(KIND=REAL64),INTENT(IN) :: etah,etad,ak
    
    INTEGER ::                np4                              
    
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
    
    np4 = np*4
    
    OPEN(PrintToggle) 
    
    WRITE(PrintToggle,*) np

    ALLOCATE(object%dl1(object%numberOfRadialPoints,object%numberOfRadialPoints),   &
               object%y(object%numberOfRadialPoints),        &
               object%rwork(8*np4)                          ,&
               object%r(object%numberOfRadialPoints),        &
!               object%rmx(object%numberOfRadialPoints),      &
               object%drm(object%numberOfRadialPoints),      &
!               object%rmt(object%numberOfRadialPoints),      &
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
          WRITE(PrintToggle,*) 'Entering gridModule'
         CALL grid(np  = object%numberOfRadialPoints,    &
                   sig = object%hubTipRatio,             &! sig, &
                   x   = object%y,                       &!
                   r   = object%r)

         CALL derivs(np  = object%numberOfRadialPoints,  &
                     sig = object%hubTipRatio,           &
                     dl1 = object%dl1,                   &
                     ed2 = object%secondOrderSmoother,   &
                     ed4 = object%fourthOrderSmoother)
       else
          WRITE(PrintToggle,*) 'Entering fdgridModule'
         CALL fdgrid(np  = object%numberOfRadialPoints,  &
                     sig = object%hubTipRatio,           &
                     x   = object%y,                     &
                     r   = object%r)
          WRITE(PrintToggle,*) 'Leaving fdgridModule'
         
          WRITE(PrintToggle,*) 'Entering fdrivsModule'
         CALL fdrivs(np     = object%numberOfRadialPoints,    &
                     sig    = object%hubTipRatio,             &
                     dl1    = object%dl1,                     &
                     iorder = object%FiniteDifferenceFlag,    &!ifdff, &
                     ed2    = object%secondOrderSmoother,     &
                     ed4    = object%fourthOrderSmoother)
          WRITE(PrintToggle,*) 'Leaving fdrivsModule'

       endif

        WRITE(PrintToggle,*) 'Entering smachAndSndspdModule'
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
!        SoundSpeed = object%snd
        WRITE(PrintToggle,*) 'Leaving smachAndSndspdModule'
        WRITE(PrintToggle,*) 'Entering rmachModule'
       
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

        WRITE(PrintToggle,*) 'Leaving rmachModule'

     else
        WRITE(PrintToggle,*) 'Entering interpModule'
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

        WRITE(PrintToggle,*) 'Leaving interpModule'
      endif

! output the mean flow data.
      
      
      WRITE(PrintToggle,*) 'Entering machoutModule'
      CALL machout(npts  = object%numberOfRadialPoints,  &
                   rr    = object%r,   &
                   rmch  = object%rmx, &
                   rmchp = object%drm, &
                   rmsw  = object%rmt, &
                   rmswp = object%drt, &
                   snd   = object%snd, &
                   dsn   = object%dsn, &
                   rhob  = object%rho)
      WRITE(PrintToggle,*) 'Leaving machoutModule'
    ! Set up global matrices.
    
      WRITE(PrintToggle,*) 'Entering globalModule'
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
      WRITE(PrintToggle,*) 'Leaving globalModule'

      WRITE(PrintToggle,*) 'Entering boundaryModule'
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
      WRITE(PrintToggle,*) 'Leaving boundaryModule'

     object%aa_before = object%aa
     object%bb_before = object%bb

      WRITE(PrintToggle,*) 'Entering analysisModule'          
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
      WRITE(PrintToggle,*) 'Leaving analysisModule'          

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

      WRITE(PrintToggle,*) 'Entering outputModule' 
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
      WRITE(PrintToggle,*) 'Leaving outputModule' 
!
  END SUBROUTINE CreateSwirlClassObject

  SUBROUTINE FindResidualVector(object    ,& 
                                axialWavenumber ,& 
                                vRPertubationData,&
                                vThPertubationData,&
                                vXPertubationData,&
                                pPertubationData,&
                                vRResidual,&
                                vThResidual,&
                                vXResidual,&
                                pResidual       ,&
                                S            )
  
  TYPE(SwirlClassType)                             ,INTENT(INOUT) :: object
  
!  INTEGER                                          ,INTENT(INOUT) :: modeNumber

  REAL(KIND=REAL64),DIMENSION(object%numberOfRadialPoints)  :: vRPertubationData,&
                                                               vThPertubationData,&
                                                               vXPertubationData ,&
                                                               pPertubationData 

  REAL(KIND=REAL64),DIMENSION(object%numberOfRadialPoints)  :: vRResidual,&
                                                                  vThResidual,&
                                                                  vXResidual ,&
                                                                  pResidual 
  COMPLEX(KIND=REAL64),DIMENSION(object%numberOfRadialPoints*4)  :: Xmatrix
  COMPLEX(KIND=REAL64) , INTENT(IN) :: axialWavenumber
  COMPLEX(KIND=REAL64),DIMENSION(object%numberOfRadialPoints*4),INTENT(OUT) :: S

  INTEGER :: i                                                                            

    DO i =1,object%numberOfRadialPoints
    Xmatrix(i) = vRPertubationData(i)
    Xmatrix(object%numberOfRadialPoints+i) = vThPertubationData(i)
    Xmatrix(2*object%numberOfRadialPoints+i) = vXPertubationData(i)
    Xmatrix(3*object%numberOfRadialPoints+i) = pPertubationData(i)
    ENDDO

    S = MATMUL(object%aa_before,Xmatrix) + CMPLX(0.0_rDef,1.0_rDef)*axialWavenumber*MATMUL(object%bb_before,Xmatrix)

    DO i =1,object%numberOfRadialPoints
    vXResidual(i) = S(i)
    vThResidual(i) = S(i+ object%numberOfRadialPoints)
    vXResidual(i) = S(i+ 2*object%numberOfRadialPoints)
    pResidual(i) = S(i+ 3*object%numberOfRadialPoints)
    ENDDO

  END SUBROUTINE FindResidualVector
  SUBROUTINE GetRadialModeData(object         ,&
                               modeNumber     ,&
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
    WRITE(6,*) 'Cannot provide radial mode data, no object is provided'
  ENDIF

  END SUBROUTINE GetRadialModeData 
  SUBROUTINE GetMeanData(object   ,&
                         axialMach,&
                         thetaMach)
  
  TYPE(SwirlClassType), INTENT(INOUT) :: object
  REAL(KIND=rDef),DIMENSION(object%numberOfRadialPoints), INTENT(OUT) :: &
  axialMach,&
  thetaMach
  
  axialMach = object%rmx 
  thetaMach = object%rmt
  END SUBROUTINE
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
                 object%S_MMS_Array, &
                 object%vl,    &
                 object%vr)
     
  END SUBROUTINE DestroySwirlClassObject  
END MODULE swirlClassObj
