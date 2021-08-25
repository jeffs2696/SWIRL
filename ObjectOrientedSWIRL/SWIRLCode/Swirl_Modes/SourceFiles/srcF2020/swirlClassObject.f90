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

        !These are needed before trivial elements are removed
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
            vl, &
            vr


    END TYPE SwirlClassType

! Local Variable Declaration

    LOGICAL :: debug = .FALSE. ! turns on/off printing to console
    CHARACTER :: &
        jobvl = 'N' ,& ! needed for zggev
        jobvr = 'V'    !

! additional variables, implicitly defined in the original code

    INTEGER ::&
        ir      = 2,  & ! axial mach number "knob" to switch between analytical solutions
        is      = 5,  & ! tangential mach number
        PrintToggle = 0 ! set this variable to 6 to see progress in terminal

! JS: added i and j to zero out matricies

    ! INTEGER :: i , j
    REAL(KIND = REAL64) ::&
        slope  = 0.00_rDef

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
            ThetaMachData

! Local variables            
        INTEGER ::&
            np4

        REAL(KIND = REAL64) :: &
            ed2 ,   &
            ed4

        COMPLEX(KIND = REAL64), INTENT(IN) ::&
            etah, etad, ak

        object%isInitialized = .TRUE.

        IF (object%isInitialized) THEN
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
            np4                         = object%numberOfRadialPoints*4


! NEW: allocate data arrays
!

            ALLOCATE(&
                object%dl1(object%numberOfRadialPoints,object%numberOfRadialPoints),   &
                object%y(object%numberOfRadialPoints),        &
                object%rwork(8*np4), &
                object%r(object%numberOfRadialPoints),        &
                object%drm(object%numberOfRadialPoints),      &
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
                object%aa_before(np,np),  &
                object%bb(np4,np4),  &
                object%bb_before(np,np),  &
                object%vl(np4,np4),  &
                object%vr(np4,np4))


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

                    IF (debug) THEN
                        WRITE(PrintToggle,*) 'Entering fdgrid CALL'
                    ELSE
                    ENDIF


                    ! Finite Difference grid
                    CALL fdgrid(&
                        np  = object%numberOfRadialPoints,  &
                        sig = object%hubTipRatio, &
                        x   = object%y,   &
                        r   = object%r)

                    IF (debug) THEN
                        WRITE(PrintToggle,*) 'Leaving fdgrid CALL'
                    ELSE
                    ENDIF

                    IF (debug) THEN
                        WRITE(PrintToggle,*) 'Entering fdrivs CALL'
                    ELSE
                    ENDIF


                    CALL fdrivs(&
                        np     = object%numberOfRadialPoints,    &
                        sig    = object%hubTipRatio,   &
                        dl1    = object%dl1,   &
                        iorder = object%FiniteDifferenceFlag, &
                        ed2    = object%secondOrderSmoother,   &
                        ed4    = object%fourthOrderSmoother)
                    IF (debug) THEN
                        WRITE(PrintToggle,*) 'Leaving fdrivs CALL'
                    ELSE
                    ENDIF


                endif

                IF (debug) THEN
                    WRITE(PrintToggle,*) 'Entering smachAndSndspd CALL'
                ELSE
                ENDIF

                CALL smachAndSndspd(&
                    npts  = object%numberOfRadialPoints,    &
                    rr    = object%r,     &
                    rmsw  = object%rmt,   &
                    rmswp = object%drt,   &
                    snd   = object%snd,   &
                    dsn   = object%dsn,   &
                    dd    = object%dl1   )

                IF (debug) THEN
                    WRITE(PrintToggle,*) 'Leaving smachAndSndspd CALL'
                ELSE
                ENDIF

                IF (debug) THEN
                    WRITE(PrintToggle,*) 'Entering rmach CALL'
                ELSE
                ENDIF

                CALL rmach(&
                    npts  = object%numberOfRadialPoints,    &
                    rmch  = object%rmx,   &
                    drm   = object%drm,   &
                    dd    = object%dl1    &
                    )
                IF (debug) THEN
                    WRITE(PrintToggle,*) 'Leaving rmach CALL'
                ELSE
                ENDIF

            else
                ! JS: Future release will allow interpolation for mean flow
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

            ! Set up global matrices.
            IF (debug) THEN
                WRITE(PrintToggle,*) 'Entering globalM CALL'
            ELSE
            ENDIF

            CALL globalM(&
                np   = object%numberOfRadialPoints,  &
                np4  = object%numberOfRadialPoints*4, &
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

            IF (debug) THEN
                WRITE(PrintToggle,*) 'Leaving globalM CALL'
            ELSE
            ENDIF

            IF (debug) THEN
                WRITE(PrintToggle,*) 'Entering boundary CALL'
            ELSE
            ENDIF

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

            IF (debug) THEN
                WRITE(PrintToggle,*) 'Leaving boundary CALL'
            ELSE
            ENDIF

            ! this is added to save A and B matricies before they are altered by the analysis CALL
            object%aa_before = object%aa
            object%bb_before = object%bb

            ! WRITE(6,*) SIZE(object%aa_before,1), SIZE(object%aa_before,2)

            IF (debug) THEN
                WRITE(PrintToggle,*) 'Entering analysis CALL'
            ELSE
            ENDIF

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

            IF (debug) THEN
                WRITE(PrintToggle,*) 'Leaving analysis CALL'
            ELSE
            ENDIF


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

        ! The data we originally sent in
        axialMach     = object%rmx
        axialMach_dr  = object%drm
        thetaMach     = object%rmt
        thetaMach_dr  = object%drt
        SoundSpeed    = object%snd
        SoundSpeed_dr = object%dsn
        radialData    = object%r

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

        ! INTEGER :: i

        eigenValue = object%wvn(eigenIndex)
        eigenVector= object%vr(:,eigenIndex)

    END SUBROUTINE GetRadialModeData 

    SUBROUTINE GetResidualVector(&
        object                  ,&
            S)

        TYPE(SwirlClassType), INTENT(IN) ::&
            object

        ! COMPLEX(KIND = rDef), INTENT(IN) :: lambda
        COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4), INTENT(INOUT) :: &
            S
        ! COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4) ,INTENT(IN) :: x

        ! Local variables 
        INTEGER :: np , i, j

        COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4,object%numberOfRadialPoints*4) :: &
            S_A,S_B,S_S


        WRITE(6,*) 'Shape',SIZE(S)
        np = object%numberOfRadialPoints

        ! WRITE(6,*) SHAPE(object%vr,KIND=rDef)
        S_A = MATMUL(object%aa,object%vr)
        S_B = MATMUL(object%bb,object%vr) 

        WRITE(6,*) SIZE(S_A,1) , SIZE(S_A,2)
        DO i = 1,np*4
            DO j = 1,np*4
                S_S(i,j)   = S_A(i,j) - object%wvn(j)*S_B(i,j)  
                WRITE(6,*) S_S(i,j)
            ENDDO
        ENDDO


        ! S = S_A - S_B


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
