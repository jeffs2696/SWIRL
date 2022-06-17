modulE swirlClassObject
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
    USE outputModule
    USE FindResidualVectorModule
    USE SourceTermModule
    USE mmsClassObject
    IMPLICIT NONE

    PRIVATE
    PUBLIC ::&
        runSwirlClassMethods, &
        CreateMMSObject , &
        CreateObject   ,&
        DestroyObject  ,&
        SwirlClassType ,&
        GetMeanFlowData, &
        FindResidualData, &
        GetModeData

! Creates a derived data type containing SWIRL's essential modules
    INTERFACE CreateObject
        MODULE PROCEDURE CreateSwirlClassObject
    END INTERFACE CreateObject

    INTERFACE runSwirlClassMethods
        MODULE PROCEDURE runSWIRL
    END INTERFACE runSwirlClassMethods

    INTERFACE CreateMMSObject
        MODULE PROCEDURE CreateSwirlClassObjectMMS
    END INTERFACE CreateMMSObject

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

    INTERFACE GetSortedModeData
        MODULE PROCEDURE SortModeData
    END INTERFACE GetSortedModeData

    INTERFACE DestroyObject
        MODULE PROCEDURE DestroySwirlClassObject
    END INTERFACE DestroyObject

    INTEGER, PARAMETER:: rDef = REAL64

    TYPE SwirlClassType
        PRIVATE  ! prevents user from accessing any of the following variables

        LOGICAL :: &
            isInitialized = .FALSE.         ! flag to identify if object exists

        INTEGER :: &
            azimuthalMode            ,&    ! m, Circumfirential mode number
            numberOfRadialPoints     ,&    ! npts, number of radial mesh points
            numberOfPropagatingModes, &    ! number of modes that the user wants
            FiniteDifferenceFlag     !,&    ! Use central FD for derivatives, ! 1 = 2nd Order, 2 = 4th Order PrintToggle                    ! Turns on print to screen

        REAL(KIND = REAL64) :: &
            hubTipRatio        ,&
            dr                 , &
            kappa              , & !ratio of speCific heats
            secondOrderSmoother, &!derivative "smoothers"
            fourthOrderSmoother  !"          "

        REAL(KIND = REAL64), DIMENSION(:), ALLOCATABLE :: &
            soundSpeed_L2array , &
            vRad   , &
            vT   , &
            vX   , &
            Pr   , &
            y,     &! used to map grid on a-1 to 1 scale
            rwork, &! needed for ZGGEV
            r,     &! radial locations
            rmx,   &! axial mach number
            drm,   &! derivative of the axial mach number
            rmt,   &! theta mach number
            drt,   &! derivative of the theta mach number
            snd,   &! speed of sound
            SoundSpeedExpected , &
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
            aa, bb,S_aa,S_bb

        !These are needed before trivial elements are removed
        COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE ::&
            aa_before, bb_before

        COMPLEX(KIND = REAL64), DIMENSION(:), ALLOCATABLE ::&
            alpha, &
            beta,  &
            work,  &
            vph,   &
            S_MMS ,&
            S_Expected , &
            S_actual , &
            S_1   ,&
            S_2   ,&
            S_3   ,&
            S_4   ,&
            eigenVectorMMS , &
            wvn

        COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE :: &
            vl, &
            vr

    END TYPE SwirlClassType

! Local Variable Declaration

    LOGICAL :: debug = .FALSE. ! turns on/off printing to console

    ! CHARACTER :: &
    !     jobvl = 'N' ,& ! needed for zggev
    !     jobvr = 'V'    !

! ! additional variables, implicitly defined in the original code

    ! INTEGER ::&
    !     ir      = 2,  & ! axial mach number "knob" to switch between analytical solutions
    !     is      = 5     ! tangential mach number

CONTAINS

    SUBROUTINE CreateSwirlClassObject(&
        object          , &
        azimuthalMode   , &
        np              , &
        sig             , &
        axialMachData   , &
        tangentialMachData   , &
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
            axialMachData, &
            tangentialMachData

! Local variables
        INTEGER ::&
            np4

        REAL(KIND = REAL64) :: &
            ed2 ,   &
            ed4

        COMPLEX(KIND = REAL64), INTENT(IN) ::&
            etah, etad, ak

        object%isInitialized = .TRUE.

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
        object%rmx                  = axialMachData
        object%rmt                  = tangentialMachData
        np4                         = object%numberOfRadialPoints*4

        object%dr =  &
            (1.0_rDef-object%hubTipRatio)/REAL(object%numberOfRadialPoints - 1, rDef)

        ALLOCATE(&
            object%eigenVectorMMS(object%numberOfRadialPoints*4) , &
            object%S_actual(object%numberOfRadialPoints*4) , &
            object%S_Expected(object%numberOfRadialPoints*4) , &
            object%S_1(object%numberOfRadialPoints) , &
            object%S_2(object%numberOfRadialPoints) , &
            object%S_3(object%numberOfRadialPoints) , &
            object%S_4(object%numberOfRadialPoints) , &
            object%snd(object%numberOfRadialPoints),&
            object%SoundSpeedExpected(object%numberOfRadialPoints) , &
            object%soundSpeed_L2array(object%numberOfRadialPoints) , &
            object%vRad(object%numberOfRadialPoints),&
            object%vT(object%numberOfRadialPoints),&
            object%vX(object%numberOfRadialPoints),&
            object%Pr(object%numberOfRadialPoints))

        ALLOCATE(&
            object%dl1(object%numberOfRadialPoints,object%numberOfRadialPoints),   &
            object%y(object%numberOfRadialPoints),        &
            object%rwork(8*np4), &
            object%r(object%numberOfRadialPoints),        &
            object%drm(object%numberOfRadialPoints),      &
            object%drt(object%numberOfRadialPoints),      &
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
            object%S_aa(np4,np4),  &
            object%aa_before(np4,np4),  &
            object%bb(np4,np4),  &
            object%S_bb(np4,np4),  &
            object%bb_before(np4,np4),  &
            object%vl(np4,np4),  &
            object%vr(np4,np4))

    END SUBROUTINE CreateSwirlClassObject
    SUBROUTINE runSWIRL(&
        object)

        TYPE(SwirlClassType) , INTENT(INOUT) :: &
            object

        IF (object%isInitialized) THEN
            ! Set up Gauss-Lobatto grid and compute Chebyshev derivative matrix.
            ! if (is .ne. -1) then

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
                    WRITE(0,*) 'Entering fdgrid CALL'
                ELSE
                ENDIF


                ! Finite Difference grid
                CALL fdgrid(&
                    np  = object%numberOfRadialPoints,  &
                    sig = object%hubTipRatio, &
                    x   = object%y,   &
                    r   = object%r)

                IF (debug) THEN
                    WRITE(0,*) 'Leaving fdgrid CALL'
                ELSE
                ENDIF

                IF (debug) THEN
                    WRITE(0,*) 'Entering fdrivs CALL'
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
                    WRITE(0,*) 'Leaving fdrivs CALL'
                ELSE
                ENDIF


            ENDIF

            IF (debug) THEN
                WRITE(0,*) 'Entering smachAndSndspd CALL'
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
                WRITE(0,*) 'Leaving smachAndSndspd CALL'
            ELSE
            ENDIF

            IF (debug) THEN
                WRITE(0,*) 'Entering rmach CALL'
            ELSE
            ENDIF

            CALL rmach(&
                npts  = object%numberOfRadialPoints,    &
                rmch  = object%rmx,   &
                drm   = object%drm,   &
                dd    = object%dl1    &
                )
            IF (debug) THEN
                WRITE(0,*) 'Leaving rmach CALL'
            ELSE
            ENDIF

            ! Set up global matrices.
            IF (debug) THEN
                WRITE(0,*) 'Entering globalM CALL'
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
                bb   = object%bb,  &
                S_aa   = object%S_aa,  &
                S_bb   = object%S_bb,  &
                row    = 1 ,&
                col    = 4)


            IF (debug) THEN
                WRITE(0,*) 'Leaving globalM CALL'
            ELSE
            ENDIF

            IF (debug) THEN
                WRITE(0,*) 'Entering boundary CALL'
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
                WRITE(0,*) 'Leaving boundary CALL'
            ELSE
            ENDIF

            object%aa_before = object%aa
            object%bb_before = object%bb


            IF (debug) THEN
                WRITE(0,*) 'Entering analysis CALL'
            ELSE
            ENDIF

            ! CALL analysis(&
            !     np    = object%numberOfRadialPoints,    &
            !     np4   = object%numberOfRadialPoints*4,   &
            !     ak    = object%frequency,    &
            !     rr    = object%r,     &
            !     snd   = object%snd,   &
            !     rmx   = object%rmx,   &
            !     rmt   = object%rmt,   &
            !     aa    = object%aa,    &
            !     bb    = object%bb,    &
            !     alpha = object%alpha, &
            !     beta  = object%beta,  &
            !     vl    = object%vl,    &
            !     vr    = object%vr,    &
            !     work  = object%work,  &
            !     rwork = object%rwork, &
            !     gam   = object%wvn,   &
            !     jobvl = jobvl, &
            !     jobvr = jobvr, &
            !     mm    = object%azimuthalMode,    &
            !     ir    = ir,    &
            !     is    = is,    &
            !     vphi  = object%vph,   &
            !     akap  = object%akap)

           IF (debug) THEN
               WRITE(0,*) 'Leaving analysis CALL'
           ELSE
           ENDIF

            ! CALL output(&
            !     np     = object%numberOfRadialPoints,    &
            !     np4    = object%numberOfRadialPoints*4,   &
            !     mode   = object%azimuthalMode,    &
            !     rho    = object%hubTipRatio,   &
            !     omega  = object%frequency,    &
            !     egv    = jobvr, &
            !     attenh = object%hubLinerAdmittance,  &
            !     attend = object%ductLinerAdmittance,  &
            !     rmx    = object%rmx,   &
            !     drm    = object%drm,   &
            !     rmt    = object%rmt,   &
            !     drt    = object%drt,   &
            !     snd    = object%snd,   &
            !     rr     = object%r,     &
            !     wvn    = object%wvn,   &
            !     vrm    = object%vr,    &
            !     vphi   = object%vph,   &
            !     is     = is)

        ELSE
            WRITE(6,*) 'ERROR: The object is not initialized'
            CONTINUE
        ENDIF

    END SUBROUTINE runSWIRL
    SUBROUTINE CreateSwirlClassObjectMMS()!&
!        TYPE(SwirlClassType) :: manufacturedObject
!        TYPE(mmsClassType) :: SoundSpeedMMS_ClassObj, SourceTermMMS_ClassObj
!
!        INTEGER :: &
!            i
!
!
!        REAL(KIND = rDef) :: &
!            ExpectedRateOfConvergenceSoundSpeed , &
!            ExpectedRateOfConvergenceSourceTerm
!
!        REAL(KIND = rDef) ,DIMENSION(:), ALLOCATABLE ::&
!            SoundSpeedExpected , &
!            thetaMachData, &
!            axialMachData
!
!        COMPLEX(KIND = rDef) :: &
!            axialWavenumberMMS
!
!
!        COMPLEX(KIND = rDef) , DIMENSION(:) , ALLOCATABLE :: &
!            S_A, S_B
!
!
!            CALL CreateObject(&
!                object          = manufacturedObject , &
!                azimuthalMode   = azimuthalModeNumber, &
!                np              = numberOfGridPoints , &
!                sig             = hubToTipRatio             , &
!                axialMachData   = axialMachData , &
!                tangentialMachData   = thetaMachData, &
!                ak = frequency, &
!                etah = hubAdmittance            , &
!                etad = ductAdmittance            , &
!                ifdff = finiteDiffFlag           )
!
!            CALL runSwirlClassMethods(object = manufacturedObject)
!
!            IF (manufacturedObject%FiniteDifferenceFlag.eq.1) THEN
!                ExpectedRateOfConvergenceSoundSpeed = 2.0_rDef
!            ELSEIF (manufacturedObject%FiniteDifferenceFlag.eq.2) THEN
!                ExpectedRateOfConvergenceSoundSpeed = 4.0_rDef
!            ENDIF
!
!            IF (manufacturedObject%FiniteDifferenceFlag.eq.1) THEN
!                ExpectedRateOfConvergenceSourceTerm = 2.0_rDef
!            ELSEIF (manufacturedObject%FiniteDifferenceFlag.eq.2) THEN
!                ExpectedRateOfConvergenceSourceTerm = 4.0_rDef
!            ENDIF
!            DO i = 1,manufacturedObject%numberOfRadialPoints
!
!                manufacturedObject%eigenVectorMMS(i) = &
!                    CMPLX(manufacturedObject%vRad(i),KIND = rDef)
!
!                manufacturedObject%eigenVectorMMS(i +   manufacturedObject%numberOfRadialPoints) = &
!                    CMPLX(manufacturedObject%vT(i), KIND = rDef)
!
!                manufacturedObject%eigenVectorMMS(i + 2*manufacturedObject%numberOfRadialPoints) = &
!                    CMPLX(manufacturedObject%vX(i), KIND = rDef)
!
!                manufacturedObject%eigenVectorMMS(i + 3*manufacturedObject%numberOfRadialPoints) = &
!                    CMPLX(manufacturedObject%Pr(i), KIND = rDef)
!
!                manufacturedObject%S_Expected(i)  =&
!                    manufacturedObject%S_1(i)
!
!                manufacturedObject%S_Expected(i + manufacturedObject%numberOfRadialPoints)  =&
!                    manufacturedObject%S_2(i)
!
!                manufacturedObject%S_Expected(i + 2*manufacturedObject%numberOfRadialPoints)  =&
!                    manufacturedObject%S_3(i)
!
!                manufacturedObject%S_Expected(i + 3*manufacturedObject%numberOfRadialPoints)  =&
!                    manufacturedObject%S_4(i)
!
!            ENDDO
!
!
!            axialWavenumberMMS = CMPLX(100.0_rDef,0.00_rDef,KIND=rDef) ! the value Is arbitrary, up to user
!
!            S_A = MATMUL(manufacturedObject%aa_before,manufacturedObject%eigenVectorMMS)
!            S_B = MATMUL(manufacturedObject%bb_before,manufacturedObject%eigenVectorMMS)
!
!            manufacturedObject%S_actual = S_A - (-CMPLX(0.0, 1.0, KIND=rDef)*axialWavenumberMMS)*S_B
!
!
!            CALL getL2Norm(&
!                object    = SoundSpeedMMS_ClassObj ,&
!                L2        = SoundSpeedErrorL2      ,&
!                dataSet1  = manufacturedObject%SoundSpeedExpected     ,&
!                dataSet2  = manufacturedObject%snd           )
!
!
!            CALL getL2Norm(&
!                object    = SourceTermMMS_ClassObj,&
!                L2        = S_L2 ,&
!                dataSet1  = manufacturedObject%S_actual,&
!                dataSet2  = manufacturedObject%S_Expected)
!
!            CALL DestroyObject(object = manufacturedObject)
!            DEALLOCATE(SoundSpeedExpected,thetaMachData,axialMachData)
!
    END SUBROUTINE CreateSwirlClassObjectMMS
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
        eigenVector             ,&
        eigenValue              ,&
        S)

        TYPE(SwirlClassType), INTENT(IN) ::&
            object

        COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4), INTENT(IN) :: &
            eigenVector

        COMPLEX(KIND = rDef), INTENT(IN) :: &
            eigenValue

        COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4), INTENT(INOUT) :: &
            S
        ! Local variables
        INTEGER :: np


        COMPLEX(KIND = rDef),  DIMENSION(object%numberOfRadialPoints*4) :: &
            S_A, &
            S_B


        np = object%numberOfRadialPoints

        S_A =  MATMUL(object%aa_before,eigenVector)
        S_B =  MATMUL(object%bb_before,eigenVector)
        ! S_A =  MATMUL(object%S_aa,eigenVector)
        ! S_B =  MATMUL(object%S_bb,age vector)

        S = S_A - eigenValue*S_B


    END SUBROUTINE GetResidualVector
    SUBROUTINE SortModeData()

    END SUBROUTINE SortModeData
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
            object%SoundSpeedExpected , &
            object%soundSpeed_L2array , &
            object%dsn,   &
            object%rho,   &
            object%akap,  &
            object%alpha, &
            object%beta,  &
            object%work,  &
            object%vph,   &
            object%wvn,   &
            object%aa,    &
            object%S_aa,    &
            object%aa_before,    &
            object%bb,    &
            object%S_bb,    &
            object%bb_before,    &
            object%vl,    &
            object%vr,    &
            object%S_MMS , &
            object%S_actual , &
            object%S_Expected , &
            object%S_1,&
            object%S_2,&
            object%S_3,&
            object%S_4,&
            object%vRad,&
            object%vT,&
            object%vX,&
            object%Pr,&
            object%eigenVectorMMS)

    END SUBROUTINE DestroySwirlClassObject
!
END MODULE swirlClassObject
