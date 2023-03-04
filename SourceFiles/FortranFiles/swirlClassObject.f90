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
    USE L2NormModule
    USE trapezoidalRuleModule
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
        CutOnResiduialCheck ,&
        GetModeData     , &
        GetAnalyiticModeShape,&
        NormalizeModeShape , &
        CompareModeShapes
        


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
        ! MODULE PROCEDURE GetResidualVector2
    END INTERFACE FindResidualData

    INTERFACE CutOnResiduialCheck
        MODULE PROCEDURE CheckFirstRadialModeEigenResidual
    END INTERFACE CutOnResiduialCheck

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

    INTERFACE GetAnalyiticModeShape
        MODULE PROCEDURE GetModeShape 
    END INTERFACE GetAnalyiticModeShape

    INTERFACE NormalizeModeShape
        MODULE PROCEDURE NormModeShape
    END INTERFACE NormalizeModeShape

    INTERFACE CompareModeShapes
        MODULE PROCEDURE CompareModeShape
    END INTERFACE CompareModeShapes

    INTERFACE ComputeAnalyticAxialWavenumber
        MODULE PROCEDURE ComputeAnalyticAxialWavenumber1
    END INTERFACE ComputeAnalyticAxialWavenumber

    INTEGER, PARAMETER:: rDef = REAL64

    TYPE SwirlClassType
        PRIVATE  ! prevents user from accessing any of the following variables

        CHARACTER(LEN = 500) :: &
            test_name_id , &
            grid_number_id, &
            finite_difference_id ,& 
            hub_to_tip_ratio_id


        LOGICAL :: &
            isInitialized = .FALSE.         ! flag to identify if object exists

        INTEGER :: &
            azimuthalMode            ,&    ! m, Circumfirential mode number
            numberOfRadialPoints     ,&    ! npts, number of radial mesh points
            numberOfRadialModes, &    ! number of modes that the user wants
            FiniteDifferenceFlag     !,&    ! Use central FD for derivatives, ! 1 = 2nd Order, 2 = 4th Order PrintToggle                    ! Turns on print to screen

        INTEGER, DIMENSION(:), ALLOCATABLE :: &
            mode_index,radial_mode_number_array

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
            numerical_wavenumber_array, &
            wvn

        COMPLEX(KIND = REAL64), DIMENSION(:,:), ALLOCATABLE :: &
            vl, &
            vr

        COMPLEX(KIND=rDef), DIMENSION(:,:), ALLOCATABLE :: &
            analytic_mode_array , numerical_mode_array

!         LOGICAL, :: &
!             debugFlag

    END TYPE SwirlClassType

! Local Variable Declaration

    ! LOGICAL :: debug = .FALSE. ! turns on/off printing to console

    CHARACTER :: &
        jobvl = 'N' ,& ! needed for zggev
        jobvr = 'V'    !

! additional variables, implicitly defined in the original code

    INTEGER ::&
        ir      = 2,  & ! axial mach number "knob" to switch between analytical solutions
        is      = 5     ! tangential mach number

CONTAINS

    SUBROUTINE CreateSwirlClassObject(&
        object              , &
        azimuthalMode       , &
        numberOfRadialModes , &
        np                  , &
        sig                 , &
        axialMachData       , &
        tangentialMachData  , &
        ak                  , &
        etah                , &
        etad                , &
        ifdff               , &
        secondOrderSmoother , &
        fourthOrderSmoother , &
        debugFlag, &
        test_name)

        TYPE(SwirlClassType), INTENT(INOUT) ::&
            object

        INTEGER, INTENT(INOUT) :: &
            ifdff               , &
            azimuthalMode       , &
            np
        INTEGER,INTENT(IN) :: numberOfRadialModes



        REAL(KIND = REAL64), INTENT(IN) :: &
            sig

        REAL(KIND = REAL64), INTENT(IN) :: &
            secondOrderSmoother , &
            fourthOrderSmoother

        REAL(KIND = REAL64), DIMENSION(:), INTENT(INOUT) :: &
            axialMachData, &
            tangentialMachData

        LOGICAL, INTENT(IN) :: &
            debugFlag

! Local variables
        INTEGER ::&
            np4

        REAL(KIND = REAL64) :: &
            ed2 ,   &
            ed4

        COMPLEX(KIND = REAL64), INTENT(IN) ::&
            etah, etad, ak

        CHARACTER(LEN=*), INTENT(IN) :: &
            test_name



        object%numberOfRadialModes = numberOfRadialModes 

        object%isInitialized = .TRUE.
        IF (debugFlag) THEN 
            WRITE(0,*) 'swirlClassObject isInitialized:', object%isInitialized
        ENDIF

        ! Set user input to the object 'properties';
        ed2                         = secondOrderSmoother
        ed4                         = fourthOrderSmoother
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

        WRITE(object%grid_number_id,*) object%numberOfRadialPoints
        WRITE(object%finite_difference_id,*) object%FiniteDifferenceFlag
        IF (object%hubTipRatio .gt. 0.0_rDef) THEN 
            WRITE(object%hub_to_tip_ratio_id,*) 'a_'
        ELSE
            WRITE(object%hub_to_tip_ratio_id,*) 'c_'
        ENDIF

        
        
        ! (object%hubTipRatio) .gt. 0.0_rDef THEN

        object%test_name_id = &
            TRIM(&
            TRIM(&
            ADJUSTL(TRIM(test_name)) //&
            '_npts'//&
            ADJUSTL(TRIM(object%grid_number_id))) //& 
            '_fd' //&
            ADJUSTL(TRIM(object%finite_difference_id))) //&
            '_domain_' //&
            ADJUSTL(TRIM(object%hub_to_tip_ratio_id))


        ! WRITE(0,*) object%test_name_id 


        ALLOCATE(&
            object%mode_index(object%numberOfRadialModes*2) , &
            object%radial_mode_number_array(object%numberOfRadialModes*2))
        ALLOCATE(&
            object%analytic_mode_array(object%numberOfRadialPoints,object%numberOfRadialModes), &
            object%numerical_mode_array(object%numberOfRadialPoints,object%numberOfRadialModes*2))


        ALLOCATE(&
            object%eigenVectorMMS(object%numberOfRadialPoints*4) , &
            object%numerical_wavenumber_array(object%numberOfRadialModes*2) , &
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
            OBJECt%Pr(object%numberOfRadialPoints))

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
        object         ,&
        debugFlag      ,&
        MMSflag)

        TYPE(SwirlClassType) , INTENT(INOUT) :: &
            object

        !total debug flag
        LOGICAL, INTENT(IN) :: &
            debugFlag, &
            MMSflag
        !eigensolver debug flag
        LOGICAL :: &
            debug_analysis = .FALSE.

         
        

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

                IF (debugFlag) THEN
                    WRITE(0,*) 'Entering fdgrid CALL'
                ELSE
                ENDIF


                ! Finite Difference grid
                CALL fdgrid(&
                    np  = object%numberOfRadialPoints,  &
                    sig = object%hubTipRatio, &
                    x   = object%y,   &
                    r   = object%r)

                IF (debugFlag) THEN
                    WRITE(0,*) 'Leaving fdgrid CALL'
                ELSE
                ENDIF

                IF (debugFlag) THEN
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
                IF (debugFlag) THEN
                    WRITE(0,*) 'Leaving fdrivs CALL'
                ELSE
                ENDIF


            ENDIF

            IF (debugFlag) THEN
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

            IF (debugFlag) THEN
                WRITE(0,*) 'Leaving smachAndSndspd CALL'
            ELSE
            ENDIF

            IF (debugFlag) THEN
                WRITE(0,*) 'Entering rmach CALL'
            ELSE
            ENDIF

            CALL rmach(&
                npts  = object%numberOfRadialPoints,    &
                rmch  = object%rmx,   &
                drm   = object%drm,   &
                dd    = object%dl1    &
                )

            IF (debugFlag) THEN
                WRITE(0,*) 'Leaving rmach CALL'
            ELSE
            ENDIF

            ! Set up global matrices.
            IF (debugFlag) THEN
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


            IF (debugFlag) THEN
                WRITE(0,*) 'Leaving globalM CALL'
            ELSE
            ENDIF

            IF (debugFlag) THEN
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

            IF (debugFlag) THEN
                WRITE(0,*) 'Leaving boundary CALL'
            ELSE
            ENDIF

            object%aa_before = object%aa
            object%bb_before = object%bb


            IF (MMSflag .eqv. .FALSE.) THEN
            IF (debugFlag) THEN
                WRITE(0,*) 'Entering analysis CALL'
            ELSE
            ENDIF

            CALL analysis(&
                np    = object%numberOfRadialPoints,    &
                np4   = object%numberOfRadialPoints*4,   &
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
                ! ir    = ir,    &
                ! is    = is,    &
                vphi  = object%vph,   &
                akap  = object%akap , &
                debug = debug_analysis)


           IF (debugFlag) THEN
               WRITE(0,*) 'Leaving analysis CALL'
           ELSE
           ENDIF
           ENDIF
            CALL output(&
                np     = object%numberOfRadialPoints,    &
                np4    = object%numberOfRadialPoints*4,   &
                mode   = object%azimuthalMode,    &
                numberOfRadialModes = object%numberOfRadialModes  , &
                rho    = object%hubTipRatio,   &
                omega  = object%frequency,    &
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
                is     = is        ,   &
                indx_out = object%mode_index , &
                izeros_out = object%radial_mode_number_array , &
                sortedRadialModeShapes = object%numerical_mode_array , & 
                sortedWavenumberArray = object%numerical_wavenumber_array, &
                file_name_string = object%test_name_id)

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
        radialData  , &
        debugFlag)

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

        LOGICAL, INTENT(IN) :: &
            debugFlag


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
    SUBROUTINE CheckFirstRadialModeEigenResidual(&
            object)!, & S_eigcheck_L2)

       TYPE(SwirlClassType), INTENT(IN) ::&
            object

        ! COMPLEX(KIND=rDef) ,DIMENSION(:), INTENT(INOUT) :: S_eigcheck_L2

        INTEGER :: np ,i, UNIT

        COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4) :: &
            S_all_egv, S_sorted_egv

        COMPLEX(KIND = rDef) :: &
            L2

        ! S_all_egv = (MATMUL(object%aa_before,object%vr(:,1))- MATMUL(object%vr(:,1),MATMUL(object%bb_before,object%wvn)))
        ! S_all_egv =MATMUL(object%aa_before,object%vr(:,1))- object%wvn()*MATMUL(object%bb_before,object%vr(:,1) )
       ! WRITE(0,*) S_all_egv   !- MATMUL(object%bb_before,object%vr(:,1)*object%wvn(1)))
       ! WRITE(0,*) SIZE(object%wvn)
       np = object%numberOfRadialPoints
       ! WRITE(0,*)  MATMUL(object%aa_before ,object%vr(:,1)) !-MATMUL((MATMUL(object%bb_before,object%vr)),object%wvn)
       ! WRITE(0,*)  MATMUL(object%aa_before(3*np+1:4*np,3*np+1:4*np) ,object%vr(3*np+1:4*np,3*np+1:4*np)) !-MATMUL((MATMUL(object%bb_before,object%vr)),object%wvn)

       ! WRITE(0,*) SIZE(MATMUL(MATMUL(object%bb_before,object%vr(:,1)),object%wvn(1)))
       ! WRITE(0,*) SIZE(MATMUL(object%bb_before,object%vr(:,1))*object%wvn(1))

       ! WRITE(0,*) SIZE(MATMUL(object%aa_before,object%vr(:,1))) 


           S_sorted_egv = &
               MATMUL(object%aa_before,object%vr(:,object%mode_index(1))) -&
               CMPLX(0.0_rDef,-1.0_rDef,KIND=rDef)*&
               (MATMUL(object%bb_before,object%vr(:,object%mode_index(1)))*object%wvn(object%mode_index(1)))

           S_all_egv = &
               MATMUL(object%aa_before,object%vr(:,1)) -&
               (MATMUL(object%bb_before,object%vr(:,1))*object%wvn(1))
           OPEN(NEWUNIT = UNIT, FILE='S_sorted_egv.dat')
           WRITE(UNIT,*) 'i ', 'S_real ', 'S_imag ', 'egv_real ', 'egv_imag ', 'k_x_real ', 'k_x_imag '
           DO i = 1,SIZE(S_sorted_egv)
           WRITE(UNIT,*) i ,REAL(S_sorted_egv(i)), AIMAG(S_sorted_egv(i)) ,&
               REAL(object%vr(i,object%mode_index(1))), AIMAG(object%vr(i,object%mode_index(1))), &
               REAL(object%wvn(object%mode_index(1))), AIMAG(object%wvn(object%mode_index(1)))
           
           ! WRITE(0,*) S_sorted_egv(i), object%vr(i,321), object%wvn(321) 
           ENDDO
           CLOSE(UNIT)
           OPEN(NEWUNIT = UNIT, FILE='S_sorted_first_egv.dat')
           WRITE(UNIT,*) 'i ', 'S_real ', 'S_imag ', 'egv_real ', 'egv_imag ', 'k_x_real ', 'k_x_imag '
           DO i = 1,SIZE(S_sorted_egv)
           WRITE(UNIT,*) i ,REAL(S_sorted_egv(i)), AIMAG(S_sorted_egv(i)) ,&
               REAL(object%vr(i,(1))), AIMAG(object%vr(i,(1))), &
               REAL(object%wvn((1))), AIMAG(object%wvn((1)))
           
           ! WRITE(0,*) S_sorted_egv(i), object%vr(i,321), object%wvn(321) 
           ENDDO
           CLOSE(UNIT)
           ! S_sorted_egv = MATMUL(object%aa_before,object%vr(:,1)) - (MATMUL(object%bb_before,object%vr(:,1))*object%wvn(1))
       ! DO i = 1,SIZE(S_all_egv)
       ! WRITE(0,*) S_all_egv(i)
       ! ENDDO

!         CALL GetResidualVector(&
!             object                  ,&
!             object%vr(:,3),&
!             object%wvn(3)             ,&
!             S_all_egv)

        ! CALL GetResidualVector(&
        !     object                  ,&
        !     object%vr(:,3),&
        !     object%wvn(3)             ,&
        !     ! object%numerical_mode_array(:,2),&
        !     ! object%numerical_wavenumber_array(2)             ,&
        !     S_sorted_egv)
        
        ! WRITE(0,*) S
        ! CALL getL2Norm(L2,S_all_egv)
        ! WRITE(0,*) 'L2 all egv', L2

        CALL getL2Norm(L2,S_sorted_egv)


        ! OPEN(NEWUNIT = UNIT, FILE='S_sorted.dat')
        ! WRITE(UNIT,*) 'i ', 'S_real ', 'S_imag '
        ! DO i = 1,SIZE(S_sorted_egv)
        ! WRITE(UNIT,*) i ,REAL(S_sorted_egv(i)), AIMAG(S_sorted_egv(i))
        ! WRITE(0,*) S_sorted_egv(i), object%vr(i,1), object%wvn(1) 
        ! ENDDO
        ! CLOSE(UNIT)
        

        WRITE(0,*) 'L2 sorted egv', L2
    END SUBROUTINE CheckFirstRadialModeEigenResidual


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
            object%eigenVectorMMS , &
            object%analytic_mode_array, &
            object%numerical_mode_array, &
            object%numerical_wavenumber_array,&
            object%mode_index , &
            object%radial_mode_number_array ,&
        )

    END SUBROUTINE DestroySwirlClassObject
    SUBROUTINE GetModeShape(&
            object)

        TYPE(SwirlClassType), INTENT(INOUT) ::&
            object

        INTEGER, PARAMETER :: &
          rDef = REAL64

        LOGICAL :: &
            debug_flag = .FALSE.

        CHARACTER(LEN = 500) :: &
            file_id , &
            gridpoint_file_id, &
            filename

        INTEGER :: &
            UNIT                  ,&
            ! numberOfGridPoints    , &
            azimuthal_mode_number ,&
            radial_mode_number    ,&
            i,j
        
        
        REAL(KIND=rDef) :: & 
            r_min, &
            r_max, &
            dr,  &
            weighting_coefficient_A, &
            weighting_coefficient_B, &
            hubTipRatio,&
            convergence_criteria ,& 
            mode_shape

        REAL(KIND=rDef), DIMENSION(:), ALLOCATABLE :: &
            radial_grid

        REAL(KIND=rDef), DIMENSION(:,:), ALLOCATABLE :: &
            analytic_mode_array 

        REAL(KIND=rDef),DIMENSION(2) :: &
            rmode_bessel_function_errors

        REAL(KIND=rDef),DIMENSION(4) :: &
            eigen_bessel_function_errors , &
            anfu_bessel_function_errors 

        REAL(KIND=rDef) :: &
            anrt_convergence_flag 
        
        REAL(KIND=rDef) :: &
            non_dimensional_roots



        ALLOCATE(&
            radial_grid(object%numberOfRadialPoints) , &
            analytic_mode_array(object%numberOfRadialPoints,object%numberOfRadialModes) &
        )



        r_min = object%hubTipRatio!0.045537!0.20_rDef 

        r_max = object%r(object%numberOfRadialPoints)!1.0_rDef      

        azimuthal_mode_number = object%azimuthalMode 
        convergence_criteria = 1.0E-12_rDef
        ! redundant to recreate radial grid
        hubTipRatio = r_min/r_max 

        dr    = (r_max-r_min)/REAL(object%numberOfRadialPoints-1, rDef)

        DO i =1,object%numberOfRadialPoints

        radial_grid(i)  = (r_min+REAL(i-1, rDef)*dr)!radial grid 
        
        ENDDO
        DO j = 1,object%numberOfRadialModes
        radial_mode_number = j
        
        CALL ANRT(&
            azimuthal_mode_number,&
            radial_mode_number,&
            hubTipRatio,&
            convergence_criteria,&
            non_dimensional_roots,&
            anfu_bessel_function_errors,&
            anrt_convergence_flag)

        ! checking ANRT result

        IF (anrt_convergence_flag .gt. 10e-12_rDef) THEN
            WRITE(0,*) 'ERROR: ANRT DID NOT CONVERGE ',anrt_convergence_flag
        ELSE
        ENDIF

        IF (debug_flag.eqv..TRUE.) THEN
            WRITE(0,*) 'k_mn r_max' ,non_dimensional_roots
        ELSE
        ENDIF
        
        ! Obtaining A and B coefficients for radial mode shape

        CALL EIGEN(&
            azimuthal_mode_number, &
            hubTipRatio          , &
            non_dimensional_roots, &
            weighting_coefficient_A                  , &
            weighting_coefficient_B                  , &
            eigen_bessel_function_errors)

        IF (debug_flag.eqv..TRUE.) THEN
            WRITE(0,*) 'A ' , weighting_coefficient_A
            WRITE(0,*) 'B ' , weighting_coefficient_B
            WRITE(0,*) 'k_mn r_max ' , non_dimensional_roots
            WRITE(0,*) 'k_mn ' , non_dimensional_roots/r_max
        ELSE
        ENDIF

        WRITE(file_id, '(i0.2)') radial_mode_number
        WRITE(gridpoint_file_id, '(i0.4)') object%numberOfRadialPoints

        filename = '03-EVanalysis/analytic_radial_mode_data_radial_mode_number_' &
            // TRIM(ADJUSTL(file_id)) // '_np_'  &
            // TRIM(ADJUSTL(gridpoint_file_id)) // '.dat'
        OPEN(NEWUNIT=UNIT,FILE=TRIM(ADJUSTL(filename))) 

        WRITE(UNIT,'(A12, A12)') &
            'radius', &
            'pressure'
         
        
        DO i = 1,object%numberOfRadialPoints

        CALL RMODE(&
        azimuthal_mode_number,&
        non_dimensional_roots*radial_grid(i)/r_max,&
        weighting_coefficient_A,&
        weighting_coefficient_B,&
        mode_shape,&
        rmode_bessel_function_errors)

        analytic_mode_array(i,j) = mode_shape
        object%analytic_mode_array(i,j) = analytic_mode_array(i,j)
        
  
        WRITE(UNIT,"(F16.12,F16.12)") &
            radial_grid(i)/r_max,&
            analytic_mode_array(i,j) 

        IF (debug_flag) THEN


            WRITE(0,*) &
                radial_grid(i)/r_max , &
                analytic_mode_array(i,j)
        ENDIF
        ENDDO

        CLOSE(UNIT)

        OPEN(NEWUNIT=UNIT,FILE='radial_mode_parameters' & 
            // TRIM(ADJUSTL(file_id)) // '_np_'  &
            // TRIM(ADJUSTL(gridpoint_file_id)) // '.dat')
        WRITE(UNIT,*) &
            'azimuthal_mode_number ', &
            'radial_mode_number ', &
            'weighting_factor_A ', &
            'weighting_factor_B ', &
            'non_dimensional_roots ' 
        WRITE(UNIT,*) &
            azimuthal_mode_number, &
            radial_mode_number , &
            weighting_coefficient_A, &
            weighting_coefficient_B, &
            non_dimensional_roots 
        CLOSE(UNIT)

        ENDDO
        
        DEALLOCATE(&
            radial_grid, &
            analytic_mode_array &
        )

    END SUBROUTINE GetModeShape   
    SUBROUTINE NormModeShape(&
            object)

        TYPE(SwirlClassType), INTENT(INOUT) ::&
            object
        INTEGER :: &
            i 
        REAL(KIND = rDef) :: &
            total

        REAL(KIND = rDef) , DIMENSION(:) , ALLOCATABLE :: &
            test_integrand,   &
            test_result

        
        ALLOCATE(&
            test_integrand(object%numberOfRadialPoints), &
            test_result(object%numberOfRadialPoints))



!         DO i = 1,object%numberOfRadialPoints
!             test_integrand = REAL(object%numerical_mode_array(i,1)**2) 
!             ! WRITE(0,*) object%r(i), test_integrand(i)
!         END DO


!         CALL trapezoidalRule(&
!             rr = object%r , &
!             integrand = test_integrand, &
!             total = total )

!         DO i = 1,object%numberOfRadialPoints
             
!             WRITE(0,*) object%r(i), object%analytic_mode_array(i,1), (1/total**2)*object%numerical_mode_array(i,1)

!         END DO

        DEALLOCATE(test_integrand,test_result)
        
        
    END SUBROUTINE NormModeShape

    SUBROUTINE CompareModeShape(&
            object)

        TYPE(SwirlClassType), INTENT(INOUT) ::&
            object
        INTEGER :: &
            i

        ! DO i = 1,object%numberOfRadialPoints
        ! WRITE(0,*) object%analytic_mode_array(i,1), object%numerical_mode_array(i,1), object%numerical_mode_array(i,2)
        ! END DO
    END SUBROUTINE CompareModeShape

    SUBROUTINE ComputeAnalyticAxialWavenumber1( &
        object)

        TYPE(SwirlClassType), INTENT(INOUT) ::&
            object


    END SUBROUTINE ComputeAnalyticAxialWavenumber1
! 
END MODULE swirlClassObject
