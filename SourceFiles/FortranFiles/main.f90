PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject               ! Runs SWIRL for a given set of parameters
    USE mmsClassObject                 ! Calculates L2Norm, L2Max, and Rate Of Convergence
    USE SourceTermModule               ! Calculated the symbolic terms needed for MMS (used Python)

    IMPLICIT NONE

    ! Defining the variables  
    INTEGER, PARAMETER :: &
        rDef = REAL64   

    INTEGER  :: &
        finiteDiffFlag           ,& ! finite difference flag
        numericalIntegrationFlag ,& !  numerical integration flag
        numberOfGridPoints       ,& ! number of points
        azimuthalModeNumber      ,& ! mode order
        i                        ,& ! indexer for do loops
        fac                      ,& ! variable used for doubling grid points
        facCount                    ! counts the outermost do loop

    COMPLEX(KIND = rDef) :: &
        ci                          ! imaginary number, sqrt(-1)                                 

    REAL(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        r                   ,& !radial grid locations
        axialMachData       ,& !M_x
        thetaMachData       ,& !M_th
        totalMachData          !M_total = sqrt(M_x^2+M_th^2)

    ! Perturbation variables
    REAL(KIND = REAL64) ::  &
        secondOrderSmoother ,& !2nd order smoothing coefficient
        fourthOrderSmoother ,& !4th order smoothing coefficient
        dr                  ,&
        hubToTipRatio      ! ,& SoundSpeedErrorL2

! input variables:
    REAL(KIND = rDef), PARAMETER ::&
        r_min  = 0.30_rDef  ,&
        r_max  = 1.000_rDef  

    INTEGER, PARAMETER :: &
        numberOfIterations = 4

    COMPLEX(KIND=rDef), PARAMETER :: &
        hubAdmittance   = CMPLX(0.0,0,rDef)   , &
        ductAdmittance  = CMPLX(0.3,0.0,rDef) , &
        frequency       = CMPLX(30,0,rDef)

    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: &
        swirlClassObj
! Code Starts Here!
    CONTINUE

    ! local variables 
    ! inputs needed for SwirlClassType
    azimuthalModeNumber       =  1
    hubToTipRatio             =  r_min/r_max
    numericalIntegrationFlag  =  1
    finiteDiffFlag            =  1
    secondOrderSmoother       =  0.0_rDef
    fourthOrderSmoother       =  0.0_rDef
    ! constants needed for calculations

    ci  = CMPLX(0.0, 1.0, rDef)  !imaginary number

    facCount = 0 ! initializer for fac count
    
    DO fac = 1, numberOfIterations
        facCount             = facCount + 1
        numberOfGridPoints   = 5+(2**fac)
        dr                   = (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)

        ALLOCATE(&
            r(numberOfGridPoints)                                            , &
            thetaMachData(numberOfGridPoints)                                , &
            axialMachData(numberOfGridPoints)                                , &
            totalMachData(numberOfGridPoints))                                

        DO i = 1, numberOfGridPoints

            r(i)             = (r_min+REAL(i-1, rDef)*dr)/r_max
            axialMachData(i) = 0.2*COS(0.2*r(i))
            thetaMachData(i) = 0.2*COS(0.2*r(i))

        END DO

        !Create a swirlClassObj for a given flow
        CALL CreateObject(&
            object        = swirlClassObj(fac)  ,&
            radius        = r                    ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            axialMachData = axialMachData        ,&
            tangentialMachData = thetaMachData        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       )
        
        CALL runSwirlClassMethods(&
            object = swirlClassObj(fac))

        CALL DestroyObject(object = swirlClassObj(fac))

        DEALLOCATE(&
            r                     ,&
            thetaMachData         ,&
            axialMachData         ,&
            totalMachData         )
    END DO

END PROGRAM MAIN
