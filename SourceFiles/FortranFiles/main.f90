PROGRAM MAIN
    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject               ! Runs SWIRL for a given set of parameters
    USE mmsClassObject                 ! Calculates L2Norm, L2Max, and Rate Of Convergence
    USE SourceTermModule               ! Calculated the symbolic terms needed for MMS (used Python)

    IMPLICIT NONE

    ! Defining the variables  
    include '/main-scripts/main-variables.f90'

! Code Starts Here!
    CONTINUE

    include '/main-scripts/debug-script.f90'
    ! local variables 
    include '/main-scripts/main-local-variables.f90'
    
    DO fac = 1, numberOfIterations
        include '/main-scripts/iterative-loop.f90'
    END DO
    ! Construct spars A and B to check each term

    include '/main-scripts/calculating-rate-of-convergence.f90'

    ! Should I put the data writing in another script? : JS
    include '/main-scripts/swirl-data-export-MMS.f90'

    DEALLOCATE( &
        SoundSpeedL2Array ,&
        S_L2Array ,&
        RateOfConvergence1 ,&
        RateOfConvergence2)

END PROGRAM MAIN
