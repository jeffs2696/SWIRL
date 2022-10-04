MODULE zeroCrossingModule

    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: zeroCrossing

    INTERFACE zeroCrossing
        MODULE PROCEDURE zeroCrossingProcedure 
    END INTERFACE zeroCrossing

    INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

        SUBROUTINE zeroCrossingProcedure()
        END SUBROUTINE zeroCrossingProcedure
    
END MODULE zeroCrossingModule
