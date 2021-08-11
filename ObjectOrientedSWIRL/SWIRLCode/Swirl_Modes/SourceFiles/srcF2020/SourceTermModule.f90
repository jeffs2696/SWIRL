MODULE SourceTermModule

    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: getMMSSourceTerms  

    INTERFACE getMMSSourceTerms
        MODULE PROCEDURE SourceCalc
    END INTERFACE

    INTEGER,PARAMETER :: rDef = REAL64

CONTAINS

    include 'SourceTermMMS.f90'

END MODULE SourceTermModule
