MODULE SourceTermModule

    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: &
        getMMSSourceTerms ,&
        getSoundSpeed     ,&
        getPerturbationVariables

    INTERFACE getMMSSourceTerms
        MODULE PROCEDURE SourceCalc
    END INTERFACE getMMSSourceTerms

    INTERFACE getSoundSpeed
        MODULE PROCEDURE CalcSoundSpeed
    END INTERFACE getSoundSpeed 

    INTERFACE getPerturbationVariables
        MODULE PROCEDURE CalcPerturbationVariables
    END INTERFACE getPerturbationVariables 


    INTEGER,PARAMETER :: rDef = REAL64

CONTAINS

    include 'SourceTermMMS.f90'

    include 'SoundSpeedMMS.f90'

    include 'CalcPerturbationVariablesMMS.f90'

END MODULE SourceTermModule
