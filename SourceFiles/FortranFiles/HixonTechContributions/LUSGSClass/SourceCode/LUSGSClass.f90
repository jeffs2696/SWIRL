MODULE LUSGSClass

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: LUSGSType,           &
            CreateObject,        &
            DestroyObject,       &
            ObjectIsInitialized, &
            PerformLUSGSUpdate

! module error data

  TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE LUSGSClass: '

! define floating point precision

  INTEGER, PARAMETER :: rDef = REAL64

include 'LUSGSClassDef.f90'

INTERFACE CreateObject
  MODULE PROCEDURE CreateLUSGSObject
END INTERFACE CreateObject

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyLUSGSObject
END INTERFACE DestroyObject

INTERFACE ObjectIsInitialized
  MODULE PROCEDURE LUSGSObjectIsInitialized
END INTERFACE ObjectIsInitialized

INTERFACE PerformLUSGSUpdate
  MODULE PROCEDURE LUSGSInviscid4DE4D
  MODULE PROCEDURE LUSGSInviscid3DE3D
  MODULE PROCEDURE LUSGSInviscid2DE2D
  MODULE PROCEDURE LUSGSInviscid1DE1D
  MODULE PROCEDURE LUSGSViscous4DE4D
  MODULE PROCEDURE LUSGSViscous3DE3D
  MODULE PROCEDURE LUSGSViscous2DE2D
  MODULE PROCEDURE LUSGSViscous1DE1D
END INTERFACE PerformLUSGSUpdate

CONTAINS

SUBROUTINE CreateLUSGSObject(object,                        &
                             numberOfTopologicalDimensions, &
                             enableChecking,                &
                             errorInfoObject)

  TYPE(LUSGSType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: numberOfTopologicalDimensions
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateLUSGSObject'

  CONTINUE ! execution begins here

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    charStringObject%charString = 'ERROR: object is already initialized.'
    GO TO 100
   ELSE IF (numberOfTopologicalDimensions > 4) THEN
    WRITE(charStringObject%charString,'(a,i5)') &
       'ERROR: max number of topological dimensions is 4 -- found ',numberOfTopologicalDimensions
    GO TO 100
   ELSE IF (numberOfTopologicalDimensions < 1) THEN
    WRITE(charStringObject%charString,'(a,i5)') &
       'ERROR: min number of topological dimensions is 1 -- found ',numberOfTopologicalDimensions
    GO TO 100
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE ! feeling lucky
  END IF

  object%numberOfTopologicalDimensions = numberOfTopologicalDimensions
  object%initialSweepDirection = 1
  object%isInitialized = .TRUE.

  RETURN

100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  GO TO 101
101 CONTINUE
  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE CreateLUSGSObject

SUBROUTINE DestroyLUSGSObject(object,                        &
                              enableChecking,                &
                              errorInfoObject)

  TYPE(LUSGSType), INTENT(INOUT) :: object
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyLUSGSObject'

  CONTINUE ! execution begins here

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'ERROR: object is not initialized.'
    GO TO 100
   END IF
  ELSE
   CONTINUE ! feeling lucky
  END IF

  object%numberOfTopologicalDimensions = -1
  object%initialSweepDirection = -1
  object%isInitialized = .FALSE.

  RETURN

100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  GO TO 101
101 CONTINUE
  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE DestroyLUSGSObject

LOGICAL FUNCTION LUSGSObjectIsInitialized(object)
  TYPE(LUSGSType), INTENT(IN) :: object
  
  LUSGSObjectIsInitialized = object%isInitialized

  RETURN
END FUNCTION LUSGSObjectIsInitialized

include 'LUSGSClassRoutinesInviscid4D.f90'
include 'LUSGSClassRoutinesInviscid3D.f90'
include 'LUSGSClassRoutinesInviscid2D.f90'
include 'LUSGSClassRoutinesInviscid1D.f90'

include 'LUSGSClassRoutinesViscous4D.f90'
include 'LUSGSClassRoutinesViscous3D.f90'
include 'LUSGSClassRoutinesViscous2D.f90'
include 'LUSGSClassRoutinesViscous1D.f90'

END MODULE LUSGSClass
