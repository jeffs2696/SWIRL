MODULE OrderOfAccuracyClass
  
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: OrderOfAccuracyType,     &
            ObjectIsInitialized,     &
            CreateObject,            &
            DestroyObject,           &
            CalculateOrderOfAccuracy

! module error data

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE OrderOfAccuracyClass: '

! define floating point precision

  INTEGER, PARAMETER :: rDef = REAL64

include 'OrderOfAccuracyClassDef.f90'

INTERFACE ObjectIsInitialized
  MODULE PROCEDURE ObjectIsInitialized1
END INTERFACE ObjectIsInitialized

INTERFACE CreateObject
  MODULE PROCEDURE CreateOrderOfAccuracyObject
END INTERFACE CreateObject

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyOrderOfAccuracyObject
END INTERFACE DestroyObject

INTERFACE CalculateOrderOfAccuracy
  MODULE PROCEDURE CalculateOrderOfAccuracy1
END INTERFACE CalculateOrderOfAccuracy

CONTAINS

LOGICAL FUNCTION ObjectIsInitialized1(object)
  TYPE(OrderOfAccuracyType), INTENT(INOUT) :: object

  ObjectIsInitialized1 = object%isInitialized

  RETURN
END FUNCTION ObjectIsInitialized1

SUBROUTINE CreateOrderOfAccuracyObject(object,               &
                                       enableChecking,       &
                                       errorInfoObject)

  TYPE(OrderOfAccuracyType), INTENT(INOUT) :: object

  LOGICAL, INTENT(IN) :: enableChecking

  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateOrderOfAccuracyObject'

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    charStringObject%charString = 'OrderOfAccuracyObject is already initialized.'
    GO TO 100
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

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

END SUBROUTINE CreateOrderOfAccuracyObject

SUBROUTINE DestroyOrderOfAccuracyObject(object,                   &
                                        enableChecking,           &
                                        errorInfoObject)

  TYPE(OrderOfAccuracyType), INTENT(INOUT) :: object

  LOGICAL, INTENT(IN) :: enableChecking

  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyOrderOfAccuracyObject'

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'OrderOfAccuracyObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  object%isInitialized = .FALSE.

  RETURN
 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE DestroyOrderOfAccuracyObject

SUBROUTINE CalculateOrderOfAccuracy1(object,                    &
                                     iMin,                      &
                                     iMax,                      &
                                     errorMagnitude,            &
                                     gridSpacing,               &
                                     calculatedOrderOfAccuracy, &
                                     enableChecking,            &
                                     errorInfoObject)

  TYPE(OrderOfAccuracyType), INTENT(INOUT) :: object

  INTEGER, INTENT(IN) :: iMin,iMax

  REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: errorMagnitude, &
                                               gridSpacing

  REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: calculatedOrderOfAccuracy

  LOGICAL, INTENT(IN) :: enableChecking

  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: iStart,i

  REAL(KIND=rDef) :: dXFac,eFac

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CalculateOrderOfAccuracy1'

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'OrderOfAccuracyObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF
   IF (SIZE(errorMagnitude) < 2) THEN
    charStringObject%charString = &
     'Need at least two errorMagnitudes to calculate orderOfAccuracy.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
   IF (SIZE(errorMagnitude) /= SIZE(gridSpacing)) THEN
    charStringObject%charString = &
     'ERROR: errorMagnitude and gridSpacing are not the same length.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
   IF (SIZE(errorMagnitude) /= SIZE(calculatedOrderOfAccuracy)) THEN
    charStringObject%charString = &
     'ERROR: errorMagnitude and calculatedOrderOfAccuracy are not the same length.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
   IF (iMin < 1) THEN
    charStringObject%charString = &
     'Error: iMin < 1.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
   IF (iMax > SIZE(errorMagnitude)) THEN
    charStringObject%charString = &
     'Error: iMax is greater than the errorMagnitude array length.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
   IF (iMax < iMin) THEN
    charStringObject%charString = &
     'Error: iMax < iMin.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE ! feeling lucky!
  END IF

! Order of accuracy is calculated from coarser to finer:
!
! e(i-1) = A (dX_{i-1})^{p}
! e(i) = A (dX_{i})^{p}
!
! e(i)/e(i-1) = (dX_{i}/dX_{i-1})^p
!
! p = ln(e(i)/e(i-1))/ln(dX_{i}/dX_{i-1})
!  
  IF (iMin == 1) THEN
   iStart = 2
   calculatedOrderOfAccuracy(iMin) = 0.0_rDef
  ELSE
   iStart = iMin
  END IF

  DO i=iStart,iMax
   eFac  = errorMagnitude(i)/errorMagnitude(i-1)
   dXFac = gridSpacing(i)/gridSpacing(i-1)
   calculatedOrderOfAccuracy(i) = LOG(eFac)/LOG(dXFac)
  END DO

  RETURN
 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CalculateOrderOfAccuracy1

END MODULE OrderOfAccuracyClass

