MODULE OrderOfAccuracyClass
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: OrderOfAccuracyType,     &
            ObjectIsInitialized,     &
            CreateObject,            &
            DestroyObject,           &
            CalculateOrderOfAccuracy

! module error data


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

SUBROUTINE CreateOrderOfAccuracyObject(object)

  TYPE(OrderOfAccuracyType), INTENT(INOUT) :: object



! local variables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateOrderOfAccuracyObject'

  object%isInitialized = .TRUE.

  RETURN

END SUBROUTINE CreateOrderOfAccuracyObject

SUBROUTINE DestroyOrderOfAccuracyObject(object)

  TYPE(OrderOfAccuracyType), INTENT(INOUT) :: object


! local variables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyOrderOfAccuracyObject'

   IF (object%isInitialized) THEN
    CONTINUE
   ELSE

   END IF
  object%isInitialized = .FALSE.

  RETURN

END SUBROUTINE DestroyOrderOfAccuracyObject

SUBROUTINE CalculateOrderOfAccuracy1(object,                    &
                                     iMin,                      &
                                     iMax,                      &
                                     errorMagnitude,            &
    gridSpacing,               &
    calculatedOrderOfAccuracy )
                                     

  TYPE(OrderOfAccuracyType), INTENT(INOUT) :: object

  INTEGER, INTENT(IN) :: iMin,iMax

  REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: errorMagnitude, &
                                               gridSpacing

  REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: calculatedOrderOfAccuracy



! local variables

  INTEGER :: iStart,i

  REAL(KIND=rDef) :: dXFac,eFac

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CalculateOrderOfAccuracy1'

   IF (object%isInitialized) THEN
    CONTINUE
   ELSE

   END IF
   IF (SIZE(errorMagnitude) < 2) THEN

   IF (SIZE(errorMagnitude) /= SIZE(gridSpacing)) THEN

   ELSE
    CONTINUE
   END IF
   IF (SIZE(errorMagnitude) /= SIZE(calculatedOrderOfAccuracy)) THEN

   ELSE
    CONTINUE
   END IF
   IF (iMin < 1) THEN

   ELSE
    CONTINUE
   END IF
   IF (iMax > SIZE(errorMagnitude)) THEN

   ELSE
    CONTINUE
   END IF
   IF (iMax < iMin) THEN

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
  RETURN

END SUBROUTINE CalculateOrderOfAccuracy1

END MODULE OrderOfAccuracyClass

