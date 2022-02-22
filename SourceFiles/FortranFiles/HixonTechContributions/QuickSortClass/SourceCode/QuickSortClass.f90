MODULE QuickSortClass

  USE ErrorInformationClass
  USE QuickSortInt4
  USE QuickSortInt8
  USE QuickSortReal4
  USE QuickSortReal8

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: QuickSortType, &
            CreateObject,  &
            DestroyObject, &
            SortArrayData

INTERFACE CreateObject
  MODULE PROCEDURE CreateQuickSortClassObject
END INTERFACE CreateObject

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyQuickSortClassObject
END INTERFACE DestroyObject

INTERFACE SortArrayData
  MODULE PROCEDURE SortArrayDataInt4WithLL
  MODULE PROCEDURE SortArrayDataInt4WOutLL
  MODULE PROCEDURE SortArrayDataInt8WithLL
  MODULE PROCEDURE SortArrayDataInt8WOutLL
  MODULE PROCEDURE SortArrayDataReal4WithLL
  MODULE PROCEDURE SortArrayDataReal4WOutLL
  MODULE PROCEDURE SortArrayDataReal8WithLL
  MODULE PROCEDURE SortArrayDataReal8WOutLL
END INTERFACE SortArrayData

TYPE QuickSortType
  PRIVATE
  LOGICAL :: isInitialized = .FALSE.
END TYPE QuickSortType

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE QuickSortClass: '

CONTAINS

SUBROUTINE CreateQuickSortClassObject(object,         &
                                      enableChecking, &
                                      errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateQuickSortClassObject'

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    charStringObject%charString = 'QuickSortClassObject is already initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  object%isInitialized = .TRUE.

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CreateQuickSortClassObject

SUBROUTINE DestroyQuickSortClassObject(object,         &
                                      enableChecking, &
                                      errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyQuickSortClassObject'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  object%isInitialized = .FALSE.

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE DestroyQuickSortClassObject

SUBROUTINE SortArrayDataInt4WithLL(object,         &
                                   arrayData,      &
                                   locationList,   &
                                   enableChecking, &
                                   errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  INTEGER(KIND=int4Kind), DIMENSION(:), INTENT(INOUT) :: arrayData
  INTEGER, DIMENSION(:), INTENT(OUT) :: locationList
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(SIZE(arrayData,1)) :: order

  INTEGER :: i,nPts

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SortArrayDataInt4WithLL'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  CALL quick_sort(list  = arrayData,  &
                  order = order)

  nPts = SIZE(arrayData,1)

  DO i=1,nPts
   locationList(order(i)) = i
  END DO

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SortArrayDataInt4WithLL

SUBROUTINE SortArrayDataInt4WOutLL(object,         &
                                   arrayData,      &
                                   enableChecking, &
                                   errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  INTEGER(KIND=int4Kind), DIMENSION(:), INTENT(INOUT) :: arrayData
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(SIZE(arrayData,1)) :: order

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SortArrayDataInt4WOutLL'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  CALL quick_sort(list  = arrayData,  &
                  order = order)

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SortArrayDataInt4WOutLL

SUBROUTINE SortArrayDataInt8WithLL(object,         &
                                   arrayData,      &
                                   locationList,   &
                                   enableChecking, &
                                   errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  INTEGER(KIND=int8Kind), DIMENSION(:), INTENT(INOUT) :: arrayData
  INTEGER, DIMENSION(:), INTENT(OUT) :: locationList
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(SIZE(arrayData,1)) :: order

  INTEGER :: i,nPts

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SortArrayDataInt8WithLL'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  CALL quick_sort(list  = arrayData,  &
                  order = order)

  nPts = SIZE(arrayData,1)

  DO i=1,nPts
   locationList(order(i)) = i
  END DO

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SortArrayDataInt8WithLL

SUBROUTINE SortArrayDataInt8WOutLL(object,         &
                                   arrayData,      &
                                   enableChecking, &
                                   errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  INTEGER(KIND=int8Kind), DIMENSION(:), INTENT(INOUT) :: arrayData
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(SIZE(arrayData,1)) :: order

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SortArrayDataInt8WOutLL'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  CALL quick_sort(list  = arrayData,  &
                  order = order)

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SortArrayDataInt8WOutLL

SUBROUTINE SortArrayDataReal4WithLL(object,         &
                                    arrayData,      &
                                    locationList,   &
                                    enableChecking, &
                                    errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  REAL(KIND=real4Kind), DIMENSION(:), INTENT(INOUT) :: arrayData
  INTEGER, DIMENSION(:), INTENT(OUT) :: locationList
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(SIZE(arrayData,1)) :: order

  INTEGER :: i,nPts

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SortArrayDataReal4WithLL'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  CALL quick_sort(list  = arrayData,  &
                  order = order)

  nPts = SIZE(arrayData,1)

  DO i=1,nPts
   locationList(order(i)) = i
  END DO

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SortArrayDataReal4WithLL

SUBROUTINE SortArrayDataReal4WOutLL(object,         &
                                    arrayData,      &
                                    enableChecking, &
                                    errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  REAL(KIND=real4Kind), DIMENSION(:), INTENT(INOUT) :: arrayData
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(SIZE(arrayData,1)) :: order

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SortArrayDataReal4WOutLL'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  CALL quick_sort(list  = arrayData,  &
                  order = order)

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SortArrayDataReal4WOutLL

SUBROUTINE SortArrayDataReal8WithLL(object,         &
                                    arrayData,      &
                                    locationList,   &
                                    enableChecking, &
                                    errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  REAL(KIND=real8Kind), DIMENSION(:), INTENT(INOUT) :: arrayData
  INTEGER, DIMENSION(:), INTENT(OUT) :: locationList
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(SIZE(arrayData,1)) :: order

  INTEGER :: i,nPts

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SortArrayDataReal8WithLL'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  CALL quick_sort(list  = arrayData,  &
                  order = order)

  nPts = SIZE(arrayData,1)

  DO i=1,nPts
   locationList(order(i)) = i
  END DO

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SortArrayDataReal8WithLL

SUBROUTINE SortArrayDataReal8WOutLL(object,         &
                                    arrayData,      &
                                    enableChecking, &
                                    errorInfoObject)

  TYPE(QuickSortType), INTENT(INOUT) :: object
  REAL(KIND=real8Kind), DIMENSION(:), INTENT(INOUT) :: arrayData
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(SIZE(arrayData,1)) :: order

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SortArrayDataReal8WOutLL'

  IF (enableChecking) THEN
   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'QuickSortClassObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE
  END IF 

  CALL quick_sort(list  = arrayData,  &
                  order = order)

  RETURN
101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SortArrayDataReal8WOutLL

END MODULE QuickSortClass
