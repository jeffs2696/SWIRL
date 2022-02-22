MODULE CheckSorting 

  USE QuickSortInt4, ONLY: int4Kind
  USE QuickSortInt8, ONLY: int8Kind
  USE QuickSortReal4, ONLY: real4Kind
  USE QuickSortReal8, ONLY: real8Kind
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: IsSorted,           &
            LocationListIsCorrect

INTERFACE IsSorted
  MODULE PROCEDURE Int4ArrayIsSorted
  MODULE PROCEDURE Int8ArrayIsSorted
  MODULE PROCEDURE Real4ArrayIsSorted
  MODULE PROCEDURE Real8ArrayIsSorted
END INTERFACE IsSorted

INTERFACE LocationListIsCorrect
  MODULE PROCEDURE Int4ArrayLocationListIsCorrect
  MODULE PROCEDURE Int8ArrayLocationListIsCorrect
  MODULE PROCEDURE Real4ArrayLocationListIsCorrect
  MODULE PROCEDURE Real8ArrayLocationListIsCorrect
END INTERFACE LocationListIsCorrect

REAL(KIND=real4Kind), PARAMETER :: real4Tol = 1.0e-6_real4Kind
REAL(KIND=real8Kind), PARAMETER :: real8Tol = 1.0e-12_real8Kind

CONTAINS

LOGICAL FUNCTION Real4ArrayIsSorted(array)
  REAL(KIND=real4Kind), DIMENSION(:), INTENT(IN) :: array
  INTEGER :: nPts,i

  Real4ArrayIsSorted = .TRUE.
  nPts = SIZE(array,1)
  DO i=2,nPts
   IF (array(i) < array(i-1)) THEN
    Real4ArrayIsSorted = .FALSE.
    RETURN
   END IF
  END DO
  RETURN
END FUNCTION Real4ArrayIsSorted

LOGICAL FUNCTION Real4ArrayLocationListIsCorrect(arrayOrig,arraySorted,locationList)
  REAL(KIND=real4Kind), DIMENSION(:), INTENT(IN) :: arrayOrig,arraySorted
  INTEGER, DIMENSION(:), INTENT(IN) :: locationList
  INTEGER :: nPts,i
  REAL(KIND=real4Kind) :: err

  Real4ArrayLocationListIsCorrect = .TRUE.
  nPts = SIZE(arrayOrig,1)
  DO i=1,nPts
   err = ABS(arraySorted(locationList(i))-arrayOrig(i))
   IF (err > real4Tol) THEN
    Real4ArrayLocationListIsCorrect = .FALSE.
    RETURN
   END IF
  END DO
  RETURN
END FUNCTION Real4ArrayLocationListIsCorrect

LOGICAL FUNCTION Real8ArrayIsSorted(array)
  REAL(KIND=real8Kind), DIMENSION(:), INTENT(IN) :: array
  INTEGER :: nPts,i

  Real8ArrayIsSorted = .TRUE.
  nPts = SIZE(array,1)
  DO i=2,nPts
   IF (array(i) < array(i-1)) THEN
    Real8ArrayIsSorted = .FALSE.
    RETURN
   END IF
  END DO
  RETURN
END FUNCTION Real8ArrayIsSorted

LOGICAL FUNCTION Real8ArrayLocationListIsCorrect(arrayOrig,arraySorted,locationList)
  REAL(KIND=real8Kind), DIMENSION(:), INTENT(IN) :: arrayOrig,arraySorted
  INTEGER, DIMENSION(:), INTENT(IN) :: locationList
  INTEGER :: nPts,i
  REAL(KIND=real8Kind) :: err

  Real8ArrayLocationListIsCorrect = .TRUE.
  nPts = SIZE(arrayOrig,1)
  DO i=1,nPts
   err = ABS(arraySorted(locationList(i))-arrayOrig(i))
   IF (err > real8Tol) THEN
    Real8ArrayLocationListIsCorrect = .FALSE.
    RETURN
   END IF
  END DO
  RETURN
END FUNCTION Real8ArrayLocationListIsCorrect

LOGICAL FUNCTION Int4ArrayIsSorted(array)
  INTEGER(KIND=int4Kind), DIMENSION(:), INTENT(IN) :: array
  INTEGER :: nPts,i

  Int4ArrayIsSorted = .TRUE.
  nPts = SIZE(array,1)
  DO i=2,nPts
   IF (array(i) < array(i-1)) THEN
    Int4ArrayIsSorted = .FALSE.
    RETURN
   END IF
  END DO
  RETURN
END FUNCTION Int4ArrayIsSorted

LOGICAL FUNCTION Int4ArrayLocationListIsCorrect(arrayOrig,arraySorted,locationList)
  INTEGER(KIND=int4Kind), DIMENSION(:), INTENT(IN) :: arrayOrig,arraySorted
  INTEGER, DIMENSION(:), INTENT(IN) :: locationList
  INTEGER :: nPts,i
  INTEGER(KIND=int4Kind) :: err

  Int4ArrayLocationListIsCorrect = .TRUE.
  nPts = SIZE(arrayOrig,1)
  DO i=1,nPts
   err = ABS(arraySorted(locationList(i))-arrayOrig(i))
   IF (err > 0_int4Kind) THEN
    Int4ArrayLocationListIsCorrect = .FALSE.
    RETURN
   END IF
  END DO
  RETURN
END FUNCTION Int4ArrayLocationListIsCorrect

LOGICAL FUNCTION Int8ArrayIsSorted(array)
  INTEGER(KIND=int8Kind), DIMENSION(:), INTENT(IN) :: array
  INTEGER :: nPts,i

  Int8ArrayIsSorted = .TRUE.
  nPts = SIZE(array,1)
  DO i=2,nPts
   IF (array(i) < array(i-1)) THEN
    Int8ArrayIsSorted = .FALSE.
    RETURN
   END IF
  END DO
  RETURN
END FUNCTION Int8ArrayIsSorted

LOGICAL FUNCTION Int8ArrayLocationListIsCorrect(arrayOrig,arraySorted,locationList)
  INTEGER(KIND=int8Kind), DIMENSION(:), INTENT(IN) :: arrayOrig,arraySorted
  INTEGER, DIMENSION(:), INTENT(IN) :: locationList
  INTEGER :: nPts,i
  INTEGER(KIND=int8Kind) :: err

  Int8ArrayLocationListIsCorrect = .TRUE.
  nPts = SIZE(arrayOrig,1)
  DO i=1,nPts
   err = ABS(arraySorted(locationList(i))-arrayOrig(i))
   IF (err > 0_int8Kind) THEN
    Int8ArrayLocationListIsCorrect = .FALSE.
    RETURN
   END IF
  END DO
  RETURN
END FUNCTION Int8ArrayLocationListIsCorrect

END MODULE CheckSorting
