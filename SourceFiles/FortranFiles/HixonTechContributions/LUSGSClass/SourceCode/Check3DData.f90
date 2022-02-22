MODULE Check3DData

  USE ErrorInformationClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CheckDataValue
  
INTERFACE CheckDataValue
  MODULE PROCEDURE Check3DInt4DataValue
  MODULE PROCEDURE Check3DInt8DataValue
  MODULE PROCEDURE Check3DCharDataValue
  MODULE PROCEDURE Check3DLogicalDataValue
  MODULE PROCEDURE Check3DRealDataValue
  MODULE PROCEDURE Check3DDblePrecDataValue
END INTERFACE CheckDataValue

REAL, PARAMETER :: tolReal = 5.0e-6_4

DOUBLE PRECISION, PARAMETER :: tolDblePrec = 5.0e-12_8

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE Check3DData: '

CONTAINS

SUBROUTINE Check3DInt4DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=4), DIMENSION(:,:,:), INTENT(IN) :: dataValue, &
                                                   expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(3) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check3DInt4DataValue'

  DO n=1,3
   arrayLength1(n) = SIZE(dataValue,n)

   arrayLength2(n) = SIZE(expectedDataValue,n)

   IF (arrayLength1(n) /= arrayLength2(n)) THEN

    WRITE(charStringObject%charString,'(1x,a42,i1,a7,i5,a7,i5,a2 )')                        &
                                             'Array lengths are different in dimension ',n, &
                                             ': (dV: ',                                     &
                                             arrayLength1(n),                               &
                                             '; dVE: ',                                     &
                                             arrayLength2(n),                               &
                                             ').'

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    charStringObject%charString = moduleLocation
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    charStringObject%charString = 'Error in: '//location
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    RETURN
   END IF
   errorLocation(n) = -1
  END DO

  correctValue = .TRUE.

  DO k=1,arrayLength1(3)
   DO j=1,arrayLength1(2)
    DO i=1,arrayLength1(1)
     IF (dataValue(i,j,k) /= expectedDataValue(i,j,k)) THEN
      correctValue = .FALSE.
      errorLocation(1) = i   
      errorLocation(2) = j   
      errorLocation(3) = k   
      RETURN
     END IF
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check3DInt4DataValue

SUBROUTINE Check3DInt8DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=8), DIMENSION(:,:,:), INTENT(IN) :: dataValue, &
                                                   expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(3) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check3DInt8DataValue'

  DO n=1,3
   arrayLength1(n) = SIZE(dataValue,n)

   arrayLength2(n) = SIZE(expectedDataValue,n)

   IF (arrayLength1(n) /= arrayLength2(n)) THEN

    WRITE(charStringObject%charString,'(1x,a42,i1,a7,i5,a7,i5,a2 )')                        &
                                             'Array lengths are different in dimension ',n, &
                                             ': (dV: ',                                     &
                                             arrayLength1(n),                               &
                                             '; dVE: ',                                     &
                                             arrayLength2(n),                               &
                                             ').'

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    charStringObject%charString = moduleLocation
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    charStringObject%charString = 'Error in: '//location
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    RETURN
   END IF
   errorLocation(n) = -1
  END DO

  correctValue = .TRUE.

  DO k=1,arrayLength1(3)
   DO j=1,arrayLength1(2)
    DO i=1,arrayLength1(1)
     IF (dataValue(i,j,k) /= expectedDataValue(i,j,k)) THEN
      correctValue = .FALSE.
      errorLocation(1) = i   
      errorLocation(2) = j   
      errorLocation(3) = k   
      RETURN
     END IF
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check3DInt8DataValue
   
SUBROUTINE Check3DCharDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  CHARACTER, DIMENSION(:,:,:), INTENT(IN) :: dataValue, &
                                         expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(3) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check3DCharDataValue'

  DO n=1,3
   arrayLength1(n) = SIZE(dataValue,n)

   arrayLength2(n) = SIZE(expectedDataValue,n)

   IF (arrayLength1(n) /= arrayLength2(n)) THEN

    WRITE(charStringObject%charString,'(1x,a42,i1,a7,i5,a7,i5,a2 )')                        &
                                             'Array lengths are different in dimension ',n, &
                                             ': (dV: ',                                     &
                                             arrayLength1(n),                               &
                                             '; dVE: ',                                     &
                                             arrayLength2(n),                               &
                                             ').'

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    charStringObject%charString = moduleLocation
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    charStringObject%charString = 'Error in: '//location
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    RETURN
   END IF
   errorLocation(n) = -1
  END DO

  correctValue = .TRUE.

  DO k=1,arrayLength1(3)
   DO j=1,arrayLength1(2)
    DO i=1,arrayLength1(1)
     IF (dataValue(i,j,k) /= expectedDataValue(i,j,k)) THEN
      correctValue = .FALSE.
      errorLocation(1) = i   
      errorLocation(2) = j   
      errorLocation(3) = k   
      RETURN
     END IF
    END DO
   END DO
  END DO

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check3DCharDataValue
   
SUBROUTINE Check3DRealDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  REAL, DIMENSION(:,:,:), INTENT(IN) :: dataValue, &
                                          expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL :: den,num,scaledTol

  INTEGER, DIMENSION(3) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check3DRealDataValue'

  DO n=1,3
   arrayLength1(n) = SIZE(dataValue,n)

   arrayLength2(n) = SIZE(expectedDataValue,n)

   IF (arrayLength1(n) /= arrayLength2(n)) THEN

    WRITE(charStringObject%charString,'(1x,a42,i1,a7,i5,a7,i5,a2 )')                        &
                                             'Array lengths are different in dimension ',n, &
                                             ': (dV: ',                                     &
                                             arrayLength1(n),                               &
                                             '; dVE: ',                                     &
                                             arrayLength2(n),                               &
                                             ').'

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    charStringObject%charString = moduleLocation
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    charStringObject%charString = 'Error in: '//location
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    RETURN
   END IF
   errorLocation(n) = -1
  END DO

  correctValue = .TRUE.

  DO k=1,arrayLength1(3)
   DO j=1,arrayLength1(2)
    DO i=1,arrayLength1(1)

     num = ABS((dataValue(i,j,k)) - (expectedDataValue(i,j,k)))

! if num = 0, these are equal by definition.

     IF (num == 0.0) THEN
      CONTINUE
     ELSE 

      den = ABS(dataValue(i,j,k)) + ABS(expectedDataValue(i,j,k))
 
      scaledTol = tolReal*den

      IF (num >= scaledTol) THEN
       correctValue = .FALSE.
       errorLocation(1) = i   
       errorLocation(2) = j   
       errorLocation(3) = k   
       RETURN
      END IF
     END IF
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check3DRealDataValue

SUBROUTINE Check3DDblePrecDataValue(dataValue,         &
                                    expectedDataValue, &
                                    correctValue,      &
                                    errorLocation,     &
                                    errorInfoObject)

  DOUBLE PRECISION, DIMENSION(:,:,:), INTENT(IN) :: dataValue, &
                                                     expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  DOUBLE PRECISION :: den,num

  INTEGER, DIMENSION(3) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k

  DOUBLE PRECISION, DIMENSION(SIZE(dataValue,1), &
                              SIZE(dataValue,2), &
                              SIZE(dataValue,3)) :: errAbsValue

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check3DDblePrecDataValue'

  DO n=1,3
   arrayLength1(n) = SIZE(dataValue,n)

   arrayLength2(n) = SIZE(expectedDataValue,n)

   IF (arrayLength1(n) /= arrayLength2(n)) THEN

    WRITE(charStringObject%charString,'(1x,a42,i1,a7,i5,a7,i5,a2 )')                        &
                                             'Array lengths are different in dimension ',n, &
                                             ': (dV: ',                                     &
                                             arrayLength1(n),                               &
                                             '; dVE: ',                                     &
                                             arrayLength2(n),                               &
                                             ').'

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    charStringObject%charString = moduleLocation
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    charStringObject%charString = 'Error in: '//location
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    RETURN
   END IF
   errorLocation(n) = -1
  END DO

  correctValue = .TRUE.

  DO k=1,arrayLength1(3)
   DO j=1,arrayLength1(2)
    DO i=1,arrayLength1(1)

     num = ABS((dataValue(i,j,k)) - (expectedDataValue(i,j,k)))

! if num = 0, these are equal by definition.

     den = ABS(dataValue(i,j,k)) + ABS(expectedDataValue(i,j,k))
     errAbsValue(i,j,k) = (num - tolDblePrec*den)

!    IF (num == 0.0d0) THEN
!     CONTINUE
!    ELSE ! continue

!     den = ABS(dataValue(i,j,k)) + ABS(expectedDataValue(i,j,k))

!     scaledTol = tolDblePrec*den

!     IF (num >= scaledTol) THEN
!      correctValue = .FALSE.
!      errorLocation(1) = i   
!      errorLocation(2) = j   
!      errorLocation(3) = k   
!      RETURN
!     END IF
!    END IF
    END DO
   END DO
  END DO

  IF (MAXVAL(errAbsValue) > 0.0_8) THEN
   correctValue = .FALSE.
   errorLocation(1:3) = MAXLOC(errAbsValue)
  END IF

  RETURN
END SUBROUTINE Check3DDblePrecDataValue

SUBROUTINE Check3DLogicalDataValue(dataValue,         &
                                   expectedDataValue, &
                                   correctValue,      &
                                   errorLocation,     &
                                   errorInfoObject)

  LOGICAL, DIMENSION(:,:,:), INTENT(IN) :: dataValue, &
                                             expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(3) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check3DLogicalDataValue'

  DO n=1,3
   arrayLength1(n) = SIZE(dataValue,n)

   arrayLength2(n) = SIZE(expectedDataValue,n)

   IF (arrayLength1(n) /= arrayLength2(n)) THEN

    WRITE(charStringObject%charString,'(1x,a42,i1,a7,i5,a7,i5,a2 )')                        &
                                             'Array lengths are different in dimension ',n, &
                                             ': (dV: ',                                     &
                                             arrayLength1(n),                               &
                                             '; dVE: ',                                     &
                                             arrayLength2(n),                               &
                                             ').'

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    charStringObject%charString = moduleLocation
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    charStringObject%charString = 'Error in: '//location
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    RETURN
   END IF
   errorLocation(n) = -1
  END DO

  correctValue = .TRUE.

  DO k=1,arrayLength1(3)
   DO j=1,arrayLength1(2)
    DO i=1,arrayLength1(1)

     IF (dataValue(i,j,k) .NEQV. expectedDataValue(i,j,k)) THEN
      correctValue = .FALSE.
      errorLocation(1) = i   
      errorLocation(2) = j   
      errorLocation(3) = k   
      RETURN
     END IF
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check3DLogicalDataValue
   
END MODULE Check3DData
