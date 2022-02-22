MODULE Check2DData

  USE ErrorInformationClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CheckDataValue
  
INTERFACE CheckDataValue
  MODULE PROCEDURE Check2DInt4DataValue
  MODULE PROCEDURE Check2DInt8DataValue
  MODULE PROCEDURE Check2DCharDataValue
  MODULE PROCEDURE Check2DLogicalDataValue
  MODULE PROCEDURE Check2DRealDataValue
  MODULE PROCEDURE Check2DDblePrecDataValue
END INTERFACE CheckDataValue

REAL, PARAMETER :: tolReal = 1.0e-6_4

DOUBLE PRECISION, PARAMETER :: tolDblePrec = 1.0e-12_8

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE Check2DData: '

CONTAINS

SUBROUTINE Check2DInt4DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=4), DIMENSION(:,:), INTENT(IN) :: dataValue, &
                                                 expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(2) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check2DInt4DataValue'

  DO n=1,2
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

  DO j=1,arrayLength1(2)
   DO i=1,arrayLength1(1)
    IF (dataValue(i,j) /= expectedDataValue(i,j)) THEN
     correctValue = .FALSE.
     errorLocation(1) = i   
     errorLocation(2) = j   
     RETURN
    END IF
   END DO
  END DO

  RETURN
END SUBROUTINE Check2DInt4DataValue
   
SUBROUTINE Check2DInt8DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=8), DIMENSION(:,:), INTENT(IN) :: dataValue, &
                                                 expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(2) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check2DInt8DataValue'

  DO n=1,2
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

  DO j=1,arrayLength1(2)
   DO i=1,arrayLength1(1)
    IF (dataValue(i,j) /= expectedDataValue(i,j)) THEN
     correctValue = .FALSE.
     errorLocation(1) = i   
     errorLocation(2) = j   
     RETURN
    END IF
   END DO
  END DO

  RETURN
END SUBROUTINE Check2DInt8DataValue
   
SUBROUTINE Check2DCharDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  CHARACTER, DIMENSION(:,:), INTENT(IN) :: dataValue, &
                                         expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(2) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check2DCharDataValue'

  DO n=1,2
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

  DO j=1,arrayLength1(2)
   DO i=1,arrayLength1(1)
    IF (dataValue(i,j) /= expectedDataValue(i,j)) THEN
     correctValue = .FALSE.
     errorLocation(1) = i   
     errorLocation(2) = j   
     RETURN
    END IF
   END DO
  END DO

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check2DCharDataValue
   
SUBROUTINE Check2DRealDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  REAL, DIMENSION(:,:), INTENT(IN) :: dataValue, &
                                          expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL :: den,num,scaledTol

  INTEGER, DIMENSION(2) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check2DRealDataValue'

  DO n=1,2
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

  DO j=1,arrayLength1(2)
   DO i=1,arrayLength1(1)

    num = ABS((dataValue(i,j)) - (expectedDataValue(i,j)))

! if num = 0, these are equal by definition.

    IF (num == 0.0) THEN
     CONTINUE
    ELSE 

     den = ABS(dataValue(i,j)) + ABS(expectedDataValue(i,j))
 
     scaledTol = tolReal*den

     IF (num >= scaledTol) THEN
      correctValue = .FALSE.
      errorLocation(1) = i   
      errorLocation(2) = j   
      RETURN
     END IF
    END IF
   END DO
  END DO

  RETURN
END SUBROUTINE Check2DRealDataValue

SUBROUTINE Check2DDblePrecDataValue(dataValue,         &
                                    expectedDataValue, &
                                    correctValue,      &
                                    errorLocation,     &
                                    errorInfoObject)

  DOUBLE PRECISION, DIMENSION(:,:), INTENT(IN) :: dataValue, &
                                                      expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  DOUBLE PRECISION :: den,num,scaledTol

  INTEGER, DIMENSION(2) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check2DDblePrecDataValue'

  DO n=1,2
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

  DO j=1,arrayLength1(2)
   DO i=1,arrayLength1(1)

    num = ABS((dataValue(i,j)) - (expectedDataValue(i,j)))

! if num = 0, these are equal by definition.

    IF (num == 0.0d0) THEN
     CONTINUE
    ELSE ! continue

     den = ABS(dataValue(i,j)) + ABS(expectedDataValue(i,j))

     scaledTol = tolDblePrec*den

     IF (num >= scaledTol) THEN
      correctValue = .FALSE.
      errorLocation(1) = i   
      errorLocation(2) = j   
      RETURN
     END IF
    END IF
   END DO
  END DO

  RETURN
END SUBROUTINE Check2DDblePrecDataValue

SUBROUTINE Check2DLogicalDataValue(dataValue,         &
                                   expectedDataValue, &
                                   correctValue,      &
                                   errorLocation,     &
                                   errorInfoObject)

  LOGICAL, DIMENSION(:,:), INTENT(IN) :: dataValue, &
                                             expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(2) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check2DLogicalDataValue'

  DO n=1,2
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

  DO j=1,arrayLength1(2)
   DO i=1,arrayLength1(1)

    IF (dataValue(i,j) .NEQV. expectedDataValue(i,j)) THEN
     correctValue = .FALSE.
     errorLocation(1) = i   
     errorLocation(2) = j   
     RETURN
    END IF
   END DO
  END DO

  RETURN
END SUBROUTINE Check2DLogicalDataValue
   
END MODULE Check2DData
