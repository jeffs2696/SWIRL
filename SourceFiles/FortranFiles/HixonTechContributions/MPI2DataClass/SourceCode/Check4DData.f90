MODULE Check4DData

  USE ErrorInformationClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CheckDataValue
  
INTERFACE CheckDataValue
  MODULE PROCEDURE Check4DInt4DataValue
  MODULE PROCEDURE Check4DInt8DataValue
  MODULE PROCEDURE Check4DCharDataValue
  MODULE PROCEDURE Check4DLogicalDataValue
  MODULE PROCEDURE Check4DRealDataValue
  MODULE PROCEDURE Check4DDblePrecDataValue
END INTERFACE CheckDataValue

REAL, PARAMETER :: tolReal = 1.0e-6_4

DOUBLE PRECISION, PARAMETER :: tolDblePrec = 1.0e-12_8

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE Check4DData: '

CONTAINS

SUBROUTINE Check4DInt4DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=4), DIMENSION(:,:,:,:), INTENT(IN) :: dataValue, &
                                                     expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(4) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check4DInt4DataValue'

  DO n=1,4
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

  DO l=1,arrayLength1(4)
   DO k=1,arrayLength1(3)
    DO j=1,arrayLength1(2)
     DO i=1,arrayLength1(1)
      IF (dataValue(i,j,k,l) /= expectedDataValue(i,j,k,l)) THEN
       correctValue = .FALSE.
       errorLocation(1) = i   
       errorLocation(2) = j   
       errorLocation(3) = k   
       errorLocation(4) = l   
       RETURN
      END IF
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check4DInt4DataValue

SUBROUTINE Check4DInt8DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=8), DIMENSION(:,:,:,:), INTENT(IN) :: dataValue, &
                                                     expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(4) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check4DInt8DataValue'

  DO n=1,4
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

  DO l=1,arrayLength1(4)
   DO k=1,arrayLength1(3)
    DO j=1,arrayLength1(2)
     DO i=1,arrayLength1(1)
      IF (dataValue(i,j,k,l) /= expectedDataValue(i,j,k,l)) THEN
       correctValue = .FALSE.
       errorLocation(1) = i   
       errorLocation(2) = j   
       errorLocation(3) = k   
       errorLocation(4) = l   
       RETURN
      END IF
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check4DInt8DataValue
   
SUBROUTINE Check4DCharDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  CHARACTER, DIMENSION(:,:,:,:), INTENT(IN) :: dataValue, &
                                         expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(4) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check4DCharDataValue'

  DO n=1,4
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

  DO l=1,arrayLength1(4)
   DO k=1,arrayLength1(3)
    DO j=1,arrayLength1(2)
     DO i=1,arrayLength1(1)
      IF (dataValue(i,j,k,l) /= expectedDataValue(i,j,k,l)) THEN
       correctValue = .FALSE.
       errorLocation(1) = i   
       errorLocation(2) = j   
       errorLocation(3) = k   
       errorLocation(4) = l   
       RETURN
      END IF
     END DO
    END DO
   END DO
  END DO

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check4DCharDataValue
   
SUBROUTINE Check4DRealDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  REAL, DIMENSION(:,:,:,:), INTENT(IN) :: dataValue, &
                                          expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL :: den,num,scaledTol

  INTEGER, DIMENSION(4) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check4DRealDataValue'

  DO n=1,4
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

  DO l=1,arrayLength1(4)
   DO k=1,arrayLength1(3)
    DO j=1,arrayLength1(2)
     DO i=1,arrayLength1(1)

      num = ABS((dataValue(i,j,k,l)) - (expectedDataValue(i,j,k,l)))

! if num = 0, these are equal by definition.

      IF (num == 0.0) THEN
       CONTINUE
      ELSE 

       den = ABS(dataValue(i,j,k,l)) + ABS(expectedDataValue(i,j,k,l))
 
       scaledTol = tolReal*den

       IF (num >= scaledTol) THEN
        correctValue = .FALSE.
        errorLocation(1) = i   
        errorLocation(2) = j   
        errorLocation(3) = k   
        errorLocation(4) = l   
        RETURN
       END IF
      END IF
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check4DRealDataValue

SUBROUTINE Check4DDblePrecDataValue(dataValue,         &
                                    expectedDataValue, &
                                    correctValue,      &
                                    errorLocation,     &
                                    errorInfoObject)

  DOUBLE PRECISION, DIMENSION(:,:,:,:), INTENT(IN) :: dataValue, &
                                                      expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  DOUBLE PRECISION :: den,num,scaledTol

  INTEGER, DIMENSION(4) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check4DDblePrecDataValue'

  DO n=1,4
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

  DO l=1,arrayLength1(4)
   DO k=1,arrayLength1(3)
    DO j=1,arrayLength1(2)
     DO i=1,arrayLength1(1)

      num = ABS((dataValue(i,j,k,l)) - (expectedDataValue(i,j,k,l)))

! if num = 0, these are equal by definition.

      IF (num == 0.0d0) THEN
       CONTINUE
      ELSE ! continue

       den = ABS(dataValue(i,j,k,l)) + ABS(expectedDataValue(i,j,k,l))

       scaledTol = tolDblePrec*den

       IF (num >= scaledTol) THEN
        correctValue = .FALSE.
        errorLocation(1) = i   
        errorLocation(2) = j   
        errorLocation(3) = k   
        errorLocation(4) = l   
        RETURN
       END IF
      END IF
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check4DDblePrecDataValue

SUBROUTINE Check4DLogicalDataValue(dataValue,         &
                                   expectedDataValue, &
                                   correctValue,      &
                                   errorLocation,     &
                                   errorInfoObject)

  LOGICAL, DIMENSION(:,:,:,:), INTENT(IN) :: dataValue, &
                                             expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(4) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check4DLogicalDataValue'

  DO n=1,4
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

  DO l=1,arrayLength1(4)
   DO k=1,arrayLength1(3)
    DO j=1,arrayLength1(2)
     DO i=1,arrayLength1(1)

      IF (dataValue(i,j,k,l) .NEQV. expectedDataValue(i,j,k,l)) THEN
       correctValue = .FALSE.
       errorLocation(1) = i   
       errorLocation(2) = j   
       errorLocation(3) = k   
       errorLocation(4) = l   
       RETURN
      END IF
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check4DLogicalDataValue
   
END MODULE Check4DData
