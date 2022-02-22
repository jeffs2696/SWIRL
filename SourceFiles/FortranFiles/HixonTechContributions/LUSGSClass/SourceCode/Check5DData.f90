MODULE Check5DData

  USE ErrorInformationClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CheckDataValue
  
INTERFACE CheckDataValue
  MODULE PROCEDURE Check5DInt5DataValue
  MODULE PROCEDURE Check5DInt8DataValue
  MODULE PROCEDURE Check5DCharDataValue
  MODULE PROCEDURE Check5DLogicalDataValue
  MODULE PROCEDURE Check5DRealDataValue
  MODULE PROCEDURE Check5DDblePrecDataValue
END INTERFACE CheckDataValue

REAL, PARAMETER :: tolReal = 5.0e-6_4

DOUBLE PRECISION, PARAMETER :: tolDblePrec = 5.0e-12_8

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE Check5DData: '

CONTAINS

SUBROUTINE Check5DInt5DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=4), DIMENSION(:,:,:,:,:), INTENT(IN) :: dataValue, &
                                                       expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(5) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check5DInt5DataValue'

  DO n=1,5
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

  DO m=1,arrayLength1(5)
   DO l=1,arrayLength1(4)
    DO k=1,arrayLength1(3)
     DO j=1,arrayLength1(2)
      DO i=1,arrayLength1(1)
       IF (dataValue(i,j,k,l,m) /= expectedDataValue(i,j,k,l,m)) THEN
        correctValue = .FALSE.
        errorLocation(1) = i   
        errorLocation(2) = j   
        errorLocation(3) = k   
        errorLocation(4) = l   
        errorLocation(5) = m   
        RETURN
       END IF
      END DO
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check5DInt5DataValue

SUBROUTINE Check5DInt8DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=8), DIMENSION(:,:,:,:,:), INTENT(IN) :: dataValue, &
                                                       expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(5) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check5DInt8DataValue'

  DO n=1,5
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

  DO m=1,arrayLength1(5)
   DO l=1,arrayLength1(4)
    DO k=1,arrayLength1(3)
     DO j=1,arrayLength1(2)
      DO i=1,arrayLength1(1)
       IF (dataValue(i,j,k,l,m) /= expectedDataValue(i,j,k,l,m)) THEN
        correctValue = .FALSE.
        errorLocation(1) = i   
        errorLocation(2) = j   
        errorLocation(3) = k   
        errorLocation(4) = l   
        errorLocation(5) = m   
        RETURN
       END IF
      END DO
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check5DInt8DataValue
   
SUBROUTINE Check5DCharDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  CHARACTER, DIMENSION(:,:,:,:,:), INTENT(IN) :: dataValue, &
                                                 expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(5) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check5DCharDataValue'

  DO n=1,5
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

  DO m=1,arrayLength1(5)
   DO l=1,arrayLength1(4)
    DO k=1,arrayLength1(3)
     DO j=1,arrayLength1(2)
      DO i=1,arrayLength1(1)
       IF (dataValue(i,j,k,l,m) /= expectedDataValue(i,j,k,l,m)) THEN
        correctValue = .FALSE.
        errorLocation(1) = i   
        errorLocation(2) = j   
        errorLocation(3) = k   
        errorLocation(4) = l   
        errorLocation(5) = m   
        RETURN
       END IF
      END DO
     END DO
    END DO
   END DO
  END DO

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check5DCharDataValue
   
SUBROUTINE Check5DRealDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  REAL, DIMENSION(:,:,:,:,:), INTENT(IN) :: dataValue, &
                                            expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL :: den,num,scaledTol

  INTEGER, DIMENSION(5) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check5DRealDataValue'

  DO n=1,5
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

  DO m=1,arrayLength1(5)
   DO l=1,arrayLength1(4)
    DO k=1,arrayLength1(3)
     DO j=1,arrayLength1(2)
      DO i=1,arrayLength1(1)

       num = ABS((dataValue(i,j,k,l,m)) - (expectedDataValue(i,j,k,l,m)))

! if num = 0, these are equal by definition.

       IF (num == 0.0) THEN
        CONTINUE
       ELSE 

        den = ABS(dataValue(i,j,k,l,m)) + ABS(expectedDataValue(i,j,k,l,m))
  
        scaledTol = tolReal*den
 
        IF (num >= scaledTol) THEN
         correctValue = .FALSE.
         errorLocation(1) = i   
         errorLocation(2) = j   
         errorLocation(3) = k   
         errorLocation(4) = l   
         errorLocation(5) = m   
         RETURN
        END IF
       END IF
      END DO
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check5DRealDataValue

SUBROUTINE Check5DDblePrecDataValue(dataValue,         &
                                    expectedDataValue, &
                                    correctValue,      &
                                    errorLocation,     &
                                    errorInfoObject)

  DOUBLE PRECISION, DIMENSION(:,:,:,:,:), INTENT(IN) :: dataValue, &
                                                        expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  DOUBLE PRECISION :: den,num,scaledTol

  INTEGER, DIMENSION(5) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check5DDblePrecDataValue'

  DO n=1,5
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

  DO m=1,arrayLength1(5)
   DO l=1,arrayLength1(4)
    DO k=1,arrayLength1(3)
     DO j=1,arrayLength1(2)
      DO i=1,arrayLength1(1)

       num = ABS((dataValue(i,j,k,l,m)) - (expectedDataValue(i,j,k,l,m)))

! if num = 0, these are equal by definition.

       IF (num == 0.0d0) THEN
        CONTINUE
       ELSE 

        den = ABS(dataValue(i,j,k,l,m)) + ABS(expectedDataValue(i,j,k,l,m))
  
        scaledTol = tolDblePrec*den
 
        IF (num >= scaledTol) THEN
         correctValue = .FALSE.
         errorLocation(1) = i   
         errorLocation(2) = j   
         errorLocation(3) = k   
         errorLocation(4) = l   
         errorLocation(5) = m   
         RETURN
        END IF
       END IF
      END DO
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check5DDblePrecDataValue

SUBROUTINE Check5DLogicalDataValue(dataValue,         &
                                   expectedDataValue, &
                                   correctValue,      &
                                   errorLocation,     &
                                   errorInfoObject)

  LOGICAL, DIMENSION(:,:,:,:,:), INTENT(IN) :: dataValue, &
                                               expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, DIMENSION(5) :: arrayLength1,arrayLength2
  INTEGER :: n,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check5DLogicalDataValue'

  DO n=1,5
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

  DO m=1,arrayLength1(5)
   DO l=1,arrayLength1(4)
    DO k=1,arrayLength1(3)
     DO j=1,arrayLength1(2)
      DO i=1,arrayLength1(1)

       IF (dataValue(i,j,k,l,m) .NEQV. expectedDataValue(i,j,k,l,m)) THEN
        correctValue = .FALSE.
        errorLocation(1) = i   
        errorLocation(2) = j   
        errorLocation(3) = k   
        errorLocation(4) = l   
        errorLocation(5) = m   
        RETURN
       END IF
      END DO
     END DO
    END DO
   END DO
  END DO

  RETURN
END SUBROUTINE Check5DLogicalDataValue
   
END MODULE Check5DData
