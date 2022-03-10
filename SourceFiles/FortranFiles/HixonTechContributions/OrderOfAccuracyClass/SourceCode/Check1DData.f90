MODULE Check1DData

! no tolerance scaling is being used.

  USE ErrorInformationClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CheckDataValue
  
INTERFACE CheckDataValue
  MODULE PROCEDURE Check1DInt4DataValue
  MODULE PROCEDURE Check1DInt8DataValue
  MODULE PROCEDURE Check1DCharDataValue
  MODULE PROCEDURE Check1DLogicalDataValue
  MODULE PROCEDURE Check1DRealDataValue
  MODULE PROCEDURE Check1DDblePrecDataValue
END INTERFACE CheckDataValue

REAL, PARAMETER :: tolReal = 1.0e-6_4

DOUBLE PRECISION, PARAMETER :: tolDblePrec = 1.0e-12_8

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE Check1DData: '

CONTAINS

SUBROUTINE Check1DInt4DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: dataValue, &
                                               expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: arrayLength1,arrayLength2, n

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check1DIntDataValue'

  arrayLength1 = SIZE(dataValue)

  arrayLength2 = SIZE(expectedDataValue)

  IF (arrayLength1 /= arrayLength2) THEN

   WRITE(charStringObject%charString,'(1x,a34,i5,a7,i5,a2 )')  &
                                            'Array lengths are different: (dV: ', &
                                            arrayLength1, &
                                            '; dVE: ',    &
                                            arrayLength2, &
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

  correctValue = .TRUE.
  errorLocation(1) = -1

  DO n=1,arrayLength1
   IF (dataValue(n) /= expectedDataValue(n)) THEN
    correctValue = .FALSE.
    errorLocation(1) = n   
    RETURN
   END IF
  END DO

  RETURN
END SUBROUTINE Check1DInt4DataValue

SUBROUTINE Check1DInt8DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  INTEGER(KIND=8), DIMENSION(:), INTENT(IN) :: dataValue, &
                                               expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: arrayLength1,arrayLength2, n

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check1DIntDataValue'

  arrayLength1 = SIZE(dataValue)

  arrayLength2 = SIZE(expectedDataValue)

  IF (arrayLength1 /= arrayLength2) THEN

   WRITE(charStringObject%charString,'(1x,a34,i5,a7,i5,a2 )')  &
                                            'Array lengths are different: (dV: ', &
                                            arrayLength1, &
                                            '; dVE: ',    &
                                            arrayLength2, &
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

  correctValue = .TRUE.
  errorLocation(1) = -1

  DO n=1,arrayLength1
   IF (dataValue(n) /= expectedDataValue(n)) THEN
    correctValue = .FALSE.
    errorLocation(1) = n   
    RETURN
   END IF
  END DO

  RETURN
END SUBROUTINE Check1DInt8DataValue
   
SUBROUTINE Check1DCharDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  CHARACTER, DIMENSION(:), INTENT(IN) :: dataValue, &
                                         expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: arrayLength1,arrayLength2, n

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check1DCharDataValue'

  arrayLength1 = SIZE(dataValue)

  arrayLength2 = SIZE(expectedDataValue)

  IF (arrayLength1 /= arrayLength2) THEN

   WRITE(charStringObject%charString,'(1x,a34,i5,a7,i5,a2 )')  &
                                            'Array lengths are different: (dV: ', &
                                            arrayLength1, &
                                            '; dVE: ',    &
                                            arrayLength2, &
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

  correctValue = .TRUE.
  errorLocation(1) = -1

  DO n=1,arrayLength1
   IF (dataValue(n) /= expectedDataValue(n)) THEN
    correctValue = .FALSE.
    errorLocation(1) = n   
    RETURN
   END IF
  END DO

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check1DCharDataValue
   
SUBROUTINE Check1DRealDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorLocation,     &
                                errorInfoObject)

  REAL, DIMENSION(:), INTENT(IN) :: dataValue, &
                                    expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL :: den,num,scaledTol

  INTEGER :: arrayLength1,arrayLength2, n

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check1DRealDataValue'

  arrayLength1 = SIZE(dataValue)

  arrayLength2 = SIZE(expectedDataValue)

  IF (arrayLength1 /= arrayLength2) THEN

   WRITE(charStringObject%charString,'(1x,a34,i5,a7,i5,a2 )')  &
                                            'Array lengths are different: (dV: ', &
                                            arrayLength1, &
                                            '; dVE: ',    &
                                            arrayLength2, &
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

  correctValue = .TRUE.
  errorLocation(1) = -1

  DO n=1,arrayLength1

   num = ABS((dataValue(n)) - (expectedDataValue(n)))

! if num = 0, these are equal by definition.

   IF (num == 0.0) THEN
    CONTINUE
   ELSE

    den = ABS(dataValue(n)) + ABS(expectedDataValue(n))

!   scaledTol = tolReal*den
    scaledTol = tolReal

    IF (num >= scaledTol) THEN
     correctValue = .FALSE.
     errorLocation(1) = n   
     RETURN
    END IF
   END IF
  END DO

  RETURN
END SUBROUTINE Check1DRealDataValue

SUBROUTINE Check1DDblePrecDataValue(dataValue,         &
                                    expectedDataValue, &
                                    correctValue,      &
                                    errorLocation,     &
                                    errorInfoObject)

  DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: dataValue, &
                                                expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  DOUBLE PRECISION :: den,num,scaledTol

  INTEGER :: arrayLength1,arrayLength2, n

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check1DDblePrecDataValue'

  arrayLength1 = SIZE(dataValue)

  arrayLength2 = SIZE(expectedDataValue)

  IF (arrayLength1 /= arrayLength2) THEN

   WRITE(charStringObject%charString,'(1x,a34,i5,a7,i5,a2 )')  &
                                            'Array lengths are different: (dV: ', &
                                            arrayLength1, &
                                            '; dVE: ',    &
                                            arrayLength2, &
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

  correctValue = .TRUE.
  errorLocation(1) = -1

  DO n=1,arrayLength1

   num = ABS((dataValue(n)) - (expectedDataValue(n)))

! if num = 0, these are equal by definition.

   IF (num == 0.0d0) THEN
    CONTINUE
   ELSE ! continue

    den = ABS(dataValue(n)) + ABS(expectedDataValue(n))

!   scaledTol = tolDblePrec*den
    scaledTol = tolDblePrec

    IF (num >= scaledTol) THEN
     correctValue = .FALSE.
     errorLocation(1) = n 
     RETURN
    END IF
   END IF
  END DO

  RETURN
END SUBROUTINE Check1DDblePrecDataValue

SUBROUTINE Check1DLogicalDataValue(dataValue,         &
                                   expectedDataValue, &
                                   correctValue,      &
                                   errorLocation,     &
                                   errorInfoObject)

  LOGICAL, DIMENSION(:), INTENT(IN) :: dataValue, &
                                       expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  INTEGER, DIMENSION(:), INTENT(OUT) :: errorLocation
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: arrayLength1,arrayLength2, n

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE Check1DLogicalDataValue'

  arrayLength1 = SIZE(dataValue)

  arrayLength2 = SIZE(expectedDataValue)

  IF (arrayLength1 /= arrayLength2) THEN

   WRITE(charStringObject%charString,'(1x,a34,i5,a7,i5,a2 )')  &
                                            'Array lengths are different: (dV: ', &
                                            arrayLength1, &
                                            '; dVE: ',    &
                                            arrayLength2, &
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

  correctValue = .TRUE.
  errorLocation(1) = -1

  DO n=1,arrayLength1
   IF (dataValue(n) .NEQV. expectedDataValue(n)) THEN
    correctValue = .FALSE.
    errorLocation(1) = n   
    RETURN
   END IF
  END DO

  RETURN
END SUBROUTINE Check1DLogicalDataValue
   
END MODULE Check1DData
