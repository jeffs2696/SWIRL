MODULE Check0DData

  USE ErrorInformationClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: CheckDataValue
  
INTERFACE CheckDataValue
  MODULE PROCEDURE Check0DInt4DataValue
  MODULE PROCEDURE Check0DInt8DataValue
  MODULE PROCEDURE Check0DCharDataValue
  MODULE PROCEDURE Check0DLogicalDataValue
  MODULE PROCEDURE Check0DRealDataValue
  MODULE PROCEDURE Check0DDblePrecDataValue
END INTERFACE CheckDataValue

REAL, PARAMETER :: tolReal = 1.0e-6_4

DOUBLE PRECISION, PARAMETER :: tolDblePrec = 1.0e-12_8

!TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE Check0DData: '

CONTAINS

SUBROUTINE Check0DInt4DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorInfoObject)

  INTEGER(KIND=4), INTENT(IN) :: dataValue, &
                                 expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  IF (dataValue /= expectedDataValue) THEN
   correctValue = .FALSE.
  ELSE
   correctValue = .TRUE.
  END IF

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check0DInt4DataValue

SUBROUTINE Check0DInt8DataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorInfoObject)

  INTEGER(KIND=8), INTENT(IN) :: dataValue, &
                                 expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  IF (dataValue /= expectedDataValue) THEN
   correctValue = .FALSE.
  ELSE
   correctValue = .TRUE.
  END IF

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check0DInt8DataValue
   
SUBROUTINE Check0DCharDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorInfoObject)

  CHARACTER, INTENT(IN) :: dataValue, &
                           expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  IF (dataValue /= expectedDataValue) THEN
   correctValue = .FALSE.
  ELSE
   correctValue = .TRUE.
  END IF

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check0DCharDataValue
   
SUBROUTINE Check0DRealDataValue(dataValue,         &
                                expectedDataValue, &
                                correctValue,      &
                                errorInfoObject)

  REAL, INTENT(IN) :: dataValue, &
                      expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL :: den,num,scaledTol

  num = ABS((dataValue) - (expectedDataValue))

! if num = 0, these are equal by definition.

  IF (num == 0.0) THEN
   correctValue = .TRUE.
   RETURN
  END IF

  den = ABS(dataValue) + ABS(expectedDataValue)

  scaledTol = tolReal*den

  IF (num >= scaledTol) THEN
   correctValue = .FALSE.
  ELSE
   correctValue = .TRUE.
  END IF

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check0DRealDataValue

SUBROUTINE Check0DDblePrecDataValue(dataValue,         &
                                    expectedDataValue, &
                                    correctValue,      &
                                    errorInfoObject)

  DOUBLE PRECISION, INTENT(IN) :: dataValue, &
                                  expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  DOUBLE PRECISION :: den,num,scaledTol

  num = ABS((dataValue) - (expectedDataValue))

! if num = 0, these are equal by definition.

  IF (num == 0.0d0) THEN
   correctValue = .TRUE.
   RETURN
  END IF

  den = ABS(dataValue) + ABS(expectedDataValue)

  scaledTol = tolDblePrec*den

  IF (num >= scaledTol) THEN
   correctValue = .FALSE.
  ELSE
   correctValue = .TRUE.
  END IF

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check0DDblePrecDataValue

SUBROUTINE Check0DLogicalDataValue(dataValue,         &
                                   expectedDataValue, &
                                   correctValue,      &
                                   errorInfoObject)

  LOGICAL, INTENT(IN) :: dataValue, &
                         expectedDataValue
  LOGICAL, INTENT(OUT) :: correctValue
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  IF (dataValue .NEQV. expectedDataValue) THEN
   correctValue = .FALSE.
  ELSE
   correctValue = .TRUE.
  END IF

  RETURN
  IF (CheckForLocalError(errorInfoObject)) RETURN ! to fool compiler
END SUBROUTINE Check0DLogicalDataValue
   
END MODULE Check0DData
