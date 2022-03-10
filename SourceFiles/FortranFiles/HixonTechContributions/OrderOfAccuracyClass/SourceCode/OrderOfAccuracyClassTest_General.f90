MODULE OrderOfAccuracyClassTest_General
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE Check1DData
  USE OrderOfAccuracyClass
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: OrderOfAccuracyClassTestGeneral

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE OrderOfAccuracyClassTest_General: '

INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE OrderOfAccuracyClassTestGeneral(OofAObject,     &
                                           masterNode,        &
                                           enableChecking,    &
                                           errorInfoObject)

  TYPE(OrderOfAccuracyType), INTENT(INOUT) :: OofAObject
  LOGICAL, INTENT(IN) :: masterNode, &
                         enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  LOGICAL :: passedTest,    &
             errorExpected, &
             errorFound,    &
             correctValue

! testing data

  INTEGER :: iMin, &
             iMax, &
             i

  INTEGER, DIMENSION(1) :: errorLocation

  REAL(KIND=rDef) :: rDum

  REAL(KIND=rDef), DIMENSION(1) :: badErrorMagnitude, &
                                   badGridSpacing,    &
                                   badCalculatedOrderOfAccuracy

  REAL(KIND=rDef), DIMENSION(4) :: errorMagnitude,            &
                                   gridSpacing,               &
                                   calculatedOrderOfAccuracy, &
                                   expectedOrderOfAccuracy

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE OrderOfAccuracyClassTestGeneral'

  CONTINUE ! execution begins here

  IF (masterNode) THEN
   WRITE(0,98) 
   WRITE(0,99) 
   WRITE(0,*) 'Beginning ',location
  ELSE
   CONTINUE
  END IF

  passedTest = .TRUE.

  IF (enableChecking) THEN
   IF (ObjectIsInitialized(errorInfoObject)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'ERROR: ErrorInformationObject is not initialized!'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)
    passedTest = .FALSE.
    GO TO 100
   END IF
  ELSE
   CONTINUE
  END IF

!---------------------check that the OofAObject is not initialized------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check that OofAObject is not initialized.'
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  errorFound = ObjectIsInitialized(OofAObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: OofAObject should not be initialized.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED.'
  END IF

!---------------------try to destroy uninitialized object----------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to destroy an uninitialized OofAObject.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL DestroyObject(object            = OofAObject,     &
                     enableChecking    = enableChecking,    &
                     errorInfoObject   = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!--------------------test a successful initialization------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to create an uninitialized OofAObject.'
   WRITE(0,*) '       No error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  CALL CreateObject(object                   = OofAObject,            &
                    enableChecking           = enableChecking,           &
                    errorInfoObject          = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    CONTINUE
   END IF
  END IF

  IF (masterNode) WRITE(0,*) 'Test PASSED.'

!---------test errors from initialized object-------------------------------

  iMin = 1
  iMax = 1

!----error test: errorMagnitude length too short---------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: errorMagnitude array is too short.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL CalculateOrderOfAccuracy(                                             &
                      object                    = OofAObject,                &
                      iMin                      = iMin,                      &
                      iMax                      = iMax,                      &
                      errorMagnitude            = badErrorMagnitude,         &
                      gridSpacing               = badGridSpacing,            &
                      calculatedOrderOfAccuracy = calculatedOrderOfAccuracy, &
                      enableChecking            = enableChecking,            &
                      errorInfoObject           = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!----error test: different gridSpacing length---------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: gridSpacing array is a different length.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL CalculateOrderOfAccuracy(                                             &
                      object                    = OofAObject,                &
                      iMin                      = iMin,                      &
                      iMax                      = iMax,                      &
                      errorMagnitude            = errorMagnitude,            &
                      gridSpacing               = badGridSpacing,            &
                      calculatedOrderOfAccuracy = calculatedOrderOfAccuracy, &
                      enableChecking            = enableChecking,            &
                      errorInfoObject           = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!----error test: different calculatedOrderOfAccuracy length---------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: calculatedOrderOfAccuracy array is a different length.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL CalculateOrderOfAccuracy(                                             &
                      object                    = OofAObject,                &
                      iMin                      = iMin,                      &
                      iMax                      = iMax,                      &
                      errorMagnitude            = errorMagnitude,            &
                      gridSpacing               = gridSpacing,               &
                      calculatedOrderOfAccuracy = badCalculatedOrderOfAccuracy, &
                      enableChecking            = enableChecking,            &
                      errorInfoObject           = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!----error test: iMin too small---------------

  iMin = 0

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: iMin is too small.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL CalculateOrderOfAccuracy(                                             &
                      object                    = OofAObject,                &
                      iMin                      = iMin,                      &
                      iMax                      = iMax,                      &
                      errorMagnitude            = errorMagnitude,            &
                      gridSpacing               = gridSpacing,               &
                      calculatedOrderOfAccuracy = CalculatedOrderOfAccuracy, &
                      enableChecking            = enableChecking,            &
                      errorInfoObject           = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!----error test: iMax too big---------------

  iMin = 1
  iMax = 6

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: iMax is too large.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL CalculateOrderOfAccuracy(                                             &
                      object                    = OofAObject,                &
                      iMin                      = iMin,                      &
                      iMax                      = iMax,                      &
                      errorMagnitude            = errorMagnitude,            &
                      gridSpacing               = gridSpacing,               &
                      calculatedOrderOfAccuracy = CalculatedOrderOfAccuracy, &
                      enableChecking            = enableChecking,            &
                      errorInfoObject           = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!----error test: iMin > iMax---------------

  iMin = 2
  iMax = 1

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: iMin is greater than iMax.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL CalculateOrderOfAccuracy(                                             &
                      object                    = OofAObject,                &
                      iMin                      = iMin,                      &
                      iMax                      = iMax,                      &
                      errorMagnitude            = errorMagnitude,            &
                      gridSpacing               = gridSpacing,               &
                      calculatedOrderOfAccuracy = calculatedOrderOfAccuracy, &
                      enableChecking            = enableChecking,            &
                      errorInfoObject           = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!----actual test of the subroutine---------------------

  iMin = 1
  iMax = 4

! initial values

  CALL RANDOM_NUMBER(gridSpacing(1))
  CALL RANDOM_NUMBER(errorMagnitude(1))

  expectedOrderOfAccuracy(1) = 0.0_rDef

  DO i=2,4
   CALL RANDOM_NUMBER(rDum)

   gridSpacing(i) = gridSpacing(i-1)/(1.0_rDef+rDum)
   errorMagnitude(i) = errorMagnitude(i-1) &
                       *((gridSpacing(i)/gridSpacing(i-1))**(2*(i-1)))  
  
   expectedOrderOfAccuracy(i) = REAL(2*(i-1),rDef)
  END DO

  IF (masterNode) THEN
   DO i=iMin,iMax
    WRITE(6,*) i,gridSpacing(i),errorMagnitude(i),expectedOrderOfAccuracy(i)
   END DO
  END IF

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: determine order of accuracy.'
   WRITE(0,*) '    No error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  CALL CalculateOrderOfAccuracy(                                             &
                      object                    = OofAObject,                &
                      iMin                      = iMin,                      &
                      iMax                      = iMax,                      &
                      errorMagnitude            = errorMagnitude,            &
                      gridSpacing               = gridSpacing,               &
                      calculatedOrderOfAccuracy = calculatedOrderOfAccuracy, &
                      enableChecking            = enableChecking,            &
                      errorInfoObject           = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    CONTINUE
   END IF
  END IF

  CALL CheckDataValue(dataValue         = calculatedOrderOfAccuracy, &
                      expectedDataValue = expectedOrderOfAccuracy,   &
                      correctValue      = correctValue,              &
                      errorLocation     = errorLocation,             &
                      errorInfoObject   = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound) THEN
   passedTest = .FALSE.
   GO TO 101
  ELSE
   CONTINUE
  END IF

  IF (correctValue) THEN
   CONTINUE
  ELSE
   WRITE(charStringObject%charString,'(a,i2)') &
       'Incorrect order of accuracy found at entry ',errorLocation(1)
   passedTest = .FALSE.
   GO TO 100
  END IF 

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    CONTINUE
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!---------------------try to destroy initialized object----------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to destroy an initialized OofAObject.'
   WRITE(0,*) '       No error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  CALL DestroyObject(object            = OofAObject,     &
                     enableChecking    = enableChecking,    &
                     errorInfoObject   = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: no error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
! reset the errorInfoObject
   CALL WriteObject(object = errorInfoObject)
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
   IF (CheckForLocalError(errorInfoObject)) THEN
    charStringObject%charString = 'Error in resetting errorInfoObject.'
    passedTest = .FALSE.
    GO TO 100
   ELSE 
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!---------------end of testing-------------------------------------------------

  IF (passedTest) THEN
   IF (masterNode) THEN
    WRITE(0,98) 
    WRITE(0,99) 
    WRITE(0,*) 'PASSED all tests in ',location
   ELSE
    CONTINUE
   END IF
  ELSE
   charStringObject%charString = 'ERROR: did not successfully complete tests!'
   GO TO 100
  END IF

  RETURN
 
 100 CONTINUE

   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
 101 CONTINUE

   charStringObject%charString = moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = '-----------Test FAILED!------------------ '
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

  RETURN
 98 FORMAT(1x,80(' '))
 99 FORMAT(1x,80('-'))
END SUBROUTINE OrderOfAccuracyClassTestGeneral

END MODULE OrderOfAccuracyClassTest_General
