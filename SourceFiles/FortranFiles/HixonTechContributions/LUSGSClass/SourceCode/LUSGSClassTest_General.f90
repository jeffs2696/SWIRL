MODULE LUSGSClassTest_General
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE LUSGSClass
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: LUSGSClassTestGeneral

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE LUSGSClassTest_General: '

INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE LUSGSClassTestGeneral(LUSGSObject,     &
                                 masterNode,        &
                                 enableChecking,    &
                                 errorInfoObject)

  TYPE(LUSGSType), INTENT(INOUT) :: LUSGSObject
  LOGICAL, INTENT(IN) :: masterNode, &
                         enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  LOGICAL :: passedTest,    &
             errorExpected, &
             errorFound

! testing data

  INTEGER :: numberOfTopologicalDimensions

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE LUSGSClassTestGeneral'

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

!---------------------check that the LUSGSObject is not initialized------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check that LUSGSObject is not initialized.'
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  errorFound = ObjectIsInitialized(LUSGSObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: LUSGSObject should not be initialized.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED.'
  END IF

!---------------------try to destroy uninitialized object----------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to destroy an uninitialized LUSGSObject.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL DestroyObject(object            = LUSGSObject,     &
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

!--------------------Error check: too few dimensions ------------------

  numberOfTopologicalDimensions = 0

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to create an uninitialized LUSGSObject with too few dimensions.'
   WRITE(0,*) '          Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL CreateObject(object                        = LUSGSObject,                   &
                    numberOfTopologicalDimensions = numberOfTopologicalDimensions, &
                    enableChecking                = enableChecking,                &
                    errorInfoObject               = errorInfoObject)

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

  IF (masterNode) WRITE(0,*) 'Test PASSED.'

!--------------------Error check: too many dimensions ------------------

  numberOfTopologicalDimensions = 5

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to create an uninitialized LUSGSObject with too many dimensions.'
   WRITE(0,*) '          Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL CreateObject(object                        = LUSGSObject,                   &
                    numberOfTopologicalDimensions = numberOfTopologicalDimensions, &
                    enableChecking                = enableChecking,                &
                    errorInfoObject               = errorInfoObject)

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

  IF (masterNode) WRITE(0,*) 'Test PASSED.'

!--------------------test a successful initialization------------------

  numberOfTopologicalDimensions = 4

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to create an uninitialized LUSGSObject.'
   WRITE(0,*) '       No error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  CALL CreateObject(object                        = LUSGSObject,                   &
                    numberOfTopologicalDimensions = numberOfTopologicalDimensions, &
                    enableChecking                = enableChecking,                &
                    errorInfoObject               = errorInfoObject)

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

!---------------------try to destroy initialized object----------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to destroy an initialized LUSGSObject.'
   WRITE(0,*) '       No error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  CALL DestroyObject(object            = LUSGSObject,     &
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
END SUBROUTINE LUSGSClassTestGeneral

END MODULE LUSGSClassTest_General
