MODULE BlockTridiSolverClassTest_General
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE BlockTridiSolverClass
  USE ErrorInformationClass
  USE MessagePassingInterface
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: BlockTridiSolverClassTestGeneral

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE BlockTridiSolverClassTest_General: '

INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE BlockTridiSolverClassTestGeneral(blockTridiSolverObject, &
                                            masterNode,             &
                                            enableChecking,         &
                                            errorInfoObject)

  TYPE(BlockTridiSolverType), INTENT(INOUT) :: blockTridiSolverObject
  LOGICAL, INTENT(IN) :: masterNode, &
                         enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  LOGICAL :: passedTest,    &
             errorExpected, &
             errorFound

! local variables

  INTEGER :: numberOfMatrixDimensions, &
             numberOfVariables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE BlockTridiSolverClassTestGeneral'

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

!---------------------check that the blockTridiSolverObject is not initialized------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check that blockTridiSolverObject is not initialized.'
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  errorFound = ObjectIsInitialized(blockTridiSolverObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: blockTridiSolverObject should not be initialized.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED.'
  END IF

!---------------------try to destroy uninitialized object-----------------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to destroy an uninitialized blockTridiSolverObject.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

  CALL DestroyObject(object          = blockTridiSolverObject, &
                     enableChecking  = enableChecking,      &
                     errorInfoObject = errorInfoObject)

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

!---------------------try to create object with too few matrix dimensions----------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to create a blockTridiSolverObject with too few matrix dimensions.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  numberOfMatrixDimensions = 0
  numberOfVariables = 3

  errorExpected = .TRUE.

  CALL CreateObject(object                   = blockTridiSolverObject,   &
                    numberOfMatrixDimensions = numberOfMatrixDimensions, &
                    numberOfVariables        = numberOfVariables,        &
                    enableChecking           = enableChecking,           &
                    errorInfoObject          = errorInfoObject)

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

!---------------------try to create object with too many matrix dimensions----------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to create a blockTridiSolverObject with too many matrix dimensions.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  numberOfMatrixDimensions  = 5

  errorExpected = .TRUE.

  CALL CreateObject(object                   = blockTridiSolverObject,   &
                    numberOfMatrixDimensions = numberOfMatrixDimensions, &
                    numberOfVariables        = numberOfVariables,        &
                    enableChecking           = enableChecking,           &
                    errorInfoObject          = errorInfoObject)

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

!---------------------try to create object with too few topology dimensions----------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: attempt to create a blockTridiSolverObject with too few variable dimensions.'
   WRITE(0,*) '       Error is expected. '
  ELSE
   CONTINUE
  END IF

  numberOfMatrixDimensions = 1
  numberOfVariables        = 0

  errorExpected = .TRUE.

  CALL CreateObject(object                   = blockTridiSolverObject,   &
                    numberOfMatrixDimensions = numberOfMatrixDimensions, &
                    numberOfVariables        = numberOfVariables,        &
                    enableChecking           = enableChecking,           &
                    errorInfoObject          = errorInfoObject)

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
END SUBROUTINE BlockTridiSolverClassTestGeneral

END MODULE BlockTridiSolverClassTest_General
