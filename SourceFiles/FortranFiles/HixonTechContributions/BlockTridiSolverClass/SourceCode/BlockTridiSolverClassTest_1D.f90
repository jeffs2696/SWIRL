MODULE BlockTridiSolverClassTest_1D
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE BlockTridiSolverClass
  USE ErrorInformationClass
  USE MessagePassingInterface
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: BlockTridiSolverClassTest1D

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE BlockTridiSolverClassTest_1D: '

INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE BlockTridiSolverClassTest1D(blockTridiSolverObject, &
                                       masterNode,             &
                                       errorTolerance,         &
                                       enableChecking,         &
                                       errorInfoObject)

  TYPE(BlockTridiSolverType), INTENT(INOUT) :: blockTridiSolverObject
  REAL(KIND=rDef), INTENT(IN) :: errorTolerance
  LOGICAL, INTENT(IN) :: masterNode, &
                         enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER, PARAMETER :: numberOfMatrixDimensions = 1,  &
                        numberOfVariables        = 3,  &
                        nPts                     = 21

  INTEGER, DIMENSION(1), PARAMETER ::                           &
     iSolveStart             = (/ 2 /),                         &
     iSolveEnd               = (/ iSolveStart(1) + nPts - 1 /), &
     lowerDiagonalArrayStart = (/ iSolveStart(1) /),            &
     diagonalArrayStart      = (/ iSolveStart(1) /),            &
     upperDiagonalArrayStart = (/ iSolveStart(1) /),            &
     rhsArrayStart           = (/ iSolveStart(1) /),            &
     solutionArrayStart      = (/ 1 /)

  LOGICAL :: passedTest,    &
             errorExpected, &
             errorFound

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE BlockTridiSolverClassTest1D'

  INTEGER :: solveDimension

  REAL(KIND=rDef), DIMENSION(iSolveStart(1):iSolveEnd(1), &
                             numberOfVariables,           &  
                             numberOfVariables) ::        &
                   lowerDiagonal,                         &
                   diagonal,                              &
                   upperDiagonal 

  REAL(KIND=rDef), DIMENSION(iSolveEnd(1)+1,              &
                             numberOfVariables) ::        &
                   solutionVector,                        &
                   actualSolutionVector

  REAL(KIND=rDef), DIMENSION(iSolveStart(1):iSolveEnd(1), &
                             numberOfVariables) ::        &
                   rhsVector

  INTEGER :: i, m

  REAL(KIND=rDef) :: fac1,fac2,fac3,pi,del,err2,l2

  CONTINUE ! execution begins here

  pi = 4.0_rDef*ATAN(1.0_rDef)

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

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: create a 1D blockTridiSolverObject.'
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  CALL CreateObject(object                   = blockTridiSolverObject,   &
                    numberOfMatrixDimensions = numberOfMatrixDimensions, &
                    numberOfVariables        = numberOfVariables,        &
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
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!-----------------test the block tridi solver---------------------------

!-----------generate the test data------------------------

  DO i=iSolveStart(1),iSolveEnd(1)
   fac1 = REAL(i,rDef)
   fac3 = REAL(nPts+1-i,rDef)
   fac2 = MAX(fac1,fac3)
! lower diagonal matrix (row, column)
   lowerDiagonal(i,1,1) = 1.0_rDef*fac1
   lowerDiagonal(i,1,2) = 2.0_rDef*fac1
   lowerDiagonal(i,1,3) = 2.0_rDef*fac1

   lowerDiagonal(i,2,1) = 2.0_rDef*fac1
   lowerDiagonal(i,2,2) = 1.0_rDef*fac1
   lowerDiagonal(i,2,3) = 2.0_rDef*fac1

   lowerDiagonal(i,3,1) = 2.0_rDef*fac1
   lowerDiagonal(i,3,2) = 2.0_rDef*fac1
   lowerDiagonal(i,3,3) = 1.0_rDef*fac1
! diagonal matrix (row, column)
   diagonal(i,1,1) = 1.0_rDef*fac2
   diagonal(i,1,2) = 1.0_rDef*fac2
   diagonal(i,1,3) =-1.0_rDef*fac2

   diagonal(i,2,1) =-1.0_rDef*fac2
   diagonal(i,2,2) = 1.0_rDef*fac2
   diagonal(i,2,3) = 1.0_rDef*fac2

   diagonal(i,3,1) = 1.0_rDef*fac2
   diagonal(i,3,2) =-1.0_rDef*fac2
   diagonal(i,3,3) = 1.0_rDef*fac2
! upper diagonal matrix (row, column)
   upperDiagonal(i,1,1) = 1.0_rDef*fac3
   upperDiagonal(i,1,2) = 2.0_rDef*fac3
   upperDiagonal(i,1,3) = 4.0_rDef*fac3

   upperDiagonal(i,2,1) = 4.0_rDef*fac3
   upperDiagonal(i,2,2) = 1.0_rDef*fac3
   upperDiagonal(i,2,3) = 2.0_rDef*fac3

   upperDiagonal(i,3,1) = 2.0_rDef*fac3
   upperDiagonal(i,3,2) = 4.0_rDef*fac3
   upperDiagonal(i,3,3) = 1.0_rDef*fac3
  END DO

! generate the actual solution vector

  DO i=1,iSolveEnd(1) + 1
   actualSolutionVector(i,1) = 0.0_rDef
   actualSolutionVector(i,2) = 0.0_rDef
   actualSolutionVector(i,3) = 0.0_rDef
   solutionVector(i,1)       = 0.0_rDef
   solutionVector(i,2)       = 0.0_rDef
   solutionVector(i,3)       = 0.0_rDef
  END DO

  DO i=iSolveStart(1),iSolveEnd(1)
   actualSolutionVector(i,1) = SIN(0.3_rDef*pi*REAL(i-1,rDef)/REAL(nPts-1,rDef)) &
                             - COS(0.8_rDef*pi*REAL(i-1,rDef)/REAL(nPts-1,rDef))
   actualSolutionVector(i,2) = SIN(2.0_rDef*pi*REAL(i-1,rDef)/REAL(nPts-1,rDef)) &
                             + COS(3.0_rDef*pi*REAL(i-1,rDef)/REAL(nPts-1,rDef))
   actualSolutionVector(i,3) = SIN(pi*REAL(i-1,rDef)/REAL(nPts-1,rDef)) &
                             + COS(0.2_rDef*pi*REAL(i-1,rDef)/REAL(nPts-1,rDef))
  END DO

! generate the rhs vector...

  i=iSolveStart(1)

  rhsVector(i,1) = (diagonal(i,1,1)*actualSolutionVector(i,1)  &
                   +diagonal(i,1,2)*actualSolutionVector(i,2)  &
                   +diagonal(i,1,3)*actualSolutionVector(i,3)) &
                  +(upperDiagonal(i,1,1)*actualSolutionVector(i+1,1)  &
                   +upperDiagonal(i,1,2)*actualSolutionVector(i+1,2)  &
                   +upperDiagonal(i,1,3)*actualSolutionVector(i+1,3))

  rhsVector(i,2) = (diagonal(i,2,1)*actualSolutionVector(i,1)  &
                   +diagonal(i,2,2)*actualSolutionVector(i,2)  &
                   +diagonal(i,2,3)*actualSolutionVector(i,3)) &
                  +(upperDiagonal(i,2,1)*actualSolutionVector(i+1,1)  &
                   +upperDiagonal(i,2,2)*actualSolutionVector(i+1,2)  &
                   +upperDiagonal(i,2,3)*actualSolutionVector(i+1,3))

  rhsVector(i,3) = (diagonal(i,3,1)*actualSolutionVector(i,1)  &
                   +diagonal(i,3,2)*actualSolutionVector(i,2)  &
                   +diagonal(i,3,3)*actualSolutionVector(i,3)) &
                  +(upperDiagonal(i,3,1)*actualSolutionVector(i+1,1)  &
                   +upperDiagonal(i,3,2)*actualSolutionVector(i+1,2)  &
                   +upperDiagonal(i,3,3)*actualSolutionVector(i+1,3))

  DO i=iSolveStart(1)+1,iSolveEnd(1)-1
   rhsVector(i,1) = (diagonal(i,1,1)*actualSolutionVector(i,1)  &
                    +diagonal(i,1,2)*actualSolutionVector(i,2)  &
                    +diagonal(i,1,3)*actualSolutionVector(i,3)) &
                   +(lowerDiagonal(i,1,1)*actualSolutionVector(i-1,1)  &
                    +lowerDiagonal(i,1,2)*actualSolutionVector(i-1,2)  &
                    +lowerDiagonal(i,1,3)*actualSolutionVector(i-1,3)) &
                   +(upperDiagonal(i,1,1)*actualSolutionVector(i+1,1)  &
                    +upperDiagonal(i,1,2)*actualSolutionVector(i+1,2)  &
                    +upperDiagonal(i,1,3)*actualSolutionVector(i+1,3))
   rhsVector(i,2) = (diagonal(i,2,1)*actualSolutionVector(i,1)  &
                    +diagonal(i,2,2)*actualSolutionVector(i,2)  &
                    +diagonal(i,2,3)*actualSolutionVector(i,3)) &
                   +(lowerDiagonal(i,2,1)*actualSolutionVector(i-1,1)  &
                    +lowerDiagonal(i,2,2)*actualSolutionVector(i-1,2)  &
                    +lowerDiagonal(i,2,3)*actualSolutionVector(i-1,3)) &
                   +(upperDiagonal(i,2,1)*actualSolutionVector(i+1,1)  &
                    +upperDiagonal(i,2,2)*actualSolutionVector(i+1,2)  &
                    +upperDiagonal(i,2,3)*actualSolutionVector(i+1,3))
   rhsVector(i,3) = (diagonal(i,3,1)*actualSolutionVector(i,1)  &
                    +diagonal(i,3,2)*actualSolutionVector(i,2)  &
                    +diagonal(i,3,3)*actualSolutionVector(i,3)) &
                   +(lowerDiagonal(i,3,1)*actualSolutionVector(i-1,1)  &
                    +lowerDiagonal(i,3,2)*actualSolutionVector(i-1,2)  &
                    +lowerDiagonal(i,3,3)*actualSolutionVector(i-1,3)) &
                   +(upperDiagonal(i,3,1)*actualSolutionVector(i+1,1)  &
                    +upperDiagonal(i,3,2)*actualSolutionVector(i+1,2)  &
                    +upperDiagonal(i,3,3)*actualSolutionVector(i+1,3))
  END DO

  i=iSolveEnd(1)
  rhsVector(i,1) = (diagonal(i,1,1)*actualSolutionVector(i,1)  &
                   +diagonal(i,1,2)*actualSolutionVector(i,2)  &
                   +diagonal(i,1,3)*actualSolutionVector(i,3)) &
                  +(lowerDiagonal(i,1,1)*actualSolutionVector(i-1,1)  &
                   +lowerDiagonal(i,1,2)*actualSolutionVector(i-1,2)  &
                   +lowerDiagonal(i,1,3)*actualSolutionVector(i-1,3))
  rhsVector(i,2) = (diagonal(i,2,1)*actualSolutionVector(i,1)  &
                   +diagonal(i,2,2)*actualSolutionVector(i,2)  &
                   +diagonal(i,2,3)*actualSolutionVector(i,3)) &
                  +(lowerDiagonal(i,2,1)*actualSolutionVector(i-1,1)  &
                   +lowerDiagonal(i,2,2)*actualSolutionVector(i-1,2)  &
                   +lowerDiagonal(i,2,3)*actualSolutionVector(i-1,3))
  rhsVector(i,3) = (diagonal(i,3,1)*actualSolutionVector(i,1)  &
                   +diagonal(i,3,2)*actualSolutionVector(i,2)  &
                   +diagonal(i,3,3)*actualSolutionVector(i,3)) &
                  +(lowerDiagonal(i,3,1)*actualSolutionVector(i-1,1)  &
                   +lowerDiagonal(i,3,2)*actualSolutionVector(i-1,2)  &
                   +lowerDiagonal(i,3,3)*actualSolutionVector(i-1,3))

!--------error test:  put in an inappropriate solveDimension-----------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check the solveDimension error test.'
  ELSE
   CONTINUE
  END IF

  solveDimension = 2

! call the solve

  CALL SolveBlockTridi(object                  = blockTridiSolverObject,  &
                       solveDimension          = solveDimension,          &
                       iSolveStart             = iSolveStart,             &
                       iSolveEnd               = iSolveEnd,               &
                       lowerDiagonalArrayStart = lowerDiagonalArrayStart, &
                       lowerDiagonalArray      = lowerDiagonal,           &
                       diagonalArrayStart      = diagonalArrayStart,      &
                       diagonalArray           = diagonal,                &
                       upperDiagonalArrayStart = upperDiagonalArrayStart, &
                       upperDiagonalArray      = upperDiagonal,           &
                       rhsArrayStart           = rhsArrayStart,           &
                       rhsArray                = rhsVector,               &
                       solutionArrayStart      = solutionArrayStart,      &
                       solutionArray           = solutionVector,          &
                       enableChecking          = enableChecking,          &
                       errorInfoObject         = errorInfoObject)

  errorExpected = .TRUE.

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
    IF (masterNode) WRITE(0,*) 'Test PASSED.'
   END IF
  END IF

!-------------actual test------------------------------------------
  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check that blockTridiSolver works correctly.'
  ELSE
   CONTINUE
  END IF

!--------now use the correct solveDimension----------------------------

  solveDimension = 1
  errorExpected = .FALSE.

! call the solve

  CALL SolveBlockTridi(object                  = blockTridiSolverObject,  &
                       solveDimension          = solveDimension,          &
                       iSolveStart             = iSolveStart,             &
                       iSolveEnd               = iSolveEnd,               &
                       lowerDiagonalArrayStart = lowerDiagonalArrayStart, &
                       lowerDiagonalArray      = lowerDiagonal,           &
                       diagonalArrayStart      = diagonalArrayStart,      &
                       diagonalArray           = diagonal,                &
                       upperDiagonalArrayStart = upperDiagonalArrayStart, &
                       upperDiagonalArray      = upperDiagonal,           &
                       rhsArrayStart           = rhsArrayStart,           &
                       rhsArray                = rhsVector,               &
                       solutionArrayStart      = solutionArrayStart,      &
                       solutionArray           = solutionVector,          &
                       enableChecking          = enableChecking,          &
                       errorInfoObject         = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'errorExpected test PASSED.'
  END IF

  l2 = 0.0_rDef
  DO i=iSolveStart(1),iSolveEnd(1)
   DO m=1,numberOfVariables
    del = solutionVector(i,m) - actualSolutionVector(i,m)
!    IF (masterNode) &
!     WRITE(180,*) i,m,  &
!                        solutionVector(i,m), &
!                        actualSolutionVector(i,m), &
!                        del
    err2 = del*del
    l2 = l2 + err2
   END DO
  END DO

  l2 = SQRT(l2/REAL(nPts*numberOfVariables,rDef))

  IF (l2 > errorTolerance) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED.'
   WRITE(charStringObject%charString,'(a,e13.7)') &
       'Test FAILED: l2 error = ',l2
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED. l2 = ',l2
   CONTINUE ! all is well
  END IF


!-------------------------------------destroy object test-----------------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: destroy object. No error is expected'
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

! destroy the object

  CALL DestroyObject(object          = blockTridiSolverObject, &
                     enableChecking  = enableChecking,      &
                     errorInfoObject = errorInfoObject)

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
END SUBROUTINE BlockTridiSolverClassTest1D

END MODULE BlockTridiSolverClassTest_1D
