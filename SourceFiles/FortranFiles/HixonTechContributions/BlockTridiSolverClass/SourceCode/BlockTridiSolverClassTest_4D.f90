MODULE BlockTridiSolverClassTest_4D
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE BlockTridiSolverClass
  USE ErrorInformationClass
  USE MessagePassingInterface
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: BlockTridiSolverClassTest4D

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE BlockTridiSolverClassTest_4D: '

INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE BlockTridiSolverClassTest4D(blockTridiSolverObject, &
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

  INTEGER, PARAMETER :: numberOfMatrixDimensions = 4,  &
                        numberOfVariables        = 3,  &
                        nPtsISolve               = 21, &
                        nPtsJSolve               = 31, &
                        nPtsKSolve               = 15, &
                        nPtsLSolve               = 11

  INTEGER, DIMENSION(4), PARAMETER ::                                  &
     iSolveStart1            = (/ 2, 3, 5, 4 /),                       &
     iSolveEnd1              = (/ iSolveStart1(1) + nPtsISolve - 1,    &
                                  iSolveStart1(2) + nPtsJSolve - 1,    &
                                  iSolveStart1(3) + nPtsKSolve - 1,    &
                                  iSolveStart1(4) + nPtsLSolve - 1 /), &
     lowerDiagonalArrayStart1= (/ iSolveStart1(1),                     &
                                  iSolveStart1(2),                     &
                                  iSolveStart1(3),                     &
                                  iSolveStart1(4) /),                  &
     diagonalArrayStart1     = (/ iSolveStart1(1),                     &
                                  iSolveStart1(2),                     &
                                  iSolveStart1(3),                     &
                                  iSolveStart1(4) /),                  &
     upperDiagonalArrayStart1= (/ iSolveStart1(1),                     &
                                  iSolveStart1(2),                     &
                                  iSolveStart1(3),                     &
                                  iSolveStart1(4) /),                  &
     rhsArrayStart1          = (/ iSolveStart1(1),                     &
                                  iSolveStart1(2),                     &
                                  iSolveStart1(3),                     &
                                  iSolveStart1(4) /),                  &
     solutionArrayStart1     = (/ 1, 1, 1, 1 /)

  INTEGER, PARAMETER :: nPtsITot = iSolveStart1(1)+nPtsISolve-1 + 1, &
                        nPtsJTot = iSolveStart1(2)+nPtsJSolve-1 + 1, &
                        nPtsKTot = iSolveStart1(3)+nPtsKSolve-1 + 1, &
                        nPtsLTot = iSolveStart1(4)+nPtsLSolve-1 + 1

  LOGICAL :: passedTest,    &
             errorExpected, &
             errorFound

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE BlockTridiSolverClassTest4D'

  INTEGER :: solveDimension

  REAL(KIND=rDef), DIMENSION(iSolveStart1(1):iSolveEnd1(1), &
                             iSolveStart1(2):iSolveEnd1(2), &
                             iSolveStart1(3):iSolveEnd1(3), &
                             iSolveStart1(4):iSolveEnd1(4), &
                             numberOfVariables,             &  
                             numberOfVariables) ::          &
                   lowerDiagonal1,                          &
                   diagonal1,                               &
                   upperDiagonal1 

  REAL(KIND=rDef), DIMENSION(1:nPtsITot, &
                             1:nPtsJTot, &
                             1:nPtsKTot, &
                             1:nPtsLTot, &
                             numberOfVariables) ::         &
                   solutionVector1,                        &
                   actualSolutionVector1

  REAL(KIND=rDef), DIMENSION(iSolveStart1(1):iSolveEnd1(1), &
                             iSolveStart1(2):iSolveEnd1(2), &
                             iSolveStart1(3):iSolveEnd1(3), &
                             iSolveStart1(4):iSolveEnd1(4), &
                             numberOfVariables) ::        &
                   rhsVector1

  INTEGER :: i, j, k, l, m

  REAL(KIND=rDef) :: fac1,fac2,fac3,pi,del,err2,l2

  CONTINUE ! execution begins here

  pi = 4.0_rDef*ATAN(1.0_rDef)

  IF (masterNode) THEN
   WRITE(0,98) 
   WRITE(0,99) 
   WRITE(0,*) 'Beginning ',location
   WRITE(0,*) 'I direction: ',iSolveStart1(1),iSolveEnd1(1),nPtsITot
   WRITE(0,*) 'J direction: ',iSolveStart1(2),iSolveEnd1(2),nPtsJTot
   WRITE(0,*) 'K direction: ',iSolveStart1(3),iSolveEnd1(3),nPtsKTot
   WRITE(0,*) 'L direction: ',iSolveStart1(4),iSolveEnd1(4),nPtsLTot
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
   passedTest = .FALSE.
   IF (errorFound) THEN ! do not clobber the error
    charStringObject%charString = 'Test FAILED: error reported.'
    GO TO 101
   ELSE
    charStringObject%charString = 'Test FAILED: error reported.'
    GO TO 100
   END IF
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED.'
  END IF

  IF (CheckForGlobalError(errorInfoObject)) THEN
   charStringObject%charString = 'Error reported.'
   GO TO 100
  END IF

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: create a 4D blockTridiSolverObject.'
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
   passedTest = .FALSE.
   IF (errorFound) THEN ! do not clobber the error
    charStringObject%charString = 'Test FAILED: error reported.'
    GO TO 101
   ELSE
    charStringObject%charString = 'Test FAILED: error reported.'
    GO TO 100
   END IF
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

   DO l=iSolveStart1(4),iSolveEnd1(4)
    DO k=iSolveStart1(3),iSolveEnd1(3)
     DO j=iSolveStart1(2),iSolveEnd1(2)
      DO i=iSolveStart1(1),iSolveEnd1(1)
       fac1 = REAL(i+j+k+l,rDef)
       fac3 = REAL(iSolveEnd1(1) &
                  +iSolveEnd1(2) &
                  +iSolveEnd1(3) &
                  +iSolveEnd1(4) &
                  +3-i-j-k-l,rDef)
!      fac1 = REAL(i,rDef)
!      fac3 = REAL(iSolveEnd1(1)+1-i,rDef)
       fac2 = MAX(fac1,fac3)
!   lower diagonal matrix (row, column)
       lowerDiagonal1(i,j,k,l,1,1) = 1.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,1,2) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,1,3) = 2.0_rDef*fac1
   
       lowerDiagonal1(i,j,k,l,2,1) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,2,2) = 1.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,2,3) = 2.0_rDef*fac1
   
       lowerDiagonal1(i,j,k,l,3,1) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,3,2) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,3,3) = 1.0_rDef*fac1
!   diagonal matrix (row, column)
       diagonal1(i,j,k,l,1,1) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,1,2) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,1,3) =-1.0_rDef*fac2

       diagonal1(i,j,k,l,2,1) =-1.0_rDef*fac2
       diagonal1(i,j,k,l,2,2) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,2,3) = 1.0_rDef*fac2

       diagonal1(i,j,k,l,3,1) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,3,2) =-1.0_rDef*fac2
       diagonal1(i,j,k,l,3,3) = 1.0_rDef*fac2
!   upper diagonal matrix (row, column)
       upperDiagonal1(i,j,k,l,1,1) = 1.0_rDef*fac3
       upperDiagonal1(i,j,k,l,1,2) = 2.0_rDef*fac3
       upperDiagonal1(i,j,k,l,1,3) = 4.0_rDef*fac3

       upperDiagonal1(i,j,k,l,2,1) = 4.0_rDef*fac3
       upperDiagonal1(i,j,k,l,2,2) = 1.0_rDef*fac3
       upperDiagonal1(i,j,k,l,2,3) = 2.0_rDef*fac3
   
       upperDiagonal1(i,j,k,l,3,1) = 2.0_rDef*fac3
       upperDiagonal1(i,j,k,l,3,2) = 4.0_rDef*fac3
       upperDiagonal1(i,j,k,l,3,3) = 1.0_rDef*fac3
      END DO
     END DO
    END DO
   END DO

!   generate the actual solution vector

   DO l=1,nPtsLTot
    DO k=1,nPtsKTot
     DO j=1,nPtsJTot
      DO i=1,nPtsITot
       actualSolutionVector1(i,j,k,l,1) = 0.0_rDef
       actualSolutionVector1(i,j,k,l,2) = 0.0_rDef
       actualSolutionVector1(i,j,k,l,3) = 0.0_rDef
       solutionVector1(i,j,k,l,1)       = 0.0_rDef
       solutionVector1(i,j,k,l,2)       = 0.0_rDef
       solutionVector1(i,j,k,l,3)       = 0.0_rDef
      END DO
     END DO
    END DO
   END DO

   DO l=iSolveStart1(4),iSolveEnd1(4)
    DO k=iSolveStart1(3),iSolveEnd1(3)
     DO j=iSolveStart1(2),iSolveEnd1(2)
      DO i=iSolveStart1(1),iSolveEnd1(1)
       actualSolutionVector1(i,j,k,l,1) = SIN(0.3_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - COS(0.8_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + SIN(0.4_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - COS(0.7_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + SIN(0.5_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - COS(0.6_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + SIN(0.9_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        - COS(0.3_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
       actualSolutionVector1(i,j,k,l,2) = SIN(2.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + COS(3.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - SIN(4.0_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + COS(5.0_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - SIN(6.0_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + COS(7.0_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - SIN(8.0_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        + COS(9.0_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
       actualSolutionVector1(i,j,k,l,3) = SIN(1.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + COS(0.2_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - SIN(0.1_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + COS(0.3_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - SIN(0.2_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + COS(0.4_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - SIN(0.3_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        + COS(0.5_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
      END DO
     END DO
    END DO
   END DO

! generate the rhs vector...

  DO l=iSolveStart1(4),iSolveEnd1(4)
   DO k=iSolveStart1(3),iSolveEnd1(3)
    DO j=iSolveStart1(2),iSolveEnd1(2)
     i=iSolveStart1(1)

     rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i+1,j,k,l,1)  &
                             +upperDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i+1,j,k,l,2)  &
                             +upperDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i+1,j,k,l,3))

     rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i+1,j,k,l,1)  &
                             +upperDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i+1,j,k,l,2)  &
                             +upperDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i+1,j,k,l,3))

     rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i+1,j,k,l,1)  &
                             +upperDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i+1,j,k,l,2)  &
                             +upperDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i+1,j,k,l,3))

     DO i=iSolveStart1(1)+1,iSolveEnd1(1)-1
      rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i-1,j,k,l,1)  &
                              +lowerDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i-1,j,k,l,2)  &
                              +lowerDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i-1,j,k,l,3)) &
                             +(upperDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i+1,j,k,l,1)  &
                              +upperDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i+1,j,k,l,2)  &
                              +upperDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i+1,j,k,l,3))
      rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i-1,j,k,l,1)  &
                              +lowerDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i-1,j,k,l,2)  &
                              +lowerDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i-1,j,k,l,3)) &
                             +(upperDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i+1,j,k,l,1)  &
                              +upperDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i+1,j,k,l,2)  &
                              +upperDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i+1,j,k,l,3))
      rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i-1,j,k,l,1)  &
                              +lowerDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i-1,j,k,l,2)  &
                              +lowerDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i-1,j,k,l,3)) &
                             +(upperDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i+1,j,k,l,1)  &
                              +upperDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i+1,j,k,l,2)  &
                              +upperDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i+1,j,k,l,3))
     END DO

     i=iSolveEnd1(1)
     rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i-1,j,k,l,1)  &
                             +lowerDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i-1,j,k,l,2)  &
                             +lowerDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i-1,j,k,l,3))
     rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i-1,j,k,l,1)  &
                             +lowerDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i-1,j,k,l,2)  &
                             +lowerDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i-1,j,k,l,3))
     rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i-1,j,k,l,1)  &
                             +lowerDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i-1,j,k,l,2)  &
                             +lowerDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i-1,j,k,l,3))
  
    END DO ! j loop
   END DO ! k loop
  END DO ! l loop

!--------error test:  put in an inappropriate solveDimension-----------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check the solveDimension error test.'
  ELSE
   CONTINUE
  END IF

  solveDimension = 5

! call the solve

  CALL SolveBlockTridi(object                  = blockTridiSolverObject,   &
                       solveDimension          = solveDimension,           &
                       iSolveStart             = iSolveStart1,             &
                       iSolveEnd               = iSolveEnd1,               &
                       lowerDiagonalArrayStart = lowerDiagonalArrayStart1, &
                       lowerDiagonalArray      = lowerDiagonal1,           &
                       diagonalArrayStart      = diagonalArrayStart1,      &
                       diagonalArray           = diagonal1,                &
                       upperDiagonalArrayStart = upperDiagonalArrayStart1, &
                       upperDiagonalArray      = upperDiagonal1,           &
                       rhsArrayStart           = rhsArrayStart1,           &
                       rhsArray                = rhsVector1,               &
                       solutionArrayStart      = solutionArrayStart1,      &
                       solutionArray           = solutionVector1,          &
                       enableChecking          = enableChecking,           &
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
   WRITE(0,*) ' Test: check that blockTridiSolver works correctly (1).'
  ELSE
   CONTINUE
  END IF

!--------now use the correct solveDimension----------------------------

  solveDimension = 1
  errorExpected = .FALSE.

! call the solve

  CALL SolveBlockTridi(object                  = blockTridiSolverObject,   &
                       solveDimension          = solveDimension,           &
                       iSolveStart             = iSolveStart1,             &
                       iSolveEnd               = iSolveEnd1,               &
                       lowerDiagonalArrayStart = lowerDiagonalArrayStart1, &
                       lowerDiagonalArray      = lowerDiagonal1,           &
                       diagonalArrayStart      = diagonalArrayStart1,      &
                       diagonalArray           = diagonal1,                &
                       upperDiagonalArrayStart = upperDiagonalArrayStart1, &
                       upperDiagonalArray      = upperDiagonal1,           &
                       rhsArrayStart           = rhsArrayStart1,           &
                       rhsArray                = rhsVector1,               &
                       solutionArrayStart      = solutionArrayStart1,      &
                       solutionArray           = solutionVector1,          &
                       enableChecking          = enableChecking,           &
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
  DO l=iSolveStart1(4),iSolveEnd1(4)
   DO k=iSolveStart1(3),iSolveEnd1(3)
    DO j=iSolveStart1(2),iSolveEnd1(2)
     DO i=iSolveStart1(1),iSolveEnd1(1)
      DO m=1,numberOfVariables
       del = solutionVector1(i,j,k,l,m) - actualSolutionVector1(i,j,k,l,m)
!     IF (masterNode) &
!      WRITE(181,*) i,j,m,  &
!                         rhsVector1(i,j,m), &
!                         solutionVector1(i,j,m), &
!                         actualSolutionVector1(i,j,m), &
!                         del
       err2 = del*del
       l2 = l2 + err2
      END DO
     END DO
    END DO
   END DO
  END DO

  l2 = SQRT(l2/REAL(nPtsISolve &
                   *nPtsJSolve &
                   *nPtsKSolve &
                   *nPtsLSolve &
                   *numberOfVariables,rDef))

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

!---------now do the eta direction---------------------

!-----------regenerate the test data------------------------

   DO l=iSolveStart1(4),iSolveEnd1(4)
    DO k=iSolveStart1(3),iSolveEnd1(3)
     DO j=iSolveStart1(2),iSolveEnd1(2)
      DO i=iSolveStart1(1),iSolveEnd1(1)
       fac1 = REAL(i+j+k+l,rDef)
       fac3 = REAL(iSolveEnd1(1) &
                  +iSolveEnd1(2) &
                  +iSolveEnd1(3) &
                  +iSolveEnd1(4) &
                  +3-i-j-k-l,rDef)
!      fac1 = REAL(i,rDef)
!      fac3 = REAL(iSolveEnd1(1)+1-i,rDef)
       fac2 = MAX(fac1,fac3)
!   lower diagonal matrix (row, column)
       lowerDiagonal1(i,j,k,l,1,1) = 1.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,1,2) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,1,3) = 2.0_rDef*fac1
   
       lowerDiagonal1(i,j,k,l,2,1) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,2,2) = 1.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,2,3) = 2.0_rDef*fac1
   
       lowerDiagonal1(i,j,k,l,3,1) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,3,2) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,3,3) = 1.0_rDef*fac1
!   diagonal matrix (row, column)
       diagonal1(i,j,k,l,1,1) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,1,2) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,1,3) =-1.0_rDef*fac2

       diagonal1(i,j,k,l,2,1) =-1.0_rDef*fac2
       diagonal1(i,j,k,l,2,2) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,2,3) = 1.0_rDef*fac2

       diagonal1(i,j,k,l,3,1) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,3,2) =-1.0_rDef*fac2
       diagonal1(i,j,k,l,3,3) = 1.0_rDef*fac2
!   upper diagonal matrix (row, column)
       upperDiagonal1(i,j,k,l,1,1) = 1.0_rDef*fac3
       upperDiagonal1(i,j,k,l,1,2) = 2.0_rDef*fac3
       upperDiagonal1(i,j,k,l,1,3) = 4.0_rDef*fac3

       upperDiagonal1(i,j,k,l,2,1) = 4.0_rDef*fac3
       upperDiagonal1(i,j,k,l,2,2) = 1.0_rDef*fac3
       upperDiagonal1(i,j,k,l,2,3) = 2.0_rDef*fac3
   
       upperDiagonal1(i,j,k,l,3,1) = 2.0_rDef*fac3
       upperDiagonal1(i,j,k,l,3,2) = 4.0_rDef*fac3
       upperDiagonal1(i,j,k,l,3,3) = 1.0_rDef*fac3
      END DO
     END DO
    END DO
   END DO

!   generate the actual solution vector

   DO l=1,nPtsLTot
    DO k=1,nPtsKTot
     DO j=1,nPtsJTot
      DO i=1,nPtsITot
       actualSolutionVector1(i,j,k,l,1) = 0.0_rDef
       actualSolutionVector1(i,j,k,l,2) = 0.0_rDef
       actualSolutionVector1(i,j,k,l,3) = 0.0_rDef
       solutionVector1(i,j,k,l,1)       = 0.0_rDef
       solutionVector1(i,j,k,l,2)       = 0.0_rDef
       solutionVector1(i,j,k,l,3)       = 0.0_rDef
      END DO
     END DO
    END DO
   END DO

   DO l=iSolveStart1(4),iSolveEnd1(4)
    DO k=iSolveStart1(3),iSolveEnd1(3)
     DO j=iSolveStart1(2),iSolveEnd1(2)
      DO i=iSolveStart1(1),iSolveEnd1(1)
       actualSolutionVector1(i,j,k,l,1) = SIN(0.3_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - COS(0.8_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + SIN(0.4_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - COS(0.7_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + SIN(0.5_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - COS(0.6_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + SIN(0.9_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        - COS(0.3_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
       actualSolutionVector1(i,j,k,l,2) = SIN(2.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + COS(3.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - SIN(4.0_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + COS(5.0_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - SIN(6.0_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + COS(7.0_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - SIN(8.0_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        + COS(9.0_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
       actualSolutionVector1(i,j,k,l,3) = SIN(1.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + COS(0.2_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - SIN(0.1_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + COS(0.3_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - SIN(0.2_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + COS(0.4_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - SIN(0.3_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        + COS(0.5_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
      END DO
     END DO
    END DO
   END DO

! generate the rhs vector...

  DO l=iSolveStart1(4),iSolveEnd1(4)
   DO k=iSolveStart1(3),iSolveEnd1(3)
    j=iSolveStart1(2)
    DO i=iSolveStart1(1),iSolveEnd1(1)

     rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j+1,k,l,1)  &
                             +upperDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j+1,k,l,2)  &
                             +upperDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j+1,k,l,3))

     rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j+1,k,l,1)  &
                             +upperDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j+1,k,l,2)  &
                             +upperDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j+1,k,l,3))

     rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j+1,k,l,1)  &
                             +upperDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j+1,k,l,2)  &
                             +upperDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j+1,k,l,3))
    END DO ! I loop

    DO j=iSolveStart1(2)+1,iSolveEnd1(2)-1
     DO i=iSolveStart1(1),iSolveEnd1(1)
      rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j-1,k,l,1)  &
                              +lowerDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j-1,k,l,2)  &
                              +lowerDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j-1,k,l,3)) &
                             +(upperDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j+1,k,l,1)  &
                              +upperDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j+1,k,l,2)  &
                              +upperDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j+1,k,l,3))
      rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j-1,k,l,1)  &
                              +lowerDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j-1,k,l,2)  &
                              +lowerDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j-1,k,l,3)) &
                             +(upperDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j+1,k,l,1)  &
                              +upperDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j+1,k,l,2)  &
                              +upperDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j+1,k,l,3))
      rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j-1,k,l,1)  &
                              +lowerDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j-1,k,l,2)  &
                              +lowerDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j-1,k,l,3)) &
                             +(upperDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j+1,k,l,1)  &
                              +upperDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j+1,k,l,2)  &
                              +upperDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j+1,k,l,3))
     END DO
    END DO

    j=iSolveEnd1(2)
    DO i=iSolveStart1(1),iSolveEnd1(1)
     rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j-1,k,l,1)  &
                             +lowerDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j-1,k,l,2)  &
                             +lowerDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j-1,k,l,3))
     rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j-1,k,l,1)  &
                             +lowerDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j-1,k,l,2)  &
                             +lowerDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j-1,k,l,3))
     rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j-1,k,l,1)  &
                             +lowerDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j-1,k,l,2)  &
                             +lowerDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j-1,k,l,3))
  
    END DO ! i loop
   END DO ! k loop
  END DO ! l loop

!-------------actual test------------------------------------------
  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check that blockTridiSolver works correctly (2).'
  ELSE
   CONTINUE
  END IF

!--------now use the correct solveDimension----------------------------

  solveDimension = 2
  errorExpected = .FALSE.

! call the solve

  CALL SolveBlockTridi(object                  = blockTridiSolverObject,   &
                       solveDimension          = solveDimension,           &
                       iSolveStart             = iSolveStart1,             &
                       iSolveEnd               = iSolveEnd1,               &
                       lowerDiagonalArrayStart = lowerDiagonalArrayStart1, &
                       lowerDiagonalArray      = lowerDiagonal1,           &
                       diagonalArrayStart      = diagonalArrayStart1,      &
                       diagonalArray           = diagonal1,                &
                       upperDiagonalArrayStart = upperDiagonalArrayStart1, &
                       upperDiagonalArray      = upperDiagonal1,           &
                       rhsArrayStart           = rhsArrayStart1,           &
                       rhsArray                = rhsVector1,               &
                       solutionArrayStart      = solutionArrayStart1,      &
                       solutionArray           = solutionVector1,          &
                       enableChecking          = enableChecking,           &
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
  DO l=iSolveStart1(4),iSolveEnd1(4)
   DO k=iSolveStart1(3),iSolveEnd1(3)
    DO j=iSolveStart1(2),iSolveEnd1(2)
     DO i=iSolveStart1(1),iSolveEnd1(1)
      DO m=1,numberOfVariables
       del = solutionVector1(i,j,k,l,m) - actualSolutionVector1(i,j,k,l,m)
!     IF (masterNode) &
!      WRITE(181,*) i,j,m,  &
!                         rhsVector1(i,j,m), &
!                         solutionVector1(i,j,m), &
!                         actualSolutionVector1(i,j,m), &
!                         del
       err2 = del*del
       l2 = l2 + err2
      END DO
     END DO
    END DO
   END DO
  END DO

  l2 = SQRT(l2/REAL(nPtsISolve &
                   *nPtsJSolve &
                   *nPtsKSolve &
                   *nPtsLSolve &
                   *numberOfVariables,rDef))

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

!---------now do the zeta direction---------------------

!-----------regenerate the test data------------------------

   DO l=iSolveStart1(4),iSolveEnd1(4)
    DO k=iSolveStart1(3),iSolveEnd1(3)
     DO j=iSolveStart1(2),iSolveEnd1(2)
      DO i=iSolveStart1(1),iSolveEnd1(1)
       fac1 = REAL(i+j+k+l,rDef)
       fac3 = REAL(iSolveEnd1(1) &
                  +iSolveEnd1(2) &
                  +iSolveEnd1(3) &
                  +iSolveEnd1(4) &
                  +3-i-j-k-l,rDef)
!      fac1 = REAL(i,rDef)
!      fac3 = REAL(iSolveEnd1(1)+1-i,rDef)
       fac2 = MAX(fac1,fac3)
!   lower diagonal matrix (row, column)
       lowerDiagonal1(i,j,k,l,1,1) = 1.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,1,2) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,1,3) = 2.0_rDef*fac1
   
       lowerDiagonal1(i,j,k,l,2,1) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,2,2) = 1.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,2,3) = 2.0_rDef*fac1
   
       lowerDiagonal1(i,j,k,l,3,1) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,3,2) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,3,3) = 1.0_rDef*fac1
!   diagonal matrix (row, column)
       diagonal1(i,j,k,l,1,1) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,1,2) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,1,3) =-1.0_rDef*fac2

       diagonal1(i,j,k,l,2,1) =-1.0_rDef*fac2
       diagonal1(i,j,k,l,2,2) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,2,3) = 1.0_rDef*fac2

       diagonal1(i,j,k,l,3,1) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,3,2) =-1.0_rDef*fac2
       diagonal1(i,j,k,l,3,3) = 1.0_rDef*fac2
!   upper diagonal matrix (row, column)
       upperDiagonal1(i,j,k,l,1,1) = 1.0_rDef*fac3
       upperDiagonal1(i,j,k,l,1,2) = 2.0_rDef*fac3
       upperDiagonal1(i,j,k,l,1,3) = 4.0_rDef*fac3

       upperDiagonal1(i,j,k,l,2,1) = 4.0_rDef*fac3
       upperDiagonal1(i,j,k,l,2,2) = 1.0_rDef*fac3
       upperDiagonal1(i,j,k,l,2,3) = 2.0_rDef*fac3
   
       upperDiagonal1(i,j,k,l,3,1) = 2.0_rDef*fac3
       upperDiagonal1(i,j,k,l,3,2) = 4.0_rDef*fac3
       upperDiagonal1(i,j,k,l,3,3) = 1.0_rDef*fac3
      END DO
     END DO
    END DO
   END DO

!   generate the actual solution vector

   DO l=1,nPtsLTot
    DO k=1,nPtsKTot
     DO j=1,nPtsJTot
      DO i=1,nPtsITot
       actualSolutionVector1(i,j,k,l,1) = 0.0_rDef
       actualSolutionVector1(i,j,k,l,2) = 0.0_rDef
       actualSolutionVector1(i,j,k,l,3) = 0.0_rDef
       solutionVector1(i,j,k,l,1)       = 0.0_rDef
       solutionVector1(i,j,k,l,2)       = 0.0_rDef
       solutionVector1(i,j,k,l,3)       = 0.0_rDef
      END DO
     END DO
    END DO
   END DO

   DO l=iSolveStart1(4),iSolveEnd1(4)
    DO k=iSolveStart1(3),iSolveEnd1(3)
     DO j=iSolveStart1(2),iSolveEnd1(2)
      DO i=iSolveStart1(1),iSolveEnd1(1)
       actualSolutionVector1(i,j,k,l,1) = SIN(0.3_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - COS(0.8_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + SIN(0.4_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - COS(0.7_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + SIN(0.5_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - COS(0.6_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + SIN(0.9_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        - COS(0.3_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
       actualSolutionVector1(i,j,k,l,2) = SIN(2.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + COS(3.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - SIN(4.0_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + COS(5.0_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - SIN(6.0_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + COS(7.0_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - SIN(8.0_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        + COS(9.0_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
       actualSolutionVector1(i,j,k,l,3) = SIN(1.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + COS(0.2_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - SIN(0.1_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + COS(0.3_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - SIN(0.2_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + COS(0.4_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - SIN(0.3_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        + COS(0.5_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
      END DO
     END DO
    END DO
   END DO

! generate the rhs vector...

  DO l=iSolveStart1(4),iSolveEnd1(4)
   k=iSolveStart1(3)
   DO j=iSolveStart1(2),iSolveEnd1(2)
    DO i=iSolveStart1(1),iSolveEnd1(1)

     rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k+1,l,1)  &
                             +upperDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k+1,l,2)  &
                             +upperDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k+1,l,3))

     rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k+1,l,1)  &
                             +upperDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k+1,l,2)  &
                             +upperDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k+1,l,3))

     rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k+1,l,1)  &
                             +upperDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k+1,l,2)  &
                             +upperDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k+1,l,3))
    END DO ! I loop
   END DO  ! J loop

   DO k=iSolveStart1(3)+1,iSolveEnd1(3)-1
    DO j=iSolveStart1(2),iSolveEnd1(2)
     DO i=iSolveStart1(1),iSolveEnd1(1)
      rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k-1,l,1)  &
                              +lowerDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k-1,l,2)  &
                              +lowerDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k-1,l,3)) &
                             +(upperDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k+1,l,1)  &
                              +upperDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k+1,l,2)  &
                              +upperDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k+1,l,3))
      rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k-1,l,1)  &
                              +lowerDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k-1,l,2)  &
                              +lowerDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k-1,l,3)) &
                             +(upperDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k+1,l,1)  &
                              +upperDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k+1,l,2)  &
                              +upperDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k+1,l,3))
      rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k-1,l,1)  &
                              +lowerDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k-1,l,2)  &
                              +lowerDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k-1,l,3)) &
                             +(upperDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k+1,l,1)  &
                              +upperDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k+1,l,2)  &
                              +upperDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k+1,l,3))
     END DO
    END DO
   END DO

   k=iSolveEnd1(3)
   DO j=iSolveStart1(2),iSolveEnd1(2)
    DO i=iSolveStart1(1),iSolveEnd1(1)
     rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k-1,l,1)  &
                             +lowerDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k-1,l,2)  &
                             +lowerDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k-1,l,3))
     rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k-1,l,1)  &
                             +lowerDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k-1,l,2)  &
                             +lowerDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k-1,l,3))
     rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k-1,l,1)  &
                             +lowerDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k-1,l,2)  &
                             +lowerDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k-1,l,3))
  
    END DO ! i loop
   END DO ! j loop
  END DO ! l loop

!-------------actual test------------------------------------------
  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check that blockTridiSolver works correctly (3).'
  ELSE
   CONTINUE
  END IF

!--------now use the correct solveDimension----------------------------

  solveDimension = 3
  errorExpected = .FALSE.

! call the solve

  CALL SolveBlockTridi(object                  = blockTridiSolverObject,   &
                       solveDimension          = solveDimension,           &
                       iSolveStart             = iSolveStart1,             &
                       iSolveEnd               = iSolveEnd1,               &
                       lowerDiagonalArrayStart = lowerDiagonalArrayStart1, &
                       lowerDiagonalArray      = lowerDiagonal1,           &
                       diagonalArrayStart      = diagonalArrayStart1,      &
                       diagonalArray           = diagonal1,                &
                       upperDiagonalArrayStart = upperDiagonalArrayStart1, &
                       upperDiagonalArray      = upperDiagonal1,           &
                       rhsArrayStart           = rhsArrayStart1,           &
                       rhsArray                = rhsVector1,               &
                       solutionArrayStart      = solutionArrayStart1,      &
                       solutionArray           = solutionVector1,          &
                       enableChecking          = enableChecking,           &
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
  DO l=iSolveStart1(4),iSolveEnd1(4)
   DO k=iSolveStart1(3),iSolveEnd1(3)
    DO j=iSolveStart1(2),iSolveEnd1(2)
     DO i=iSolveStart1(1),iSolveEnd1(1)
      DO m=1,numberOfVariables
       del = solutionVector1(i,j,k,l,m) - actualSolutionVector1(i,j,k,l,m)
!     IF (masterNode) &
!      WRITE(181,*) i,j,m,  &
!                         rhsVector1(i,j,m), &
!                         solutionVector1(i,j,m), &
!                         actualSolutionVector1(i,j,m), &
!                         del
       err2 = del*del
       l2 = l2 + err2
      END DO
     END DO
    END DO
   END DO
  END DO

  l2 = SQRT(l2/REAL(nPtsISolve &
                   *nPtsJSolve &
                   *nPtsKSolve &
                   *nPtsLSolve &
                   *numberOfVariables,rDef))

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

!---------now do the tau direction---------------------

!-----------regenerate the test data------------------------

   DO l=iSolveStart1(4),iSolveEnd1(4)
    DO k=iSolveStart1(3),iSolveEnd1(3)
     DO j=iSolveStart1(2),iSolveEnd1(2)
      DO i=iSolveStart1(1),iSolveEnd1(1)
       fac1 = REAL(i+j+k+l,rDef)
       fac3 = REAL(iSolveEnd1(1) &
                  +iSolveEnd1(2) &
                  +iSolveEnd1(3) &
                  +iSolveEnd1(4) &
                  +3-i-j-k-l,rDef)
!      fac1 = REAL(i,rDef)
!      fac3 = REAL(iSolveEnd1(1)+1-i,rDef)
       fac2 = MAX(fac1,fac3)
!   lower diagonal matrix (row, column)
       lowerDiagonal1(i,j,k,l,1,1) = 1.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,1,2) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,1,3) = 2.0_rDef*fac1
   
       lowerDiagonal1(i,j,k,l,2,1) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,2,2) = 1.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,2,3) = 2.0_rDef*fac1
   
       lowerDiagonal1(i,j,k,l,3,1) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,3,2) = 2.0_rDef*fac1
       lowerDiagonal1(i,j,k,l,3,3) = 1.0_rDef*fac1
!   diagonal matrix (row, column)
       diagonal1(i,j,k,l,1,1) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,1,2) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,1,3) =-1.0_rDef*fac2

       diagonal1(i,j,k,l,2,1) =-1.0_rDef*fac2
       diagonal1(i,j,k,l,2,2) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,2,3) = 1.0_rDef*fac2

       diagonal1(i,j,k,l,3,1) = 1.0_rDef*fac2
       diagonal1(i,j,k,l,3,2) =-1.0_rDef*fac2
       diagonal1(i,j,k,l,3,3) = 1.0_rDef*fac2
!   upper diagonal matrix (row, column)
       upperDiagonal1(i,j,k,l,1,1) = 1.0_rDef*fac3
       upperDiagonal1(i,j,k,l,1,2) = 2.0_rDef*fac3
       upperDiagonal1(i,j,k,l,1,3) = 4.0_rDef*fac3

       upperDiagonal1(i,j,k,l,2,1) = 4.0_rDef*fac3
       upperDiagonal1(i,j,k,l,2,2) = 1.0_rDef*fac3
       upperDiagonal1(i,j,k,l,2,3) = 2.0_rDef*fac3
   
       upperDiagonal1(i,j,k,l,3,1) = 2.0_rDef*fac3
       upperDiagonal1(i,j,k,l,3,2) = 4.0_rDef*fac3
       upperDiagonal1(i,j,k,l,3,3) = 1.0_rDef*fac3
      END DO
     END DO
    END DO
   END DO

!   generate the actual solution vector

   DO l=1,nPtsLTot
    DO k=1,nPtsKTot
     DO j=1,nPtsJTot
      DO i=1,nPtsITot
       actualSolutionVector1(i,j,k,l,1) = 0.0_rDef
       actualSolutionVector1(i,j,k,l,2) = 0.0_rDef
       actualSolutionVector1(i,j,k,l,3) = 0.0_rDef
       solutionVector1(i,j,k,l,1)       = 0.0_rDef
       solutionVector1(i,j,k,l,2)       = 0.0_rDef
       solutionVector1(i,j,k,l,3)       = 0.0_rDef
      END DO
     END DO
    END DO
   END DO

   DO l=iSolveStart1(4),iSolveEnd1(4)
    DO k=iSolveStart1(3),iSolveEnd1(3)
     DO j=iSolveStart1(2),iSolveEnd1(2)
      DO i=iSolveStart1(1),iSolveEnd1(1)
       actualSolutionVector1(i,j,k,l,1) = SIN(0.3_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - COS(0.8_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + SIN(0.4_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - COS(0.7_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + SIN(0.5_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - COS(0.6_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + SIN(0.9_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        - COS(0.3_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
       actualSolutionVector1(i,j,k,l,2) = SIN(2.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + COS(3.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - SIN(4.0_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + COS(5.0_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - SIN(6.0_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + COS(7.0_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - SIN(8.0_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        + COS(9.0_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
       actualSolutionVector1(i,j,k,l,3) = SIN(1.0_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        + COS(0.2_rDef*pi*REAL(i-1,rDef)/REAL(nPtsITot-1,rDef)) &
                                        - SIN(0.1_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        + COS(0.3_rDef*pi*REAL(j-1,rDef)/REAL(nPtsJTot-1,rDef)) &
                                        - SIN(0.2_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        + COS(0.4_rDef*pi*REAL(k-1,rDef)/REAL(nPtsKTot-1,rDef)) &
                                        - SIN(0.3_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef)) &
                                        + COS(0.5_rDef*pi*REAL(l-1,rDef)/REAL(nPtsLTot-1,rDef))
      END DO
     END DO
    END DO
   END DO

! generate the rhs vector...

  l=iSolveStart1(4)
  DO k=iSolveStart1(3),iSolveEnd1(3)
   DO j=iSolveStart1(2),iSolveEnd1(2)
    DO i=iSolveStart1(1),iSolveEnd1(1)

     rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l+1,1)  &
                             +upperDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l+1,2)  &
                             +upperDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l+1,3))

     rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l+1,1)  &
                             +upperDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l+1,2)  &
                             +upperDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l+1,3))

     rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(upperDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l+1,1)  &
                             +upperDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l+1,2)  &
                             +upperDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l+1,3))
    END DO ! I loop
   END DO  ! J loop
  END DO   ! K loop

  DO l=iSolveStart1(4)+1,iSolveEnd1(4)-1
   DO k=iSolveStart1(3),iSolveEnd1(3)
    DO j=iSolveStart1(2),iSolveEnd1(2)
     DO i=iSolveStart1(1),iSolveEnd1(1)
      rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l-1,1)  &
                              +lowerDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l-1,2)  &
                              +lowerDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l-1,3)) &
                             +(upperDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l+1,1)  &
                              +upperDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l+1,2)  &
                              +upperDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l+1,3))
      rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l-1,1)  &
                              +lowerDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l-1,2)  &
                              +lowerDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l-1,3)) &
                             +(upperDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l+1,1)  &
                              +upperDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l+1,2)  &
                              +upperDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l+1,3))
      rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                              +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                              +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                             +(lowerDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l-1,1)  &
                              +lowerDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l-1,2)  &
                              +lowerDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l-1,3)) &
                             +(upperDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l+1,1)  &
                              +upperDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l+1,2)  &
                              +upperDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l+1,3))
     END DO
    END DO
   END DO
  END DO

  l=iSolveEnd1(4)
  DO k=iSolveStart1(3),iSolveEnd1(3)
   DO j=iSolveStart1(2),iSolveEnd1(2)
    DO i=iSolveStart1(1),iSolveEnd1(1)
     rhsVector1(i,j,k,l,1) = (diagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,1,1)*actualSolutionVector1(i,j,k,l-1,1)  &
                             +lowerDiagonal1(i,j,k,l,1,2)*actualSolutionVector1(i,j,k,l-1,2)  &
                             +lowerDiagonal1(i,j,k,l,1,3)*actualSolutionVector1(i,j,k,l-1,3))
     rhsVector1(i,j,k,l,2) = (diagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,2,1)*actualSolutionVector1(i,j,k,l-1,1)  &
                             +lowerDiagonal1(i,j,k,l,2,2)*actualSolutionVector1(i,j,k,l-1,2)  &
                             +lowerDiagonal1(i,j,k,l,2,3)*actualSolutionVector1(i,j,k,l-1,3))
     rhsVector1(i,j,k,l,3) = (diagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l,1)  &
                             +diagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l,2)  &
                             +diagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l,3)) &
                            +(lowerDiagonal1(i,j,k,l,3,1)*actualSolutionVector1(i,j,k,l-1,1)  &
                             +lowerDiagonal1(i,j,k,l,3,2)*actualSolutionVector1(i,j,k,l-1,2)  &
                             +lowerDiagonal1(i,j,k,l,3,3)*actualSolutionVector1(i,j,k,l-1,3))
  
    END DO ! i loop
   END DO ! j loop
  END DO ! l loop

!-------------actual test------------------------------------------
  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Test: check that blockTridiSolver works correctly (4).'
  ELSE
   CONTINUE
  END IF

!--------now use the correct solveDimension----------------------------

  solveDimension = 4
  errorExpected = .FALSE.

! call the solve

  CALL SolveBlockTridi(object                  = blockTridiSolverObject,   &
                       solveDimension          = solveDimension,           &
                       iSolveStart             = iSolveStart1,             &
                       iSolveEnd               = iSolveEnd1,               &
                       lowerDiagonalArrayStart = lowerDiagonalArrayStart1, &
                       lowerDiagonalArray      = lowerDiagonal1,           &
                       diagonalArrayStart      = diagonalArrayStart1,      &
                       diagonalArray           = diagonal1,                &
                       upperDiagonalArrayStart = upperDiagonalArrayStart1, &
                       upperDiagonalArray      = upperDiagonal1,           &
                       rhsArrayStart           = rhsArrayStart1,           &
                       rhsArray                = rhsVector1,               &
                       solutionArrayStart      = solutionArrayStart1,      &
                       solutionArray           = solutionVector1,          &
                       enableChecking          = enableChecking,           &
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
  DO l=iSolveStart1(4),iSolveEnd1(4)
   DO k=iSolveStart1(3),iSolveEnd1(3)
    DO j=iSolveStart1(2),iSolveEnd1(2)
     DO i=iSolveStart1(1),iSolveEnd1(1)
      DO m=1,numberOfVariables
       del = solutionVector1(i,j,k,l,m) - actualSolutionVector1(i,j,k,l,m)
!     IF (masterNode) &
!      WRITE(181,*) i,j,m,  &
!                         rhsVector1(i,j,m), &
!                         solutionVector1(i,j,m), &
!                         actualSolutionVector1(i,j,m), &
!                         del
       err2 = del*del
       l2 = l2 + err2
      END DO
     END DO
    END DO
   END DO
  END DO

  l2 = SQRT(l2/REAL(nPtsISolve &
                   *nPtsJSolve &
                   *nPtsKSolve &
                   *nPtsLSolve &
                   *numberOfVariables,rDef))

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
END SUBROUTINE BlockTridiSolverClassTest4D

END MODULE BlockTridiSolverClassTest_4D
