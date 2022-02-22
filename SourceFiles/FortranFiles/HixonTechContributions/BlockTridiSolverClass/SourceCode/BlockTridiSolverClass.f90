MODULE BlockTridiSolverClass

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   USE ErrorInformationClass
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: BlockTridiSolverType, &
             ObjectIsInitialized,  &
             CreateObject,         &
             DestroyObject,        &
             SolveBlockTridi

INTERFACE ObjectIsInitialized
  MODULE PROCEDURE BlockTridiSolverObjectIsInitialized
END INTERFACE

INTERFACE CreateObject
  MODULE PROCEDURE CreateBlockTridiSolverObject
END INTERFACE CreateObject

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyBlockTridiSolverObject
END INTERFACE DestroyObject

INTERFACE SolveBlockTridi
  MODULE PROCEDURE SolveBlockTridi1D
  MODULE PROCEDURE SolveBlockTridi2D
  MODULE PROCEDURE SolveBlockTridi3D
  MODULE PROCEDURE SolveBlockTridi4D
END INTERFACE SolveBlockTridi

TYPE BlockTridiSolverType
  PRIVATE
  LOGICAL :: isInitialized = .FALSE.
  INTEGER :: numberOfDimensions = -1, &
             numberOfVariables  = -1
END TYPE BlockTridiSolverType

! module error data

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE BlockTridiSolverClass: '

! define floating point precision

INTEGER, PARAMETER :: rDef = REAL64

! define keywords for topologyFileTypes

! mapping parameters

INTEGER, PARAMETER :: minNumberOfMatrixDimensions    =  1, &
                      maxNumberOfMatrixDimensions    =  4, & ! for the moment...
                      minNumberOfVariables           =  1

CONTAINS

LOGICAL FUNCTION BlockTridiSolverObjectIsInitialized(object)
  TYPE(BlockTridiSolverType) :: object

  BlockTridiSolverObjectIsInitialized = object%isInitialized
  RETURN
END FUNCTION BlockTridiSolverObjectIsInitialized


SUBROUTINE CreateBlockTridiSolverObject(object,                   &
                                        numberOfMatrixDimensions, &
                                        numberOfVariables,        &
                                        enableChecking,           &
                                        errorInfoObject)

  TYPE(BlockTridiSolverType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: numberOfMatrixDimensions, &
                         numberOfVariables
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateBlockTridiSolverObject'

  CONTINUE ! execution begins here

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    charStringObject%charString = 'ERROR: object is already initialized.'
    GO TO 100
   ELSE IF (numberOfMatrixDimensions < minNumberOfMatrixDimensions) THEN
    WRITE(charStringObject%charString,'(a,i2)')               &
        ' is smaller than the minimum number of matrix dimensions ', &
        minNumberOfMatrixDimensions
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    WRITE(charStringObject%charString,'(a,i2)')               &
        'Error: specified number of matrix dimensions ',     &
        numberOfMatrixDimensions

    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    GO TO 101
   ELSE IF (numberOfMatrixDimensions > maxNumberOfMatrixDimensions) THEN
    WRITE(charStringObject%charString,'(a,i2)')               &
        ' is greater than the maximum number of matrix dimensions ', &
        maxNumberOfMatrixDimensions

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    WRITE(charStringObject%charString,'(a,i2)')               &
        'Error: specified number of matrix dimensions ',     &
        numberOfMatrixDimensions

    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    GO TO 101
   ELSE IF (numberOfVariables < minNumberOfVariables) THEN
    WRITE(charStringObject%charString,'(a,i2)')               &
        ' is smaller than the minimum number of variables ', &
        minNumberOfVariables
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    WRITE(charStringObject%charString,'(a,i2)')               &
        'Error: specified number of variables ',     &
        numberOfVariables

    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE ! all is well
   END IF
  ELSE
   CONTINUE ! no checking
  END IF

  object%numberOfDimensions = numberOfMatrixDimensions
  object%numberOfVariables  = numberOfVariables

  object%isInitialized = .TRUE.

  RETURN

100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  GO TO 101
101 CONTINUE
  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE CreateBlockTridiSolverObject

SUBROUTINE DestroyBlockTridiSolverObject(object,                   &
                                         enableChecking,           &
                                         errorInfoObject)

  TYPE(BlockTridiSolverType), INTENT(INOUT) :: object
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyBlockTridiSolverObject'

  CONTINUE ! execution begins here

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'ERROR: object is not initialized.'
    GO TO 100
   END IF
  ELSE
   CONTINUE ! no checking
  END IF

  object%numberOfDimensions = -1
  object%numberOfVariables  = -1

  object%isInitialized = .FALSE.

  RETURN

100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  GO TO 101
101 CONTINUE
  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE DestroyBlockTridiSolverObject

SUBROUTINE SolveBlockTridi1D(object,                   &
                             solveDimension,           &
                             iSolveStart,              &
                             iSolveEnd,                &
                             lowerDiagonalArrayStart,  &
                             lowerDiagonalArray,       &  ! A
                             diagonalArrayStart,       &  !  
                             diagonalArray,            &  ! B
                             upperDiagonalArrayStart,  &  !  
                             upperDiagonalArray,       &  ! C
                             rhsArrayStart,            &  !  
                             rhsArray,                 &  ! R
                             solutionArrayStart,       &  !  
                             solutionArray,            &  ! X
                             enableChecking,           &
                             errorInfoObject)

  TYPE(BlockTridiSolverType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: solveDimension
  INTEGER, DIMENSION(:), INTENT(IN) :: iSolveStart,             & ! length 1
                                       iSolveEnd,               &
                                       lowerDiagonalArrayStart, &
                                       diagonalArrayStart,      &
                                       upperDiagonalArrayStart, &
                                       rhsArrayStart,           &
                                       solutionArrayStart

  REAL(KIND=rDef), DIMENSION(lowerDiagonalArrayStart(1):,:,:),  &
                   INTENT(INOUT) :: lowerDiagonalArray
  REAL(KIND=rDef), DIMENSION(diagonalArrayStart(1):,:,:),       &
                   INTENT(INOUT) :: diagonalArray
  REAL(KIND=rDef), DIMENSION(upperDiagonalArrayStart(1):,:,:),  &
                   INTENT(INOUT) :: upperDiagonalArray
  REAL(KIND=rDef), DIMENSION(rhsArrayStart(1):,:),              &
                   INTENT(INOUT) :: rhsArray
  REAL(KIND=rDef), DIMENSION(solutionArrayStart(1):,:),         &
                   INTENT(INOUT) :: solutionArray

  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: numVar

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SolveBlockTridi1D'

! LOGICAL, PARAMETER :: useBLKTRI11 = .TRUE.

  CONTINUE ! execution begins here

  numVar  = SIZE(rhsArray,2)

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'ERROR: object is not initialized.'
    GO TO 100
   END IF
! check matrix dimensions
   IF (object%numberOfDimensions /= 1) THEN
    WRITE(charStringObject%charString,'(a,i2,a)')  &
        'ERROR: expected a matrix of dimension ',  &
        object%numberOfDimensions,           &
        ' but found matrix dimension of 1.'
    GO TO 100
   ELSE
    CONTINUE
   END IF
! check number of variables
   IF (object%numberOfVariables /= numVar) THEN
    WRITE(charStringObject%charString,'(a,i2,a,i2)')  &
        'ERROR: expected ',                           &
        object%numberOfVariables,                     &
        ' variables, but found ',                     &
        numVar
    GO TO 100
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE ! no checking
  END IF

! CALLing BLKTRI11

  IF (solveDimension == 1) THEN
   CALL BLKTRI11(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE 
   WRITE(charStringObject%charString,'(a,i5)') &
    'ERROR: solveDimension must be 1; found ',solveDimension
   GO TO 100
  END IF
  RETURN

100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  GO TO 101
101 CONTINUE
  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE SolveBlockTridi1D

include 'BlockTridiSolver1D.f90'

SUBROUTINE SolveBlockTridi2D(object,                   &
                             solveDimension,           & ! 1 or 2
                             iSolveStart,              &
                             iSolveEnd,                &
                             lowerDiagonalArrayStart,  &
                             lowerDiagonalArray,       &  ! A
                             diagonalArrayStart,       &  !  
                             diagonalArray,            &  ! B
                             upperDiagonalArrayStart,  &  !  
                             upperDiagonalArray,       &  ! C
                             rhsArrayStart,            &  !  
                             rhsArray,                 &  ! R
                             solutionArrayStart,       &  !  
                             solutionArray,            &  ! X
                             enableChecking,           &
                             errorInfoObject)

  TYPE(BlockTridiSolverType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: solveDimension
  INTEGER, DIMENSION(:), INTENT(IN) :: iSolveStart,             & ! length 2
                                       iSolveEnd,               &
                                       lowerDiagonalArrayStart, &
                                       diagonalArrayStart,      &
                                       upperDiagonalArrayStart, &
                                       rhsArrayStart,           &
                                       solutionArrayStart

  REAL(KIND=rDef), DIMENSION(lowerDiagonalArrayStart(1):,      &
                             lowerDiagonalArrayStart(2):,:,:), &
                   INTENT(INOUT) :: lowerDiagonalArray
  REAL(KIND=rDef), DIMENSION(diagonalArrayStart(1):,            &
                             diagonalArrayStart(2):,:,:),       &
                   INTENT(INOUT) :: diagonalArray
  REAL(KIND=rDef), DIMENSION(upperDiagonalArrayStart(1):,       &
                             upperDiagonalArrayStart(2):,:,:),  &
                   INTENT(INOUT) :: upperDiagonalArray
  REAL(KIND=rDef), DIMENSION(rhsArrayStart(1):,                 &
                             rhsArrayStart(2):,:),              &
                   INTENT(INOUT) :: rhsArray
  REAL(KIND=rDef), DIMENSION(solutionArrayStart(1):,            &
                             solutionArrayStart(2):,:),         &
                   INTENT(INOUT) :: solutionArray

  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: numVar

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SolveBlockTridi2D'

! LOGICAL, PARAMETER :: useBLKTRI11 = .TRUE.

  CONTINUE ! execution begins here

  numVar  = SIZE(rhsArray,3)

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'ERROR: object is not initialized.'
    GO TO 100
   END IF
! check matrix dimensions
   IF (object%numberOfDimensions /= 2) THEN
    WRITE(charStringObject%charString,'(a,i2,a)')  &
        'ERROR: expected a matrix of dimension ',  &
        object%numberOfDimensions,           &
        ' but found matrix dimension of 2.'
    GO TO 100
   ELSE
    CONTINUE
   END IF
! check number of variables
   IF (object%numberOfVariables /= numVar) THEN
    WRITE(charStringObject%charString,'(a,i2,a,i2)')  &
        'ERROR: expected ',                           &
        object%numberOfVariables,                     &
        ' variables, but found ',                     &
        numVar
    GO TO 100
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE ! no checking
  END IF

  IF (solveDimension == 1) THEN

! CALLing BLKTRI21

   CALL BLKTRI21(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE IF (solveDimension == 2) THEN

! CALLing BLKTRI22

   CALL BLKTRI22(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE 
   WRITE(charStringObject%charString,'(a,i5)') &
    'ERROR: solveDimension must be 1-2; found ',solveDimension
   GO TO 100
  END IF

  RETURN

100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  GO TO 101
101 CONTINUE
  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE SolveBlockTridi2D

include 'BlockTridiSolver2D.f90'

SUBROUTINE SolveBlockTridi3D(object,                   &
                             solveDimension,           & ! 1-3 
                             iSolveStart,              &
                             iSolveEnd,                &
                             lowerDiagonalArrayStart,  &
                             lowerDiagonalArray,       &  ! A
                             diagonalArrayStart,       &  !  
                             diagonalArray,            &  ! B
                             upperDiagonalArrayStart,  &  !  
                             upperDiagonalArray,       &  ! C
                             rhsArrayStart,            &  !  
                             rhsArray,                 &  ! R
                             solutionArrayStart,       &  !  
                             solutionArray,            &  ! X
                             enableChecking,           &
                             errorInfoObject)

  TYPE(BlockTridiSolverType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: solveDimension
  INTEGER, DIMENSION(:), INTENT(IN) :: iSolveStart,             & ! length 3
                                       iSolveEnd,               &
                                       lowerDiagonalArrayStart, &
                                       diagonalArrayStart,      &
                                       upperDiagonalArrayStart, &
                                       rhsArrayStart,           &
                                       solutionArrayStart

  REAL(KIND=rDef), DIMENSION(lowerDiagonalArrayStart(1):,      &
                             lowerDiagonalArrayStart(2):,      &
                             lowerDiagonalArrayStart(3):,:,:), &
                   INTENT(INOUT) :: lowerDiagonalArray
  REAL(KIND=rDef), DIMENSION(diagonalArrayStart(1):,            &
                             diagonalArrayStart(2):,            &
                             diagonalArrayStart(3):,:,:),       &
                   INTENT(INOUT) :: diagonalArray
  REAL(KIND=rDef), DIMENSION(upperDiagonalArrayStart(1):,       &
                             upperDiagonalArrayStart(2):,       &
                             upperDiagonalArrayStart(3):,:,:),  &
                   INTENT(INOUT) :: upperDiagonalArray
  REAL(KIND=rDef), DIMENSION(rhsArrayStart(1):,                 &
                             rhsArrayStart(2):,                 &
                             rhsArrayStart(3):,:),              &
                   INTENT(INOUT) :: rhsArray
  REAL(KIND=rDef), DIMENSION(solutionArrayStart(1):,            &
                             solutionArrayStart(2):,            &
                             solutionArrayStart(3):,:),         &
                   INTENT(INOUT) :: solutionArray

  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: numVar

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SolveBlockTridi3D'

! LOGICAL, PARAMETER :: useBLKTRI11 = .TRUE.

  CONTINUE ! execution begins here

  numVar  = SIZE(rhsArray,4)

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'ERROR: object is not initialized.'
    GO TO 100
   END IF
! check matrix dimensions
   IF (object%numberOfDimensions /= 3) THEN
    WRITE(charStringObject%charString,'(a,i2,a)')  &
        'ERROR: expected a matrix of dimension ',  &
        object%numberOfDimensions,           &
        ' but found matrix dimension of 3.'
    GO TO 100
   ELSE
    CONTINUE
   END IF
! check number of variables
   IF (object%numberOfVariables /= numVar) THEN
    WRITE(charStringObject%charString,'(a,i2,a,i2)')  &
        'ERROR: expected ',                           &
        object%numberOfVariables,                     &
        ' variables, but found ',                     &
        numVar
    GO TO 100
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE ! no checking
  END IF

  IF (solveDimension == 1) THEN

! CALLing BLKTRI31

   CALL BLKTRI31(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE IF (solveDimension == 2) THEN

! CALLing BLKTRI32

   CALL BLKTRI32(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE IF (solveDimension == 3) THEN

! CALLing BLKTRI33

   CALL BLKTRI33(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE 
   WRITE(charStringObject%charString,'(a,i5)') &
    'ERROR: solveDimension must be 1-3; found ',solveDimension
   GO TO 100
  END IF

  RETURN

100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  GO TO 101
101 CONTINUE
  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE SolveBlockTridi3D

include 'BlockTridiSolver3D.f90'

SUBROUTINE SolveBlockTridi4D(object,                   &
                             solveDimension,           & ! 1-4 
                             iSolveStart,              &
                             iSolveEnd,                &
                             lowerDiagonalArrayStart,  &
                             lowerDiagonalArray,       &  ! A
                             diagonalArrayStart,       &  !  
                             diagonalArray,            &  ! B
                             upperDiagonalArrayStart,  &  !  
                             upperDiagonalArray,       &  ! C
                             rhsArrayStart,            &  !  
                             rhsArray,                 &  ! R
                             solutionArrayStart,       &  !  
                             solutionArray,            &  ! X
                             enableChecking,           &
                             errorInfoObject)

  TYPE(BlockTridiSolverType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: solveDimension
  INTEGER, DIMENSION(:), INTENT(IN) :: iSolveStart,             & ! length 4
                                       iSolveEnd,               &
                                       lowerDiagonalArrayStart, &
                                       diagonalArrayStart,      &
                                       upperDiagonalArrayStart, &
                                       rhsArrayStart,           &
                                       solutionArrayStart

  REAL(KIND=rDef), DIMENSION(lowerDiagonalArrayStart(1):,      &
                             lowerDiagonalArrayStart(2):,      &
                             lowerDiagonalArrayStart(3):,      &
                             lowerDiagonalArrayStart(4):,:,:), &
                   INTENT(INOUT) :: lowerDiagonalArray
  REAL(KIND=rDef), DIMENSION(diagonalArrayStart(1):,            &
                             diagonalArrayStart(2):,            &
                             diagonalArrayStart(3):,            &
                             diagonalArrayStart(4):,:,:),       &
                   INTENT(INOUT) :: diagonalArray
  REAL(KIND=rDef), DIMENSION(upperDiagonalArrayStart(1):,       &
                             upperDiagonalArrayStart(2):,       &
                             upperDiagonalArrayStart(3):,       &
                             upperDiagonalArrayStart(4):,:,:),  &
                   INTENT(INOUT) :: upperDiagonalArray
  REAL(KIND=rDef), DIMENSION(rhsArrayStart(1):,                 &
                             rhsArrayStart(2):,                 &
                             rhsArrayStart(3):,                 &
                             rhsArrayStart(4):,:),              &
                   INTENT(INOUT) :: rhsArray
  REAL(KIND=rDef), DIMENSION(solutionArrayStart(1):,            &
                             solutionArrayStart(2):,            &
                             solutionArrayStart(3):,            &
                             solutionArrayStart(4):,:),         &
                   INTENT(INOUT) :: solutionArray

  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  INTEGER :: numVar

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE SolveBlockTridi4D'

! LOGICAL, PARAMETER :: useBLKTRI11 = .TRUE.

  CONTINUE ! execution begins here

  numVar  = SIZE(rhsArray,5)

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'ERROR: object is not initialized.'
    GO TO 100
   END IF
! check matrix dimensions
   IF (object%numberOfDimensions /= 4) THEN
    WRITE(charStringObject%charString,'(a,i2,a)')  &
        'ERROR: expected a matrix of dimension ',  &
        object%numberOfDimensions,           &
        ' but found matrix dimension of 4.'
    GO TO 100
   ELSE
    CONTINUE
   END IF
! check number of variables
   IF (object%numberOfVariables /= numVar) THEN
    WRITE(charStringObject%charString,'(a,i2,a,i2)')  &
        'ERROR: expected ',                           &
        object%numberOfVariables,                     &
        ' variables, but found ',                     &
        numVar
    GO TO 100
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE ! no checking
  END IF

  IF (solveDimension == 1) THEN

! CALLing BLKTRI41

   CALL BLKTRI41(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE IF (solveDimension == 2) THEN

! CALLing BLKTRI42

   CALL BLKTRI42(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE IF (solveDimension == 3) THEN

! CALLing BLKTRI43

   CALL BLKTRI43(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE IF (solveDimension == 4) THEN

! CALLing BLKTRI44

   CALL BLKTRI44(numVar      = numVar,                  &
                 iSolveStart = iSolveStart,             &
                 iSolveEnd   = iSolveEnd,               &
                 aStart      = lowerDiagonalArrayStart, &
                 A           = lowerDiagonalArray,      &
                 bStart      = diagonalArrayStart,      &
                 B           = diagonalArray,           &
                 cStart      = upperDiagonalArrayStart, &
                 C           = upperDiagonalArray,      &
                 xStart      = solutionArrayStart,      &
                 X           = solutionArray,           &
                 rStart      = rhsArrayStart,           &
                 R           = rhsArray)

  ELSE 
   WRITE(charStringObject%charString,'(a,i5)') &
    'ERROR: solveDimension must be 1-4; found ',solveDimension
   GO TO 100
  END IF

  RETURN

100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  GO TO 101
101 CONTINUE
  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE SolveBlockTridi4D

include 'BlockTridiSolver4D.f90'

END MODULE BlockTridiSolverClass
