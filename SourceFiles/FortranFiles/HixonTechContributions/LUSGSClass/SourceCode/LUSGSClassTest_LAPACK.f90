MODULE LUSGSClassTest_LAPACK
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE LUSGSClass
  USE Check1DData
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: LUSGSClassTestLAPACK

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE LUSGSClassTest_LAPACK: '

INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE LUSGSClassTestLAPACK(LUSGSObject,     &
                                masterNode,        &
                                enableChecking,    &
                                errorInfoObject)

  TYPE(LUSGSType), INTENT(INOUT) :: LUSGSObject
  LOGICAL, INTENT(IN) :: masterNode, &
                         enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

  LOGICAL :: passedTest,    &
             errorExpected, &
             errorFound,    &
             correctValue

  INTEGER, DIMENSION(1) :: errorLocation

  INTEGER, PARAMETER :: numberOfVariables = 5

! testing data

  INTEGER :: NRHS

  INTEGER, DIMENSION(numberOfVariables) :: IPIV

  INTEGER :: INFO

  REAL(KIND=rDef), DIMENSION(numberOfVariables, numberOfVariables) :: aMatrix

  REAL(KIND=rDef), DIMENSION(numberOfVariables) :: rhsVec,      &
                                                   solVec

  INTEGER :: i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE LUSGSClassTestLAPACK'

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

!--------------------test a successful initialization------------------

  IF (masterNode) THEN
   WRITE(0,99) 
   WRITE(0,*) ' Testing LAPACK matrix inversion call.'
   WRITE(0,*) '       No error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

! construct the a matrix

  NRHS = 1

  DO j=1,numberOfVariables
   solVec(j) = 5.0_rDef + 0.8_rDef*REAL(j-1,rDef)
  END DO

  DO j=1,numberOfVariables
   DO i=1,numberOfVariables
    CALL RANDOM_NUMBER(aMatrix(i,j))
   END DO
  END DO

  rhsVec(:) = MATMUL(aMatrix,solVec)

! MATMUL call is OK.

! LAPACK call to solve the equation A X = B and return the
!  solution in rhsVec

  CALL DGESV(numberOfVariables, & ! N
             NRHS,              & ! NRHS
             aMatrix,           & ! A 
             numberOfVariables, & ! LDA
             IPIV,              & ! IPIV
             rhsVec,            & ! B
             numberOfVariables, & ! LDB
             INFO)                ! INFO

  IF (INFO /= 0) THEN
   WRITE(charStringObject%charString,'(a,i5)') &
    'ERROR returned from DGESV (1) call!  INFO = ',INFO
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL CheckDataValue(dataValue         = rhsVec,        &
                      expectedDataValue = solVec,        &
                      correctValue      = correctValue,  &
                      errorLocation     = errorLocation, &
                      errorInfoObject   = errorInfoObject)

  IF (correctValue) THEN
   CONTINUE ! all is well
  ELSE
   WRITE(charStringObject%charString,'(a,i5)') 'Error in matrix solution at location ',errorLocation
   passedTest = .FALSE.
   GO TO 100
  END IF

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
END SUBROUTINE LUSGSClassTestLAPACK

END MODULE LUSGSClassTest_LAPACK
