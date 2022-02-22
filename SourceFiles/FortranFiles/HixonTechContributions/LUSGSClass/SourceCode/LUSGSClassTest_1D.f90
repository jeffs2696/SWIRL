MODULE LUSGSClassTest_1D
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE LUSGSClass
  USE Check2DData
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: LUSGSClassTest1D

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE LUSGSClassTest_1D: '

INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE LUSGSClassTest1D(LUSGSObject,     &
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

  INTEGER, DIMENSION(2) :: errorLocation

  INTEGER, PARAMETER :: numberOfDimensions = 1, &
                        numberOfVariables = 3, &
                        nPXi = 5

! testing data

  INTEGER, DIMENSION(numberOfDimensions+1) :: iStartRHSVector, &
                                              iStartDeltaQ
  INTEGER, DIMENSION(numberOfDimensions+2) :: iStartAMatrixXi, &
                                              iStartBMatrixXi
  INTEGER, DIMENSION(numberOfDimensions) :: iStartEpsilonXi,  &
                                            iStartDeltaSigma, &
                                            iMinUpdate,       &
                                            iMaxUpdate

  REAL(KIND=rDef), DIMENSION(numberOfVariables,nPXi) :: rhsVector,  &
                                                        deltaQ,     &
                                                        deltaQStar, &
                                                        deltaQTest
  REAL(KIND=rDef), DIMENSION(numberOfVariables,numberOfVariables,nPXi) :: aMatrixXi, &
                                                                          bMatrixXi
  REAL(KIND=rDef), DIMENSION(nPXi) :: deltaSigma, &
                                      epsilonXi



  INTEGER :: NRHS

  INTEGER, DIMENSION(numberOfVariables) :: IPIV

  INTEGER :: INFO

  REAL(KIND=rDef), DIMENSION(numberOfVariables, numberOfVariables) :: aMatrix,    &
                                                                      aMatrixIm1, &
                                                                      aMatrixIp1

  REAL(KIND=rDef), DIMENSION(numberOfVariables) :: ddDiag, &
                                                   ddIm1,  &
                                                   ddIp1

  REAL(KIND=rDef) :: rDum

  INTEGER :: i,j,nV,ii,jj

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE LUSGSClassTest1D'

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
   WRITE(0,*) ' Testing 1D Viscous LU-SGS call.'
   WRITE(0,*) '       No error is expected. '
  ELSE
   CONTINUE
  END IF

  errorExpected = .FALSE.

  CALL CreateObject(object                        = LUSGSObject,        &
                    numberOfTopologicalDimensions = numberOfDimensions, &
                    enableChecking                = enableChecking,     &
                    errorInfoObject               = errorInfoObject)

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

! generate deltaQ

  DO i=1,nPXi
   DO nV=1,numberOfVariables
    CALL RANDOM_NUMBER(deltaQ(nV,i))
    deltaQ(nV,i) = deltaQ(nV,i) + 0.5_rDef
    deltaQTest(nV,i) = deltaQ(nV,i)
   END DO
  END DO

  DO i=1,numberOfDimensions
   iStartEpsilonXi(i) = 1
   iStartDeltaSigma(i) = 1
   iMinUpdate(i) = 1
  END DO

  iMaxUpdate(1) = nPXi

  DO i=1,numberOfDimensions+1
   iStartRHSVector(i) = 1
   iStartDeltaQ(i) = 1
  END DO

  DO i=1,numberOfDimensions+2
   iStartAMatrixXi(i) = 1
   iStartBMatrixXi(i) = 1
  END DO

! fill in the data

  DO i=1,nPXi
   DO jj=1,numberOfVariables
    DO ii=1,numberOfVariables
     CALL RANDOM_NUMBER(rDum)
     aMatrixXi(ii,jj,i) = rDum + 0.01_rDef

     CALL RANDOM_NUMBER(rDum)
     bMatrixXi(ii,jj,i) = rDum + 0.03_rDef
    END DO
   END DO
   CALL RANDOM_NUMBER(rDum)
   epsilonXi(i)  = rDum + 1.0_rDef
   CALL RANDOM_NUMBER(rDum)
   deltaSigma(i) = rDum + 10.0_rDef
  END DO

  NRHS = 1

! now:  the object should be doing a +/- LU-SGS sweep...

  include 'XiTestSweep1DViscous21.f90' ! sweep2, -
  include 'XiTestSweep1DViscous11.f90' ! sweep1, +

! debug
! DO i=iMinUpdate(1),iMaxUpdate(1)
!  DO j=1,numberOfVariables
!   WRITE(0,*) 'Test: ',i,j,deltaQTest(j,i),deltaQStar(j,i),rhsVector(j,i)
!  END DO
! END DO

! call the LU-SGS viscous solver...

  CALL PerformLUSGSUpdate(object             = LUSGSObject, &
                          numberOfVariables  = numberOfVariables,   & ! number of equations
                          iStartRHSVector    = iStartRHSVector,     & ! (nD+1)
                          rhsVector          = rhsVector,           & ! (nV,nI)
                          iStartAMatrixXi    = iStartAMatrixXi,     & ! (nD+2)
                          aMatrixXi          = aMatrixXi,           & ! (nV,nV,nI)
                          iStartEpsilonXi    = iStartEpsilonXi,     & ! (nD)
                          epsilonXi          = epsilonXi,           & ! (nI)
                          iStartBMatrixXi    = iStartBMatrixXi,     & ! (nD+2)
                          bMatrixXi          = bMatrixXi,           & ! (nV,nV,nI)
                          iStartDeltaSigma   = iStartDeltaSigma,    & ! (nD)
                          deltaSigma         = deltaSigma,          & ! (nI)
                          iStartDeltaQ       = iStartDeltaQ,        & ! (nD+1)
                          deltaQ             = deltaQ,              & ! (nV,nI)
                          iMinUpdate         = iMinUpdate,          & ! (nD)
                          iMaxUpdate         = iMaxUpdate,          & ! (nD)
                          enableChecking     = enableChecking,      &
                          errorInfoObject    = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL CheckDataValue(dataValue         = deltaQ,        &
                      expectedDataValue = deltaQTest,    &
                      correctValue      = correctValue,  &
                      errorLocation     = errorLocation, &
                      errorInfoObject   = errorInfoObject)

  IF (correctValue) THEN
   CONTINUE ! all is well
  ELSE
   WRITE(charStringObject%charString,'(a,2(1x,i5))') 'Error in matrix solution at location ', &
                                                      errorLocation(1:2)
   passedTest = .FALSE.
   WRITE(0,*) 'data: ',deltaQTest(errorLocation(1),errorLocation(2)), &
                       deltaQ(errorLocation(1),errorLocation(2))
   GO TO 100
  END IF

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  IF (masterNode) WRITE(0,*) 'Test 1(Viscous +/-) PASSED.'

! now:  the object should be doing a -/+ LU-SGS sweep...

  include 'XiTestSweep1DViscous22.f90' ! sweep2, -
  include 'XiTestSweep1DViscous12.f90' ! sweep1, +

! call the LU-SGS viscous solver...

  CALL PerformLUSGSUpdate(object             = LUSGSObject, &
                          numberOfVariables  = numberOfVariables,   & ! number of equations
                          iStartRHSVector    = iStartRHSVector,     & ! (nD+1)
                          rhsVector          = rhsVector,           & ! (nV,nI)
                          iStartAMatrixXi    = iStartAMatrixXi,     & ! (nD+2)
                          aMatrixXi          = aMatrixXi,           & ! (nV,nV,nI)
                          iStartEpsilonXi    = iStartEpsilonXi,     & ! (nD)
                          epsilonXi          = epsilonXi,           & ! (nI)
                          iStartBMatrixXi    = iStartBMatrixXi,     & ! (nD+2)
                          bMatrixXi          = bMatrixXi,           & ! (nV,nV,nI)
                          iStartDeltaSigma   = iStartDeltaSigma,    & ! (nD)
                          deltaSigma         = deltaSigma,          & ! (nI)
                          iStartDeltaQ       = iStartDeltaQ,        & ! (nD+1)
                          deltaQ             = deltaQ,              & ! (nV,nI)
                          iMinUpdate         = iMinUpdate,          & ! (nD)
                          iMaxUpdate         = iMaxUpdate,          & ! (nD)
                          enableChecking     = enableChecking,      &
                          errorInfoObject    = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL CheckDataValue(dataValue         = deltaQ,        &
                      expectedDataValue = deltaQTest,    &
                      correctValue      = correctValue,  &
                      errorLocation     = errorLocation, &
                      errorInfoObject   = errorInfoObject)

  IF (correctValue) THEN
   CONTINUE ! all is well
  ELSE
   WRITE(charStringObject%charString,'(a,2(1x,i5))') 'Error in matrix solution at location ', &
                                                      errorLocation(1:2)
   passedTest = .FALSE.
   WRITE(0,*) 'data: ',deltaQTest(errorLocation(1),errorLocation(2)), &
                       deltaQ(errorLocation(1),errorLocation(2))
   GO TO 100
  END IF

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  IF (masterNode) WRITE(0,*) 'Test 2(Viscous -/+) PASSED.'

! now:  the object should be doing a +/- LU-SGS sweep...

  include 'XiTestSweep1DInviscid21.f90' ! sweep2, -
  include 'XiTestSweep1DInviscid11.f90' ! sweep1, +

! call the LU-SGS inviscid solver...

  CALL PerformLUSGSUpdate(object             = LUSGSObject, &
                          numberOfVariables  = numberOfVariables,   & ! number of equations
                          iStartRHSVector    = iStartRHSVector,     & ! (nD+1)
                          rhsVector          = rhsVector,           & ! (nV,nI)
                          iStartAMatrixXi    = iStartAMatrixXi,     & ! (nD+2)
                          aMatrixXi          = aMatrixXi,           & ! (nV,nV,nI)
                          iStartEpsilonXi    = iStartEpsilonXi,     & ! (nD)
                          epsilonXi          = epsilonXi,           & ! (nI)
                          iStartDeltaSigma   = iStartDeltaSigma,    & ! (nD)
                          deltaSigma         = deltaSigma,          & ! (nI)
                          iStartDeltaQ       = iStartDeltaQ,        & ! (nD+1)
                          deltaQ             = deltaQ,              & ! (nV,nI)
                          iMinUpdate         = iMinUpdate,          & ! (nD)
                          iMaxUpdate         = iMaxUpdate,          & ! (nD)
                          enableChecking     = enableChecking,      &
                          errorInfoObject    = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL CheckDataValue(dataValue         = deltaQ,        &
                      expectedDataValue = deltaQTest,    &
                      correctValue      = correctValue,  &
                      errorLocation     = errorLocation, &
                      errorInfoObject   = errorInfoObject)

  IF (correctValue) THEN
   CONTINUE ! all is well
  ELSE
   WRITE(charStringObject%charString,'(a,2(1x,i5))') 'Error in matrix solution at location ', &
                                                      errorLocation(1:2)
   passedTest = .FALSE.
   WRITE(0,*) 'data: ',deltaQTest(errorLocation(1),errorLocation(2)), &
                       deltaQ(errorLocation(1),errorLocation(2))
   GO TO 100
  END IF

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  IF (masterNode) WRITE(0,*) 'Test 3(Inviscid +/-) PASSED.'

! now:  the object should be doing a -/+ LU-SGS sweep...

  include 'XiTestSweep1DInviscid22.f90' ! sweep2, +
  include 'XiTestSweep1DInviscid12.f90' ! sweep1, -

! debug
! DO i=iMinUpdate(1),iMaxUpdate(1)
!  DO j=1,numberOfVariables
!   WRITE(0,*) 'Test: ',i,j,deltaQTest(j,i),deltaQStar(j,i),rhsVector(j,i)
!  END DO
! END DO

! call the LU-SGS inviscid solver...

  CALL PerformLUSGSUpdate(object             = LUSGSObject, &
                          numberOfVariables  = numberOfVariables,   & ! number of equations
                          iStartRHSVector    = iStartRHSVector,     & ! (nD+1)
                          rhsVector          = rhsVector,           & ! (nV,nI)
                          iStartAMatrixXi    = iStartAMatrixXi,     & ! (nD+2)
                          aMatrixXi          = aMatrixXi,           & ! (nV,nV,nI)
                          iStartEpsilonXi    = iStartEpsilonXi,     & ! (nD)
                          epsilonXi          = epsilonXi,           & ! (nI)
                          iStartDeltaSigma   = iStartDeltaSigma,    & ! (nD)
                          deltaSigma         = deltaSigma,          & ! (nI)
                          iStartDeltaQ       = iStartDeltaQ,        & ! (nD+1)
                          deltaQ             = deltaQ,              & ! (nV,nI)
                          iMinUpdate         = iMinUpdate,          & ! (nD)
                          iMaxUpdate         = iMaxUpdate,          & ! (nD)
                          enableChecking     = enableChecking,      &
                          errorInfoObject    = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL CheckDataValue(dataValue         = deltaQ,        &
                      expectedDataValue = deltaQTest,    &
                      correctValue      = correctValue,  &
                      errorLocation     = errorLocation, &
                      errorInfoObject   = errorInfoObject)

  IF (correctValue) THEN
   CONTINUE ! all is well
  ELSE
   WRITE(charStringObject%charString,'(a,2(1x,i5))') 'Error in matrix solution at location ', &
                                                      errorLocation(1:2)
   passedTest = .FALSE.
   WRITE(0,*) 'data: ',deltaQTest(errorLocation(1),errorLocation(2)), &
                       deltaQ(errorLocation(1),errorLocation(2))
   GO TO 100
  END IF

! did it work?

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   charStringObject%charString = 'Test FAILED: error reported.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  IF (masterNode) WRITE(0,*) 'Test 4(Inviscid -/+) PASSED.'

!141 CONTINUE

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

!---------------------try to destroy initialized object----------------------
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
END SUBROUTINE LUSGSClassTest1D

END MODULE LUSGSClassTest_1D
