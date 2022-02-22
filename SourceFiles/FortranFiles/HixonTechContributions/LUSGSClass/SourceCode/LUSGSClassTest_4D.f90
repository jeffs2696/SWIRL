MODULE LUSGSClassTest_4D
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE LUSGSClass
  USE Check5DData
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: LUSGSClassTest4D

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE LUSGSClassTest_4D: '

INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE LUSGSClassTest4D(LUSGSObject,     &
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

  INTEGER, PARAMETER :: numberOfDimensions = 4, &
                        numberOfVariables  = 3, &
                        nPXi   =  5,            &
                        nPEta  =  7,            &
                        nPZeta =  9,            &
                        nPTau  = 11

  INTEGER, DIMENSION(numberOfDimensions+1) :: errorLocation

! testing data

  INTEGER, DIMENSION(numberOfDimensions+1) :: iStartRHSVector, &
                                              iStartDeltaQ

  INTEGER, DIMENSION(numberOfDimensions+2) :: iStartAMatrixXi,   &
                                              iStartBMatrixXi,   &
                                              iStartAMatrixEta,  &
                                              iStartBMatrixEta,  &
                                              iStartAMatrixZeta, &
                                              iStartBMatrixZeta, &
                                              iStartAMatrixTau,  &
                                              iStartBMatrixTau

  INTEGER, DIMENSION(numberOfDimensions) :: iStartEpsilonXi,   &
                                            iStartEpsilonEta,  &
                                            iStartEpsilonZeta, &
                                            iStartEpsilonTau,  &
                                            iStartDeltaSigma,  &
                                            iMinUpdate,        &
                                            iMaxUpdate

  REAL(KIND=rDef), DIMENSION(numberOfVariables,nPXi,nPEta,nPZeta,nPTau) ::  &
                                                        rhsVector,          &
                                                        deltaQ,             &
                                                        deltaQStar,         &
                                                        deltaQTest
  REAL(KIND=rDef), DIMENSION(numberOfVariables, &
                             numberOfVariables, &
                             nPXi,              &
                             nPEta,             &
                             nPZeta,            &
                             nPTau)  ::         &
                                   aMatrixXi,   &
                                   bMatrixXi,   &
                                   aMatrixEta,  &
                                   bMatrixEta,  &
                                   aMatrixZeta, &
                                   bMatrixZeta, &
                                   aMatrixTau,  &
                                   bMatrixTau

  REAL(KIND=rDef), DIMENSION(nPXi,nPEta,nPZeta,nPTau) :: deltaSigma,  &
                                                         epsilonXi,   &
                                                         epsilonEta,  &
                                                         epsilonZeta, &
                                                         epsilonTau

  INTEGER :: NRHS

  INTEGER, DIMENSION(numberOfVariables) :: IPIV

  INTEGER :: INFO

  REAL(KIND=rDef), DIMENSION(numberOfVariables, numberOfVariables) :: &
                                            aMatrix,    &
                                            aMatrixIm1, &
                                            aMatrixIp1, &
                                            aMatrixJm1, &
                                            aMatrixJp1, &
                                            aMatrixKm1, &
                                            aMatrixKp1, &
                                            aMatrixLm1, &
                                            aMatrixLp1

  REAL(KIND=rDef), DIMENSION(numberOfVariables) :: ddDiag, &
                                                   ddIm1,  &
                                                   ddIp1

  REAL(KIND=rDef), DIMENSION(numberOfVariables,nPXi) ::    &
                                                   ddJm1,  &
                                                   ddJp1

  REAL(KIND=rDef), DIMENSION(numberOfVariables,nPXi,nPEta) ::  &
                                                   ddKm1,      &
                                                   ddKp1

  REAL(KIND=rDef), DIMENSION(numberOfVariables,nPXi,nPEta,nPZeta) ::  &
                                                   ddLm1,             &
                                                   ddLp1

  REAL(KIND=rDef) :: rDum

  INTEGER :: i,j,k,l,nV,ii,jj

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE LUSGSClassTest4D'

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
   WRITE(0,*) ' Testing 4D Viscous LU-SGS call.'
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

! generate deltaQ

  DO l=1,nPTau
   DO k=1,nPZeta
    DO j=1,nPEta
     DO i=1,nPXi
      DO nV=1,numberOfVariables
       CALL RANDOM_NUMBER(deltaQ(nV,i,j,k,l))
       deltaQ(nV,i,j,k,l) = deltaQ(nV,i,j,k,l) + 0.5_rDef
       deltaQTest(nV,i,j,k,l) = deltaQ(nV,i,j,k,l)
      END DO
     END DO
    END DO
   END DO
  END DO

  DO i=1,numberOfDimensions
   iStartEpsilonXi(i)   = 1
   iStartEpsilonEta(i)  = 1
   iStartEpsilonZeta(i) = 1
   iStartEpsilonTau(i)  = 1
   iStartDeltaSigma(i)  = 1
   iMinUpdate(i) = 1
  END DO

  iMaxUpdate(1) = nPXi
  iMaxUpdate(2) = nPEta
  iMaxUpdate(3) = nPZeta
  iMaxUpdate(4) = nPTau

  DO i=1,numberOfDimensions+1
   iStartRHSVector(i) = 1
   iStartDeltaQ(i) = 1
  END DO

  DO i=1,numberOfDimensions+2
   iStartAMatrixXi(i)   = 1
   iStartBMatrixXi(i)   = 1
   iStartAMatrixEta(i)  = 1
   iStartBMatrixEta(i)  = 1
   iStartAMatrixZeta(i) = 1
   iStartBMatrixZeta(i) = 1
   iStartAMatrixTau(i)  = 1
   iStartBMatrixTau(i)  = 1
  END DO

! fill in the data

  DO l=1,nPTau
   DO k=1,nPZeta
    DO j=1,nPEta
     DO i=1,nPXi
      DO jj=1,numberOfVariables
       DO ii=1,numberOfVariables
        CALL RANDOM_NUMBER(rDum)
        aMatrixXi(ii,jj,i,j,k,l) = rDum + 0.01_rDef
        CALL RANDOM_NUMBER(rDum)
        bMatrixXi(ii,jj,i,j,k,l) = rDum + 0.06_rDef
  
        CALL RANDOM_NUMBER(rDum)
        aMatrixEta(ii,jj,i,j,k,l) = rDum + 0.02_rDef
        CALL RANDOM_NUMBER(rDum)
        bMatrixEta(ii,jj,i,j,k,l) = rDum + 0.07_rDef
  
        CALL RANDOM_NUMBER(rDum)
        aMatrixZeta(ii,jj,i,j,k,l) = rDum + 0.03_rDef
        CALL RANDOM_NUMBER(rDum)
        bMatrixZeta(ii,jj,i,j,k,l) = rDum + 0.08_rDef
  
        CALL RANDOM_NUMBER(rDum)
        aMatrixTau(ii,jj,i,j,k,l) = rDum + 0.04_rDef
        CALL RANDOM_NUMBER(rDum)
        bMatrixTau(ii,jj,i,j,k,l) = rDum + 0.09_rDef
  
       END DO
      END DO
      CALL RANDOM_NUMBER(rDum)
      epsilonXi(i,j,k,l)   = rDum + 1.02_rDef
      CALL RANDOM_NUMBER(rDum)
      epsilonEta(i,j,k,l)  = rDum + 1.03_rDef
      CALL RANDOM_NUMBER(rDum)
      epsilonZeta(i,j,k,l) = rDum + 1.04_rDef
      CALL RANDOM_NUMBER(rDum)
      epsilonTau(i,j,k,l)  = rDum + 1.05_rDef
      CALL RANDOM_NUMBER(rDum)
      deltaSigma(i,j,k,l)  = rDum + 10.0_rDef
     END DO
    END DO
   END DO
  END DO

  NRHS = 1

! now:  the object should be doing a +/- LU-SGS sweep...

  include 'TauTestSweep4DViscous21.f90' ! sweep2, -
  include 'TauTestSweep4DViscous11.f90' ! sweep1, +

! debug
! DO j=iMinUpdate(2),iMaxUpdate(2)
!  DO i=iMinUpdate(1),iMaxUpdate(1)
!   DO nV=1,numberOfVariables
!    CALL RANDOM_NUMBER(deltaQ(nV,i,j))
!   END DO
!  END DO
! END DO

! call the LU-SGS viscous solver...

  CALL PerformLUSGSUpdate(object             = LUSGSObject, &
                          numberOfVariables  = numberOfVariables,   & ! number of equations
                          iStartRHSVector    = iStartRHSVector,     & ! (nD+1)
                          rhsVector          = rhsVector,           & ! (nV,nI)
                          iStartAMatrixXi    = iStartAMatrixXi,     & ! (nD+2)
                          aMatrixXi          = aMatrixXi,           & ! (nV,nV,nI)
                          iStartAMatrixEta   = iStartAMatrixEta,    & ! (nD+2)
                          aMatrixEta         = aMatrixEta,          & ! (nV,nV,nI)
                          iStartAMatrixZeta  = iStartAMatrixZeta,   & ! (nD+2)
                          aMatrixZeta        = aMatrixZeta,         & ! (nV,nV,nI)
                          iStartAMatrixTau   = iStartAMatrixTau,    & ! (nD+2)
                          aMatrixTau         = aMatrixTau,          & ! (nV,nV,nI)
                          iStartEpsilonXi    = iStartEpsilonXi,     & ! (nD)
                          epsilonXi          = epsilonXi,           & ! (nI)
                          iStartEpsilonEta   = iStartEpsilonEta,    & ! (nD)
                          epsilonEta         = epsilonEta,          & ! (nI)
                          iStartEpsilonZeta  = iStartEpsilonZeta,   & ! (nD)
                          epsilonZeta        = epsilonZeta,         & ! (nI)
                          iStartEpsilonTau   = iStartEpsilonTau,    & ! (nD)
                          epsilonTau         = epsilonTau,          & ! (nI)
                          iStartBMatrixXi    = iStartBMatrixXi,     & ! (nD+2)
                          bMatrixXi          = bMatrixXi,           & ! (nV,nV,nI)
                          iStartBMatrixEta   = iStartBMatrixEta,    & ! (nD+2)
                          bMatrixEta         = bMatrixEta,          & ! (nV,nV,nI)
                          iStartBMatrixZeta  = iStartBMatrixZeta,   & ! (nD+2)
                          bMatrixZeta        = bMatrixZeta,         & ! (nV,nV,nI)
                          iStartBMatrixTau   = iStartBMatrixTau,    & ! (nD+2)
                          bMatrixTau         = bMatrixTau,          & ! (nV,nV,nI)
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
   WRITE(charStringObject%charString,'(a,5(1x,i5))') 'Error in matrix solution at location ', &
                                                      errorLocation(1:5)
   passedTest = .FALSE.
   WRITE(0,*) 'data: ',deltaQTest(errorLocation(1), &
                                  errorLocation(2), &
                                  errorLocation(3), &
                                  errorLocation(4), &
                                  errorLocation(5)), &
                       deltaQ(errorLocation(1), &
                              errorLocation(2), &
                              errorLocation(3), &
                              errorLocation(4), &
                              errorLocation(5))
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

  IF (masterNode) WRITE(0,*) 'Test 1 (Viscous +/-) PASSED.'

! debug
! DO j=iMinUpdate(2),iMaxUpdate(2)
!  DO i=iMinUpdate(1),iMaxUpdate(1)
!   DO nV=1,numberOfVariables
!    WRITE(0,*) 'Test: ',i,j,nV,deltaQTest(nV,i,j), &
!                               deltaQ(nV,i,j)
!   END DO
!  END DO
! END DO

! now:  the object should be doing a -/+ LU-SGS sweep...

  include 'TauTestSweep4DViscous22.f90' ! sweep2, -
  include 'TauTestSweep4DViscous12.f90' ! sweep1, +

! call the LU-SGS viscous solver...

  CALL PerformLUSGSUpdate(object             = LUSGSObject, &
                          numberOfVariables  = numberOfVariables,   & ! number of equations
                          iStartRHSVector    = iStartRHSVector,     & ! (nD+1)
                          rhsVector          = rhsVector,           & ! (nV,nI)
                          iStartAMatrixXi    = iStartAMatrixXi,     & ! (nD+2)
                          aMatrixXi          = aMatrixXi,           & ! (nV,nV,nI)
                          iStartAMatrixEta   = iStartAMatrixEta,    & ! (nD+2)
                          aMatrixEta         = aMatrixEta,          & ! (nV,nV,nI)
                          iStartAMatrixZeta  = iStartAMatrixZeta,   & ! (nD+2)
                          aMatrixZeta        = aMatrixZeta,         & ! (nV,nV,nI)
                          iStartAMatrixTau   = iStartAMatrixTau,    & ! (nD+2)
                          aMatrixTau         = aMatrixTau,          & ! (nV,nV,nI)
                          iStartEpsilonXi    = iStartEpsilonXi,     & ! (nD)
                          epsilonXi          = epsilonXi,           & ! (nI)
                          iStartEpsilonEta   = iStartEpsilonEta,    & ! (nD)
                          epsilonEta         = epsilonEta,          & ! (nI)
                          iStartEpsilonZeta  = iStartEpsilonZeta,   & ! (nD)
                          epsilonZeta        = epsilonZeta,         & ! (nI)
                          iStartEpsilonTau   = iStartEpsilonTau,    & ! (nD)
                          epsilonTau         = epsilonTau,          & ! (nI)
                          iStartBMatrixXi    = iStartBMatrixXi,     & ! (nD+2)
                          bMatrixXi          = bMatrixXi,           & ! (nV,nV,nI)
                          iStartBMatrixEta   = iStartBMatrixEta,    & ! (nD+2)
                          bMatrixEta         = bMatrixEta,          & ! (nV,nV,nI)
                          iStartBMatrixZeta  = iStartBMatrixZeta,   & ! (nD+2)
                          bMatrixZeta        = bMatrixZeta,         & ! (nV,nV,nI)
                          iStartBMatrixTau   = iStartBMatrixTau,    & ! (nD+2)
                          bMatrixTau         = bMatrixTau,          & ! (nV,nV,nI)
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
   WRITE(charStringObject%charString,'(a,5(1x,i5))') 'Error in matrix solution at location ', &
                                                      errorLocation(1:5)
   passedTest = .FALSE.
   WRITE(0,*) 'data: ',deltaQTest(errorLocation(1), &
                                  errorLocation(2), &
                                  errorLocation(3), &
                                  errorLocation(4), &
                                  errorLocation(5)), &
                       deltaQ(errorLocation(1), &
                              errorLocation(2), &
                              errorLocation(3), &
                              errorLocation(4), &
                              errorLocation(5))
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

  IF (masterNode) WRITE(0,*) 'Test 2 (Viscous -/+) PASSED.'

! now:  the object should be doing a +/- LU-SGS sweep...

  include 'TauTestSweep4DInviscid21.f90' ! sweep2, -
  include 'TauTestSweep4DInviscid11.f90' ! sweep1, +

! call the LU-SGS inviscid solver...

  CALL PerformLUSGSUpdate(object             = LUSGSObject, &
                          numberOfVariables  = numberOfVariables,   & ! number of equations
                          iStartRHSVector    = iStartRHSVector,     & ! (nD+1)
                          rhsVector          = rhsVector,           & ! (nV,nI)
                          iStartAMatrixXi    = iStartAMatrixXi,     & ! (nD+2)
                          aMatrixXi          = aMatrixXi,           & ! (nV,nV,nI)
                          iStartAMatrixEta   = iStartAMatrixEta,    & ! (nD+2)
                          aMatrixEta         = aMatrixEta,          & ! (nV,nV,nI)
                          iStartAMatrixZeta  = iStartAMatrixZeta,   & ! (nD+2)
                          aMatrixZeta        = aMatrixZeta,         & ! (nV,nV,nI)
                          iStartAMatrixTau   = iStartAMatrixTau,    & ! (nD+2)
                          aMatrixTau         = aMatrixTau,          & ! (nV,nV,nI)
                          iStartEpsilonXi    = iStartEpsilonXi,     & ! (nD)
                          epsilonXi          = epsilonXi,           & ! (nI)
                          iStartEpsilonEta   = iStartEpsilonEta,    & ! (nD)
                          epsilonEta         = epsilonEta,          & ! (nI)
                          iStartEpsilonZeta  = iStartEpsilonZeta,   & ! (nD)
                          epsilonZeta        = epsilonZeta,         & ! (nI)
                          iStartEpsilonTau   = iStartEpsilonTau,    & ! (nD)
                          epsilonTau         = epsilonTau,          & ! (nI)
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
   WRITE(charStringObject%charString,'(a,5(1x,i5))') 'Error in matrix solution at location ', &
                                                      errorLocation(1:5)
   passedTest = .FALSE.
   WRITE(0,*) 'data: ',deltaQTest(errorLocation(1), &
                                  errorLocation(2), &
                                  errorLocation(3), &
                                  errorLocation(4), &
                                  errorLocation(5)), &
                       deltaQ(errorLocation(1), &
                              errorLocation(2), &
                              errorLocation(3), &
                              errorLocation(4), &
                              errorLocation(5))
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

  IF (masterNode) WRITE(0,*) 'Test 3 (Inviscid +/-) PASSED.'

! now:  the object should be doing a -/+ LU-SGS sweep...

  include 'TauTestSweep4DInviscid22.f90' ! sweep2, +
  include 'TauTestSweep4DInviscid12.f90' ! sweep1, -

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
                          iStartAMatrixEta   = iStartAMatrixEta,    & ! (nD+2)
                          aMatrixEta         = aMatrixEta,          & ! (nV,nV,nI)
                          iStartAMatrixZeta  = iStartAMatrixZeta,   & ! (nD+2)
                          aMatrixZeta        = aMatrixZeta,         & ! (nV,nV,nI)
                          iStartAMatrixTau   = iStartAMatrixTau,    & ! (nD+2)
                          aMatrixTau         = aMatrixTau,          & ! (nV,nV,nI)
                          iStartEpsilonXi    = iStartEpsilonXi,     & ! (nD)
                          epsilonXi          = epsilonXi,           & ! (nI)
                          iStartEpsilonEta   = iStartEpsilonEta,    & ! (nD)
                          epsilonEta         = epsilonEta,          & ! (nI)
                          iStartEpsilonZeta  = iStartEpsilonZeta,   & ! (nD)
                          epsilonZeta        = epsilonZeta,         & ! (nI)
                          iStartEpsilonTau   = iStartEpsilonTau,    & ! (nD)
                          epsilonTau         = epsilonTau,          & ! (nI)
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
! reset the errorInfoObject
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
   WRITE(charStringObject%charString,'(a,5(1x,i5))') 'Error in matrix solution at location ', &
                                                      errorLocation(1:5)
   passedTest = .FALSE.
   WRITE(0,*) 'data: ',deltaQTest(errorLocation(1), &
                                  errorLocation(2), &
                                  errorLocation(3), &
                                  errorLocation(4), &
                                  errorLocation(5)), &
                       deltaQ(errorLocation(1), &
                              errorLocation(2), &
                              errorLocation(3), &
                              errorLocation(4), &
                              errorLocation(5))
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

  IF (masterNode) WRITE(0,*) 'Test 4 (Inviscid -/+) PASSED.'

  GO TO 141

 141 CONTINUE

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
END SUBROUTINE LUSGSClassTest4D

END MODULE LUSGSClassTest_4D
