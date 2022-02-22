! these routines are inserted.

SUBROUTINE LUSGSInviscid4DE4D(object,              & ! 
                              numberOfVariables,   & ! number of equations
                              iStartRHSVector,     & ! (nD+1)
                              rhsVector,           & ! (nV,nI,nJ,nK,nL)
                              iStartAMatrixXi,     & ! (nD+2)
                              aMatrixXi,           & ! (nV,nV,nI,nJ,nK,nL)
                              iStartAMatrixEta,    & ! (nD+2)
                              aMatrixEta,          & ! (nV,nV,nI,nJ,nK,nL)
                              iStartAMatrixZeta,   & ! (nD+2)
                              aMatrixZeta,         & ! (nV,nV,nI,nJ,nK,nL)
                              iStartAMatrixTau,    & ! (nD+2)
                              aMatrixTau,          & ! (nV,nV,nI,nJ,nK,nL)
                              iStartEpsilonXi,     & ! (nD)
                              epsilonXi,           & ! (nI,nJ,nK,nL)
                              iStartEpsilonEta,    & ! (nD)
                              epsilonEta,          & ! (nI,nJ,nK,nL)
                              iStartEpsilonZeta,   & ! (nD)
                              epsilonZeta,         & ! (nI,nJ,nK,nL)
                              iStartEpsilonTau,    & ! (nD)
                              epsilonTau,          & ! (nI,nJ,nK,nL)
                              iStartDeltaSigma,    & ! (nD)
                              deltaSigma,          & ! (nI,nJ,nK,nL)
                              iStartDeltaQ,        & ! (nD+1)
                              deltaQ,              & ! (nV,nI,nJ,nK,nL)
                              iMinUpdate,          & ! (nD)
                              iMaxUpdate,          & ! (nD)
                              enableChecking,      &
                              errorInfoObject) 

! define incoming data
                               
  INTEGER, PARAMETER :: nD = 4 ! this is a 4D routine

  TYPE(LUSGSType), INTENT(INOUT) :: object

  INTEGER, INTENT(IN) :: numberOfVariables

  INTEGER, DIMENSION(nD+1), INTENT(IN) :: iStartRHSVector
  REAL(KIND=rDef), DIMENSION(iStartRHSVector(1):, &
                             iStartRHSVector(2):, &
                             iStartRHSVector(3):, &
                             iStartRHSVector(4):, &
                             iStartRHSVector(5):), INTENT(IN) :: rhsVector

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartAMatrixXi
  REAL(KIND=rDef), DIMENSION(iStartAMatrixXi(1):, &
                             iStartAMatrixXi(2):, &
                             iStartAMatrixXi(3):, &
                             iStartAMatrixXi(4):, &
                             iStartAMatrixXi(5):, &
                             iStartAMatrixXi(6):), INTENT(IN) :: aMatrixXi

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartAMatrixEta
  REAL(KIND=rDef), DIMENSION(iStartAMatrixEta(1):, &
                             iStartAMatrixEta(2):, &
                             iStartAMatrixEta(3):, &
                             iStartAMatrixEta(4):, &
                             iStartAMatrixEta(5):, &
                             iStartAMatrixEta(6):), INTENT(IN) :: aMatrixEta

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartAMatrixZeta
  REAL(KIND=rDef), DIMENSION(iStartAMatrixZeta(1):, &
                             iStartAMatrixZeta(2):, &
                             iStartAMatrixZeta(3):, &
                             iStartAMatrixZeta(4):, &
                             iStartAMatrixZeta(5):, &
                             iStartAMatrixZeta(6):), INTENT(IN) :: aMatrixZeta

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartAMatrixTau
  REAL(KIND=rDef), DIMENSION(iStartAMatrixTau(1):, &
                             iStartAMatrixTau(2):, &
                             iStartAMatrixTau(3):, &
                             iStartAMatrixTau(4):, &
                             iStartAMatrixTau(5):, &
                             iStartAMatrixTau(6):), INTENT(IN) :: aMatrixTau

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartEpsilonXi
  REAL(KIND=rDef), DIMENSION(iStartEpsilonXi(1):, &
                             iStartEpsilonXi(2):, &
                             iStartEpsilonXi(3):, &
                             iStartEpsilonXi(4):), INTENT(IN) :: epsilonXi

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartEpsilonEta
  REAL(KIND=rDef), DIMENSION(iStartEpsilonEta(1):, &
                             iStartEpsilonEta(2):, &
                             iStartEpsilonEta(3):, &
                             iStartEpsilonEta(4):), INTENT(IN) :: epsilonEta

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartEpsilonZeta
  REAL(KIND=rDef), DIMENSION(iStartEpsilonZeta(1):, &
                             iStartEpsilonZeta(2):, &
                             iStartEpsilonZeta(3):, &
                             iStartEpsilonZeta(4):), INTENT(IN) :: epsilonZeta

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartEpsilonTau
  REAL(KIND=rDef), DIMENSION(iStartEpsilonTau(1):, &
                             iStartEpsilonTau(2):, &
                             iStartEpsilonTau(3):, &
                             iStartEpsilonTau(4):), INTENT(IN) :: epsilonTau

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartDeltaSigma
  REAL(KIND=rDef), DIMENSION(iStartDeltaSigma(1):, &
                             iStartDeltaSigma(2):, &
                             iStartDeltaSigma(3):, &
                             iStartDeltaSigma(4):), INTENT(IN) :: deltaSigma

  INTEGER, DIMENSION(nD+1), INTENT(IN) :: iStartDeltaQ
  REAL(KIND=rDef), DIMENSION(iStartDeltaQ(1):, &
                             iStartDeltaQ(2):, &
                             iStartDeltaQ(3):, &
                             iStartDeltaQ(4):, &
                             iStartDeltaQ(5):), INTENT(INOUT) :: deltaQ

  INTEGER, DIMENSION(nD), INTENT(IN) :: iMinUpdate, &
                                        iMaxUpdate

  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL(KIND=rDef), DIMENSION(numberOfVariables,           &
                             iMinUpdate(1):iMaxUpdate(1), &
                             iMinUpdate(2):iMaxUpdate(2), &
                             iMinUpdate(3):iMaxUpdate(3), &
                             iMinUpdate(4):iMaxUpdate(4)) :: dQ

  INTEGER, DIMENSION(4,2) :: iMinSweep, &
                             iMaxSweep

  INTEGER, DIMENSION(2) :: dISweep 

  REAL(KIND=rDef), DIMENSION(numberOfVariables,           &
                             iMinUpdate(1):iMaxUpdate(1), &
                             iMinUpdate(2):iMaxUpdate(2), &
                             iMinUpdate(3):iMaxUpdate(3)) :: dQLastTau 

  REAL(KIND=rDef), DIMENSION(numberOfVariables,           &
                             iMinUpdate(1):iMaxUpdate(1), &
                             iMinUpdate(2):iMaxUpdate(2)) :: dQLastZeta 

  REAL(KIND=rDef), DIMENSION(numberOfVariables, &
                             iMinUpdate(1):iMaxUpdate(1)) :: dQLastEta 

  REAL(KIND=rDef), DIMENSION(numberOfVariables) :: dQLastXi 

  REAL(KIND=rDef), DIMENSION(numberOfVariables,numberOfVariables) :: aMatrixLast

  INTEGER :: n,nV,i,j,k,l,ii,jj,kk,ll,n1,n2

  REAL(KIND=rDef) :: fac1,fac2,iFacA,jFacA,kFacA,lFacA

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE LUSGSInviscid4DE4D'

  IF (enableChecking) THEN
   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'LUSGSObject is not initialized.'
    GO TO 100
   END IF
  ELSE ! no checking...
   CONTINUE
  END IF

  IF (object%initialSweepDirection > 0) THEN ! +/-
   DO n=1,4
    iMinSweep(n,1) = iMinUpdate(n)
    iMaxSweep(n,1) = iMaxUpdate(n)
    dISweep(1)     =  1
    iMinSweep(n,2) = iMaxUpdate(n)
    iMaxSweep(n,2) = iMinUpdate(n)
    dISweep(2)     = -1
   END DO
  ELSE ! -/+
   DO n=1,4
    iMinSweep(n,1) = iMaxUpdate(n)
    iMaxSweep(n,1) = iMinUpdate(n)
    dISweep(1)     = -1
    iMinSweep(n,2) = iMinUpdate(n)
    iMaxSweep(n,2) = iMaxUpdate(n)
    dISweep(2)     =  1
   END DO
  END IF

! sweep 1

  include 'TauSweep4DInviscid1.f90'

! sweep2
  
  include 'TauSweep4DInviscid2.f90'

! change the sweep direction

  object%initialSweepDirection = -object%initialSweepDirection

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

END SUBROUTINE LUSGSInviscid4DE4D
                              
