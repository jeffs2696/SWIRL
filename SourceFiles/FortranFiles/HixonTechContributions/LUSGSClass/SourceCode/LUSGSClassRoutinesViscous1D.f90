! these routines are inserted.

SUBROUTINE LUSGSViscous1DE1D(object,              & ! 
                             numberOfVariables,   & ! number of equations
                             iStartRHSVector,     & ! (nD+1)
                             rhsVector,           & ! (nV,nI)
                             iStartAMatrixXi,     & ! (nD+2)
                             aMatrixXi,           & ! (nV,nV,nI)
                             iStartEpsilonXi,     & ! (nD)
                             epsilonXi,           & ! (nI)
                             iStartBMatrixXi,     & ! (nD+2)
                             bMatrixXi,           & ! (nV,nV,nI)
                             iStartDeltaSigma,    & ! (nD)
                             deltaSigma,          & ! (nI)
                             iStartDeltaQ,        & ! (nD+1)
                             deltaQ,              & ! (nV,nI)
                             iMinUpdate,          & ! (nD)
                             iMaxUpdate,          & ! (nD)
                             enableChecking,      &
                             errorInfoObject) 

! define incoming data
                               
  INTEGER, PARAMETER :: nD = 1 ! this is a 1D routine

  TYPE(LUSGSType), INTENT(INOUT) :: object

  INTEGER, INTENT(IN) :: numberOfVariables

  INTEGER, DIMENSION(nD+1), INTENT(IN) :: iStartRHSVector
  REAL(KIND=rDef), DIMENSION(iStartRHSVector(1):, &
                             iStartRHSVector(2):), INTENT(IN) :: rhsVector

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartAMatrixXi
  REAL(KIND=rDef), DIMENSION(iStartAMatrixXi(1):, &
                             iStartAMatrixXi(2):, &
                             iStartAMatrixXi(3):), INTENT(IN) :: aMatrixXi

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartEpsilonXi
  REAL(KIND=rDef), DIMENSION(iStartEpsilonXi(1):), INTENT(IN) :: epsilonXi

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartBMatrixXi
  REAL(KIND=rDef), DIMENSION(iStartBMatrixXi(1):, &
                             iStartBMatrixXi(2):, &
                             iStartBMatrixXi(3):), INTENT(IN) :: bMatrixXi

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartDeltaSigma
  REAL(KIND=rDef), DIMENSION(iStartDeltaSigma(1):), INTENT(IN) :: deltaSigma

  INTEGER, DIMENSION(nD+1), INTENT(IN) :: iStartDeltaQ
  REAL(KIND=rDef), DIMENSION(iStartDeltaQ(1):, &
                             iStartDeltaQ(2):), INTENT(INOUT) :: deltaQ

  INTEGER, DIMENSION(nD), INTENT(IN) :: iMinUpdate, &
                                        iMaxUpdate

  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL(KIND=rDef), DIMENSION(numberOfVariables,           &
                             iMinUpdate(1):iMaxUpdate(1)) :: dQ

  INTEGER, DIMENSION(1,2) :: iMinSweep, &
                             iMaxSweep

  INTEGER, DIMENSION(2) :: dISweep 

  REAL(KIND=rDef), DIMENSION(numberOfVariables) :: dQLastXi,  &
                                                   dQCurrent
 
  REAL(KIND=rDef), DIMENSION(numberOfVariables,                  &
                             numberOfVariables) :: abMatrixLast, &
                                                   abMatrixCurrent

  INTEGER, DIMENSION(numberOfVariables) :: IPIV ! for DGESV call

  INTEGER :: n,nV,i,ii,n1,n2, &
             INFO ! for DGESV call

  REAL(KIND=rDef) :: iFacA

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE LUSGSViscous1DE1D'

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
   DO n=1,1
    iMinSweep(n,1) = iMinUpdate(n)
    iMaxSweep(n,1) = iMaxUpdate(n)
    dISweep(1)     =  1
    iMinSweep(n,2) = iMaxUpdate(n)
    iMaxSweep(n,2) = iMinUpdate(n)
    dISweep(2)     = -1
   END DO
  ELSE ! -/+
   DO n=1,1
    iMinSweep(n,1) = iMaxUpdate(n)
    iMaxSweep(n,1) = iMinUpdate(n)
    dISweep(1)     = -1
    iMinSweep(n,2) = iMinUpdate(n)
    iMaxSweep(n,2) = iMaxUpdate(n)
    dISweep(2)     =  1
   END DO
  END IF

! sweep 1

  include 'XiSweep1DViscous1.f90'

! sweep2
  
  include 'XiSweep1DViscous2.f90'

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

END SUBROUTINE LUSGSViscous1DE1D
                              
