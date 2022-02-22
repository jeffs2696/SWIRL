! these routines are inserted.

SUBROUTINE LUSGSViscous2DE2D(object,              & ! 
                             numberOfVariables,   & ! number of equations
                             iStartRHSVector,     & ! (nD+1)
                             rhsVector,           & ! (nV,nI,nJ)
                             iStartAMatrixXi,     & ! (nD+2)
                             aMatrixXi,           & ! (nV,nV,nI,nJ)
                             iStartAMatrixEta,    & ! (nD+2)
                             aMatrixEta,          & ! (nV,nV,nI,nJ)
                             iStartEpsilonXi,     & ! (nD)
                             epsilonXi,           & ! (nI,nJ)
                             iStartEpsilonEta,    & ! (nD)
                             epsilonEta,          & ! (nI,nJ)
                             iStartBMatrixXi,     & ! (nD+2)
                             bMatrixXi,           & ! (nV,nV,nI,nJ)
                             iStartBMatrixEta,    & ! (nD+2)
                             bMatrixEta,          & ! (nV,nV,nI,nJ)
                             iStartDeltaSigma,    & ! (nD)
                             deltaSigma,          & ! (nI,nJ)
                             iStartDeltaQ,        & ! (nD+1)
                             deltaQ,              & ! (nV,nI,nJ)
                             iMinUpdate,          & ! (nD)
                             iMaxUpdate,          & ! (nD)
                             enableChecking,      &
                             errorInfoObject) 

! define incoming data
                               
  INTEGER, PARAMETER :: nD = 2 ! this is a 2D routine

  TYPE(LUSGSType), INTENT(INOUT) :: object

  INTEGER, INTENT(IN) :: numberOfVariables

  INTEGER, DIMENSION(nD+1), INTENT(IN) :: iStartRHSVector
  REAL(KIND=rDef), DIMENSION(iStartRHSVector(1):, &
                             iStartRHSVector(2):, &
                             iStartRHSVector(3):), INTENT(IN) :: rhsVector

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartAMatrixXi
  REAL(KIND=rDef), DIMENSION(iStartAMatrixXi(1):, &
                             iStartAMatrixXi(2):, &
                             iStartAMatrixXi(3):, &
                             iStartAMatrixXi(4):), INTENT(IN) :: aMatrixXi

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartAMatrixEta
  REAL(KIND=rDef), DIMENSION(iStartAMatrixEta(1):, &
                             iStartAMatrixEta(2):, &
                             iStartAMatrixEta(3):, &
                             iStartAMatrixEta(4):), INTENT(IN) :: aMatrixEta

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartEpsilonXi
  REAL(KIND=rDef), DIMENSION(iStartEpsilonXi(1):, &
                             iStartEpsilonXi(2):), INTENT(IN) :: epsilonXi

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartEpsilonEta
  REAL(KIND=rDef), DIMENSION(iStartEpsilonEta(1):, &
                             iStartEpsilonEta(2):), INTENT(IN) :: epsilonEta

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartBMatrixXi
  REAL(KIND=rDef), DIMENSION(iStartBMatrixXi(1):, &
                             iStartBMatrixXi(2):, &
                             iStartBMatrixXi(3):, &
                             iStartBMatrixXi(4):), INTENT(IN) :: bMatrixXi

  INTEGER, DIMENSION(nD+2), INTENT(IN) :: iStartBMatrixEta
  REAL(KIND=rDef), DIMENSION(iStartBMatrixEta(1):, &
                             iStartBMatrixEta(2):, &
                             iStartBMatrixEta(3):, &
                             iStartBMatrixEta(4):), INTENT(IN) :: bMatrixEta

  INTEGER, DIMENSION(nD), INTENT(IN) :: iStartDeltaSigma
  REAL(KIND=rDef), DIMENSION(iStartDeltaSigma(1):, &
                             iStartDeltaSigma(2):), INTENT(IN) :: deltaSigma

  INTEGER, DIMENSION(nD+1), INTENT(IN) :: iStartDeltaQ
  REAL(KIND=rDef), DIMENSION(iStartDeltaQ(1):, &
                             iStartDeltaQ(2):, &
                             iStartDeltaQ(3):), INTENT(INOUT) :: deltaQ

  INTEGER, DIMENSION(nD), INTENT(IN) :: iMinUpdate, &
                                        iMaxUpdate

  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  REAL(KIND=rDef), DIMENSION(numberOfVariables,           &
                             iMinUpdate(1):iMaxUpdate(1), &
                             iMinUpdate(2):iMaxUpdate(2)) :: dQ

  INTEGER, DIMENSION(2,2) :: iMinSweep, &
                             iMaxSweep

  INTEGER, DIMENSION(2) :: dISweep 

  REAL(KIND=rDef), DIMENSION(numberOfVariables, &
                             iMinUpdate(1):iMaxUpdate(1)) :: dQLastEta 

  REAL(KIND=rDef), DIMENSION(numberOfVariables) :: dQLastXi,  &
                                                   dQCurrent
 
  REAL(KIND=rDef), DIMENSION(numberOfVariables,                  &
                             numberOfVariables) :: abMatrixLast, &
                                                   abMatrixCurrent

  INTEGER, DIMENSION(numberOfVariables) :: IPIV ! for DGESV call

  INTEGER :: n,nV,i,j,ii,jj,n1,n2, &
             INFO ! for DGESV call

  REAL(KIND=rDef) :: iFacA,jFacA

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE LUSGSViscous2DE2D'

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
   DO n=1,2
    iMinSweep(n,1) = iMinUpdate(n)
    iMaxSweep(n,1) = iMaxUpdate(n)
    dISweep(1)     =  1
    iMinSweep(n,2) = iMaxUpdate(n)
    iMaxSweep(n,2) = iMinUpdate(n)
    dISweep(2)     = -1
   END DO
  ELSE ! -/+
   DO n=1,2
    iMinSweep(n,1) = iMaxUpdate(n)
    iMaxSweep(n,1) = iMinUpdate(n)
    dISweep(1)     = -1
    iMinSweep(n,2) = iMinUpdate(n)
    iMaxSweep(n,2) = iMaxUpdate(n)
    dISweep(2)     =  1
   END DO
  END IF

! sweep 1

  include 'EtaSweep2DViscous1.f90'

! sweep2
  
  include 'EtaSweep2DViscous2.f90'

! debug
! DO j=iMinUpdate(2),iMaxUpdate(2)
!  DO i=iMinUpdate(1),iMaxUpdate(1)
!   DO ii=1,numberOfVariables
!    WRITE(0,*) 'LU: ',i,j,ii,deltaQ(ii,i,j),dQ(ii,i,j),rhsVector(ii,i,j)
!   END DO
!  END DO
! END DO

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

END SUBROUTINE LUSGSViscous2DE2D
                              
