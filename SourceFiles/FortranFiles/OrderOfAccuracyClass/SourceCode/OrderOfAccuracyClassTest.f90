PROGRAM MAIN
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE OrderOfAccuracyClass
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  USE OrderOfAccuracyClassTest_General
  IMPLICIT NONE

! define variables

  TYPE(mpiDataObjectType) :: mpiData

  LOGICAL, PARAMETER :: verbose = .TRUE.
  LOGICAL, PARAMETER :: enableChecking = .TRUE.
  LOGICAL, PARAMETER :: forceSorting = .TRUE.

  LOGICAL :: passedTest = .TRUE., &
             masterNode = .TRUE.

  LOGICAL :: errorDetected = .FALSE., &
             errorFound,              &
             errorExpected
  CHARACTER(LEN=120) :: errorInformation

  INTEGER :: numberOfNodes,           &
             myMPINodeID

  LOGICAL :: incomingMessageDetected,   &
             nodesAreSynchronized


! MPI routine data

  DOUBLE PRECISION :: initialTime,finalTime,timePrecision

  INTEGER, PARAMETER :: rDef          = REAL64

  TYPE(OrderOfAccuracyType) :: OofAObject

  TYPE(ErrorInformationType) :: errorInfoObject

  passedTest = .TRUE.
  errorExpected = .FALSE.

  errorFound = .FALSE.

  incomingMessageDetected = .FALSE.
  IF (incomingMessageDetected) CONTINUE

  CALL CreateObject(object           = mpiData,        &
                    verbose          = verbose,        &
                    enableChecking   = enableChecking, &
                    errorDetected    = errorDetected,  &
                    errorInformation = errorInformation)

  IF (errorDetected) THEN
   passedTest = .FALSE.
   IF (verbose .AND. masterNode)  THEN
     WRITE(0,*) 'Error detected when creating MPIDataObject: ', &
       TRIM(errorInformation)
   END IF
   GO TO 100
  END IF

  masterNode = IsMasterNode(object = mpiData)

  myMPINodeID = GetNodeID(object = mpiData)

  IF (masterNode) WRITE(0,103)

  numberOfNodes = GetNumberOfNodes(object = mpiData)

  IF (masterNode) WRITE(0,104) numberOfNodes

  IF (verbose .AND. masterNode)  &
     WRITE(0,*) 'Testing MPIDataClass MPI calls: ',GetNodeID(object = mpiData)

! start timing now

  initialTime = GetMPITimerValue(mpiData = mpiData)

  IF (masterNode) WRITE(0,*)
  IF (masterNode) WRITE(0,*) 'MPI Implementation Parameters: '
  IF (masterNode) WRITE(0,*)

  IF (masterNode) WRITE(0,105) GetMPIVersion(object = mpiData), &
                               GetMPISubVersion(object = mpiData)

  IF (masterNode) WRITE(0,104) GetNumberOfNodes(object = mpiData)

  timePrecision = GetMPITimerIncrement(mpiData = mpiData)
  IF (masterNode) WRITE(0,101) timePrecision
  IF (masterNode) WRITE(0,*)

  nodesAreSynchronized = SynchronizeMPINodes(mpiData = mpiData)

  IF (nodesAreSynchronized) THEN
   IF (masterNode) WRITE(0,*) 'PASS:  SynchronizeMPINodes call.'
  ELSE
   passedTest = .FALSE.
   GO TO 100
  END IF

!----------------create the errorInfoObject---------------------

  errorExpected = .FALSE.

  IF (masterNode) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: create an errorInfoObject.'
   WRITE(0,*) '      No error is expected.'
  END IF

  CALL CreateObject(object = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   CALL WriteObject(object = errorInfoObject)
   GO TO 100
  ELSE
   IF (masterNode) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
  END IF

  CALL OrderOfAccuracyClassTestGeneral(OofAObject        = OofAObject,     &
                                       masterNode        = masterNode,     &
                                       enableChecking    = enableChecking, &
                                       errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

 100 CONTINUE

  IF (passedTest) THEN
   IF (masterNode) THEN
    WRITE(0,999)
    OPEN(33,FILE = 'OrderOfAccuracyClassTest.PASSED',FORM = 'FORMATTED')
    WRITE(33,999)
    CLOSE(33)
   END IF
  ELSE
   IF (masterNode) THEN
    WRITE(0,998)
    OPEN(33,FILE = 'OrderOfAccuracyClassTest.FAILED',FORM = 'FORMATTED')
    WRITE(33,998)
    CLOSE(33)
   END IF
  END IF

  errorFound = CheckForGlobalError(errorInfoObject) 
  IF (errorFound) THEN
   CALL WriteObject(object = errorInfoObject)
  ELSE
   CONTINUE
  END IF

  CALL DestroyObject(object = errorInfoObject)

  finalTime = GetMPITimerValue(mpiData = mpiData)
  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,102) finalTime - initialTime
  END IF

  IF (verbose .AND. masterNode) WRITE(0,*) 'Testing MPIDataClass shutdown'

  CALL DestroyObject(object           = mpiData,        &
                     verbose          = verbose,        &
                     enableChecking   = enableChecking, &
                     errorDetected    = errorDetected,  &
                     errorInformation = errorInformation)

  IF (errorDetected) THEN
   passedTest = .FALSE.
   IF (verbose .AND. masterNode)  THEN
     WRITE(0,*) 'FAIL:  Error detected when destroying MPIDataObject: ', &
       TRIM(errorInformation)
   END IF
  END IF

  STOP

 101 FORMAT(1x,'MPI timing precision = ',e13.5,' seconds.')
 102 FORMAT(1x,'Testing program took ',f10.5,' seconds.')
 103 FORMAT(1x,/,&
 1x,' ----------------------------------------- ',/,&
 1x,'|   OrderOfAccuracy Object                |',/,&
 1x,'|      testing routines                   |',/,&
 1x,'|  copyright 2021 by Hixon Technology     |',/,&
 1x,'|  All rights reserved.                   |',/,&
 1x,'|  No warranty for this code express      |',/,&
 1x,'|    or implied.                          |',/,&
 1x,'|  contact:  Ray Hixon                    |',/,&
 1x,'|   email: fshixon@yahoo.com              |',/,&
 1x,' ----------------------------------------- ',/,&
 1x)
 104 FORMAT(1x,'Number of MPI nodes detected = ',i8)
 105 FORMAT(1x,'MPI Version = ',i5,'.',i5)

 998 FORMAT(1x,'FAIL:  Testing routine FAILED test.',/)
 999 FORMAT(1x,'PASS:  Testing routine PASSED all tests.',/)

END PROGRAM MAIN
