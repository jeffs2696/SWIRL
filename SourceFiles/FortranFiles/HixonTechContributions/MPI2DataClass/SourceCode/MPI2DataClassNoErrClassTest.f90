PROGRAM MPI2DataClassNoErrClassTest
  USE MPI2DataClassNoErrClass           ! MPI2DataClassNoErrClass.f90
  USE Test1DArrayPassingModule
  USE Test2DArrayPassingModule
  USE Test3DArrayPassingModule
  USE Test4DArrayPassingModule
  USE Test5DArrayPassingModule
  USE ErrorInformationClass             ! ErrorInformationClass.f90
  USE Check0DData
  USE Check1DData
  USE Check2DData
  USE Check3DData
  USE Check4DData
  USE Check5DData

  IMPLICIT NONE

  TYPE(mpiDataObjectType) :: mpiData

  LOGICAL, PARAMETER :: verbose = .TRUE.
  LOGICAL, PARAMETER :: enableChecking = .TRUE.
  LOGICAL, PARAMETER :: test3D = .TRUE.  ! last 3D test PASSED on 7/21/13
  LOGICAL, PARAMETER :: test4D = .FALSE. ! last 4D test PASSED on 7/21/13
  LOGICAL, PARAMETER :: test5D = .FALSE.  ! 5D not tested 5/19/21
  LOGICAL, PARAMETER :: runInt8Tests = .TRUE.

  DOUBLE PRECISION :: maxWaitTimeInSeconds

  INTEGER, DIMENSION(5) :: errorLoc5D

  LOGICAL :: passedTest = .TRUE., &
             masterNode = .TRUE.

  LOGICAL :: errorDetected = .FALSE., &
             errorFound
  CHARACTER(LEN=120) :: errorInformation

  INTEGER :: numberOfNodes,           &
             myMPINodeID,             &
             broadcastingNodeID,      &
             recvingNodeID,           &
             sendingNodeID,           &
             messageTag,              &
             messageTypeID1,          &
             messageTypeID2,          &
             messageLength

  LOGICAL :: incomingMessageDetected,   &
             nodesAreSynchronized
!            messageTransferIsComplete, &

  LOGICAL :: isEqual
  INTEGER :: n,i,j,k,l, &
             dI1,dJ1,dK1,dL1, &
             dI2,dJ2,dK2,dL2, &
             ii,jj,kk,ll

  INTEGER, DIMENSION(4,4) :: tMat

! MPI routine data

  DOUBLE PRECISION :: initialTime,finalTime,timePrecision

! message buffer data; each message buffer requires extra data for
!  current message passing calls.

!  struct data types

  TYPE(structMessageObjectType) :: structMessage1,   &
                                   structMessage2,   &
                                   struct4DMessage1, &
                                   struct4DMessage2, &
                                   struct4DMessageT

  INTEGER :: structureSize

! error information object

  TYPE(errorInformationType) :: errorInfoObject
  TYPE(CharacterStringType) :: charStringObject

!--------------character message buffers----------------------

  CHARACTER :: charVar1Data
  CHARACTER :: charVar2Data
  INTEGER :: charArrayMin   = 5, &
             charArrayMax   = 1, &
             charArrayDelta = -1
  CHARACTER, DIMENSION(5) :: charArray1Data
  CHARACTER, DIMENSION(5) :: charArray2Data

!--------------logical message buffers------------------------

  LOGICAL :: logicalVar1Data
  LOGICAL :: logicalVar2Data
  INTEGER :: logicalArrayMin   = 1, &
             logicalArrayMax   = 4, &
             logicalArrayDelta = 3
  LOGICAL, DIMENSION(4) :: logicalArray1Data
  LOGICAL, DIMENSION(4) :: logicalArray2Data

!--------------int4 message buffers------------------------

  INTEGER(KIND=4) :: int4Var1Data
  INTEGER(KIND=4) :: int4Var2Data
  INTEGER :: int4ArrayMin   = 4, &
             int4ArrayMax   = 5, &
             int4ArrayDelta = 1
  INTEGER(KIND=4), DIMENSION(5) :: int4Array1Data
  INTEGER(KIND=4), DIMENSION(5) :: int4Array2Data

!--------------int8 message buffers------------------------

  INTEGER(KIND=8) :: int8Var1Data
  INTEGER(KIND=8) :: int8Var2Data
  INTEGER :: int8ArrayMin   = 4, &
             int8ArrayMax   = 5, &
             int8ArrayDelta = 1
  INTEGER(KIND=8), DIMENSION(5) :: int8Array1Data
  INTEGER(KIND=8), DIMENSION(5) :: int8Array2Data

!--------------real4 message buffers------------------------

  REAL(KIND=4) :: real4Var1Data
  REAL(KIND=4) :: real4Var2Data
  INTEGER :: real4ArrayMin   = 5, &
             real4ArrayMax   = 3, &
             real4ArrayDelta = -2
  REAL(KIND=4), DIMENSION(5) :: real4Array1Data
  REAL(KIND=4), DIMENSION(5) :: real4Array2Data

!--------------real8 message buffers------------------------

  REAL(KIND=8) :: real8Var1Data
  REAL(KIND=8) :: real8Var2Data
  INTEGER :: real8ArrayMin   = 2, &
             real8ArrayMax   = 5, &
             real8ArrayDelta = 1
  REAL(KIND=8), DIMENSION(5) :: real8Array1Data
  REAL(KIND=8), DIMENSION(5) :: real8Array2Data

! 4D arrays : 
!  array 1 -> 2:  i => -l
!                 j =>  k 
!                 k => -i
!                 l =>  j

  INTEGER, DIMENSION(4) :: iOrder1, &
                           iOrder2, &
                           iOrderT, &
                           iStart1, &
                           iStart2, &
                           iStartT, &
                           iEnd1,   &
                           iEnd2,   &
                           iEndT,   &
                           iDelta1, &
                           iDelta2, &
                           iDeltaT
  
  REAL(KIND=8), DIMENSION(4,3,2,5) :: real8Array14DData
  REAL(KIND=8), DIMENSION(2,5,3,4) :: real8Array24DData, &
                                      real8Array24DTest

!------end of variable definition----------------------------

  CONTINUE ! execution begins here

  maxWaitTimeInSeconds = 120.0d0
  initialTime = 0.0d0

  incomingMessageDetected = .FALSE.
  IF (incomingMessageDetected) CONTINUE

! IF (verbose) WRITE(6,*) 'Testing MPIDataClass startup'

  CALL CreateObject(object           = mpiData,        &
                    verbose          = verbose,        &
                    enableChecking   = enableChecking, &
                    errorDetected    = errorDetected,  &
                    errorInformation = errorInformation)

  IF (errorDetected) THEN
   passedTest = .FALSE.
   IF (verbose .AND. masterNode)  THEN
     WRITE(6,*) 'Error detected when creating MPIDataObject: ', &
       TRIM(errorInformation)
   END IF
   GO TO 800
  END IF

!----------------create the errorInfoObject---------------------

  CALL CreateObject(object = errorInfoObject)
  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound) THEN
   IF (masterNode) THEN
    WRITE(0,*) 'master: error found.'
   END IF
   CALL WriteObject(object = errorInfoObject)
  END IF

  masterNode = IsMasterNode(object = mpiData)

  myMPINodeID = GetNodeID(object = mpiData)

  IF (masterNode) WRITE(6,103)
  IF (masterNode) WRITE(6,103)

  numberOfNodes = GetNumberOfNodes(object = mpiData)

  IF (masterNode) WRITE(6,104) numberOfNodes

  IF (verbose .AND. masterNode)  &
     WRITE(6,*) 'Testing MPIDataClass MPI calls: ',GetNodeID(object = mpiData)

! now try out the calls

! start timing now

  initialTime = GetMPITimerValue(mpiData = mpiData)

  IF (masterNode) WRITE(6,*)
  IF (masterNode) WRITE(6,*) 'MPI Implementation Parameters: '
  IF (masterNode) WRITE(6,*)

  IF (masterNode) WRITE(6,105) GetMPIVersion(object = mpiData), &
                               GetMPISubVersion(object = mpiData)

  IF (masterNode) WRITE(6,104) GetNumberOfNodes(object = mpiData)

  IF (numberOfNodes /= 2) THEN
   WRITE(charStringObject%charString,'(a7,1x,i5)') 'found: ',numberOfNodes
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)
   charStringObject%charString = 'ERROR:  Testing routine is for TWO nodes only!'
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)
   passedTest = .FALSE.
   GO TO 800
  END IF

  timePrecision = GetMPITimerIncrement(mpiData = mpiData)
  IF (masterNode) WRITE(6,101) timePrecision
  IF (masterNode) WRITE(6,*)

  nodesAreSynchronized = SynchronizeMPINodes(mpiData = mpiData)

  IF (nodesAreSynchronized) THEN
   IF (masterNode) WRITE(6,*) 'PASS:  SynchronizeMPINodes call.' 
  ELSE
   passedTest = .FALSE.
   GO TO 800
  END IF

!----------------start of Character testing---------------------

!------------------CharacterArray------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  MPI_STRUCT class message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   charVar1Data = 'A'
   charVar2Data = 'B'
   charArray1Data = (/'A','B','C','D','E'/)
   charArray2Data = (/'B','C','D','E','F'/)
   logicalVar1Data = .TRUE.
   logicalVar2Data = .FALSE.
   logicalArray1Data = (/.TRUE.,.FALSE.,.TRUE.,.FALSE./)
   logicalArray2Data = (/.FALSE.,.TRUE.,.FALSE.,.TRUE./)
   int4Var1Data = 1_4
   int4Var2Data = -21_4
   int4Array1Data = (/1_4, 2_4, 3_4, 4_4, 5_4/)
   int4Array2Data = (/2_4, 3_4, 4_4, 5_4, 6_4/)
   int8Var1Data = 1_8
   int8Var2Data = -21_8
   int8Array1Data = (/1_8, 2_8, 3_8, 8_8, 5_8/)
   int8Array2Data = (/2_8, 3_8, 8_8, 5_8, 6_8/)
   real4Var1Data = 1.1_4
   real4Var2Data = -21.1_4
   real4Array1Data = (/1.1_4, 2.1_4, 3.1_4, 4.1_4, 5.1_4/)
   real4Array2Data = (/2.1_4, 3.1_4, 4.1_4, 5.1_4, 6.1_4/)
   real8Var1Data = 1.23_8
   real8Var2Data = -21.23_8
   real8Array1Data = (/1.23_8, 2.23_8, 3.23_8, 4.23_8, 5.23_8/)
   real8Array2Data = (/2.23_8, 3.23_8, 4.23_8, 5.23_8, 6.23_8/)
  ELSE
   charVar1Data = 'C'
   charVar2Data = 'D'
   charArray1Data = (/'C','D','E','F','G'/)
   charArray2Data = (/'D','E','F','G','H'/)
   logicalVar1Data = .FALSE.
   logicalVar2Data = .TRUE.
   logicalArray1Data = (/.TRUE.,.TRUE.,.TRUE.,.FALSE./)
   logicalArray2Data = (/.FALSE.,.TRUE.,.TRUE.,.TRUE./)
   int4Var1Data = 23_4
   int4Var2Data = 42_4
   int4Array1Data = (/3_4, 4_4, 5_4, 6_4, 7_4/)
   int4Array2Data = (/4_4, 5_4, 6_4, 7_4, 8_4/)
   int8Var1Data = 23_8
   int8Var2Data = 82_8
   int8Array1Data = (/3_8, 8_8, 5_8, 6_8, 7_8/)
   int8Array2Data = (/8_8, 5_8, 6_8, 7_8, 8_8/)
   real4Var1Data = 23.1_4
   real4Var2Data = 42.1_4
   real4Array1Data = (/3.1_4, 4.1_4, 5.1_4, 6.1_4, 7.1_4/)
   real4Array2Data = (/4.1_4, 5.1_4, 6.1_4, 7.1_4, 8.1_4/)
   real8Var1Data = 23.23_8
   real8Var2Data = 42.23_8
   real8Array1Data = (/3.23_8, 4.23_8, 5.23_8, 6.23_8, 7.23_8/)
   real8Array2Data = (/4.23_8, 5.23_8, 6.23_8, 7.23_8, 8.23_8/)
  END IF

!-------------create messages for all data--------------------

  messageLength = 5
  messageTag    = 5
  sendingNodeID = 0
  recvingNodeID = 0

  messageTypeID1 = 3
  messageTypeID2 = 4

  CALL CreateObject(object            = structMessage1,     &
                    nodeMPIDataObject = mpiData,            &
                    messageTag        = messageTag,         &
                    messageTypeID     = messageTypeID1,     &
                    sendingNodeID     = sendingNodeID,      & 
                    recvingNodeID     = recvingNodeID,      &
                    enableChecking    = enableChecking,     &
                    errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject creation (structMessage1) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL CreateObject(object            = structMessage2,     &
                    nodeMPIDataObject = mpiData,            &
                    messageTag        = messageTag,         &
                    messageTypeID     = messageTypeID2,     &
                    sendingNodeID     = sendingNodeID,      & 
                    recvingNodeID     = recvingNodeID,      &
                    enableChecking    = enableChecking,     &
                    errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject creation (structMessage2) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! structMessage1

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = logicalVar1Data,      &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (logicalVar1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = charArray1Data,     &
                                       iStart                = charArrayMin,       &
                                       iEnd                  = charArrayMax,       &
                                       deltaI                = charArrayDelta,     &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = real8Var1Data,      &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (real8Var1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = logicalArray1Data,  &
                                       iStart                = logicalArrayMin,    &
                                       iEnd                  = logicalArrayMax,    &
                                       deltaI                = logicalArrayDelta,  &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = real4Var1Data,      &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (real8Var1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = charVar1Data,       &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (charVar1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = int4Array1Data,     &
                                       iStart                = int4ArrayMin,       &
                                       iEnd                  = int4ArrayMax,       &
                                       deltaI                = int4ArrayDelta,     &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (int4Array1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF
 
  IF (runInt8Tests) THEN

   CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                        structMessageObject   = structMessage1,     &
                                        messageData           = int8Array1Data,     &
                                        iStart                = int8ArrayMin,       &
                                        iEnd                  = int8ArrayMax,       &
                                        deltaI                = int8ArrayDelta,     &
                                        enableChecking        = enableChecking,     &
                                        errorInfoObject       = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during messageObject data addition (int8Array1) call.'
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = real4Array1Data,    &
                                       iStart                = real4ArrayMin,      &
                                       iEnd                  = real4ArrayMax,      &
                                       deltaI                = real4ArrayDelta,    &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (runInt8Tests) THEN

   CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                        structMessageObject   = structMessage1,     &
                                        messageData           = int8Var1Data,       &
                                        enableChecking        = enableChecking,     &
                                        errorInfoObject       = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during messageObject data addition (int8Var1) call.'
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = int4Var1Data,       &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (int4Var1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage1,     &
                                       messageData           = real8Array1Data,    &
                                       iStart                = real8ArrayMin,      &
                                       iEnd                  = real8ArrayMax,      &
                                       deltaI                = real8ArrayDelta,    &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! structMessage2

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage2,     &
                                       messageData           = logicalVar2Data,      &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (logicalVar2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = structMessage2,      &
                                       messageData           = charArray2Data,      &
                                       iStart                = charArrayMin,        &
                                       iEnd                  = charArrayMax,        &
                                       deltaI                = charArrayDelta,      &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage2,     &
                                       messageData           = real8Var2Data,      &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (real8Var2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = structMessage2,      &
                                       messageData           = logicalArray2Data,   &
                                       iStart                = logicalArrayMin,     &
                                       iEnd                  = logicalArrayMax,     &
                                       deltaI                = logicalArrayDelta,   &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage2,     &
                                       messageData           = real4Var2Data,      &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (real4Var2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage2,     &
                                       messageData           = charVar2Data,       &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (charVar2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = structMessage2,      &
                                       messageData           = int4Array2Data,      &
                                       iStart                = int4ArrayMin,        &
                                       iEnd                  = int4ArrayMax,        &
                                       deltaI                = int4ArrayDelta,      &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (int4Array2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (runInt8Tests) THEN

   CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                        structMessageObject   = structMessage2,      &
                                        messageData           = int8Array2Data,      &
                                        iStart                = int8ArrayMin,        &
                                        iEnd                  = int8ArrayMax,        &
                                        deltaI                = int8ArrayDelta,      &
                                        enableChecking        = enableChecking,      &
                                        errorInfoObject       = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during messageObject data addition (int8Array2) call.'
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = structMessage2,      &
                                       messageData           = real4Array2Data,      &
                                       iStart                = real4ArrayMin,       &
                                       iEnd                  = real4ArrayMax,        &
                                       deltaI                = real4ArrayDelta,    &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (runInt8Tests) THEN

   CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                        structMessageObject   = structMessage2,     &
                                        messageData           = int8Var2Data,       &
                                        enableChecking        = enableChecking,     &
                                        errorInfoObject       = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during messageObject data addition (int8Var2) call.'
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage2,     &
                                       messageData           = int4Var2Data,       &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (int4Var2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = structMessage2,      &
                                       messageData           = real8Array2Data,      &
                                       iStart                = real8ArrayMin,       &
                                       iEnd                  = real8ArrayMax,        &
                                       deltaI                = real8ArrayDelta,    &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,            &
                                       structMessageObject   = structMessage2,     &
                                       messageData           = int4Var2Data,       &
                                       enableChecking        = enableChecking,     &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (int4Var2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

   GO TO 100
  END IF

! message data is complete

  CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                        messageObject     = structMessage1,      &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (structMessage1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                        messageObject     = structMessage2,      &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (structMessage2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'COMPLETED:  MPI_STRUCT definition.'
   WRITE(6,*)
  END IF


!-------------broadcast message-------------------------------

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  BCAST of MPI_STRUCT'
   WRITE(6,*)
  END IF

  broadcastingNodeID = 0

  CALL ChangeMessageSendingNodeID(              &
        nodeMPIDataObject = mpiData,            &
        messageObject     = structMessage1,     &
        sendingNodeID     = broadcastingNodeID, &
        enableChecking    = enableChecking,     &
        errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject (characterArray1) call: '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'COMPLETED:  ChangeMessageSendingNodeID'
   WRITE(6,*)
  END IF

  CALL BroadcastMessage(nodeMPIDataObject    = mpiData,            &
                        messageObject        = structMessage1,     &
                        enableChecking       = enableChecking,     &
                        errorInfoObject      = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during BroadcastMessage (characterArray) call: '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'COMPLETED:  BroadcastMessage call'
   WRITE(6,*)
  END IF

  CALL WaitForMessageCompletion(                                     &
                        nodeMPIDataObject    = mpiData,              &
                        messageObject        = structMessage1,       &
                        maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                        enableChecking       = enableChecking,       &
                        errorInfoObject      = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
    'FAIL:  Error detected during (characterArray) call: '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  ELSE
   IF (masterNode .AND. verbose)  &
      WRITE(6,*) 'PASS:  BroadcastMessage (characterArray) call.'
  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'COMPLETED:  WaitForMessageCompletion call'
   WRITE(6,*)
  END IF

  IF (myMPINodeID == 1) THEN

   sendingNodeID       = 1
   recvingNodeID       = 0
   messageTag          = 1

   CALL ChangeMessageSendingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage1,    &
         sendingNodeID     = sendingNodeID,     &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during messageObject (characterArray1) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageRecvingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage1,    &
         recvingNodeID     = recvingNodeID,     &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during messageObject (characterArray1) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage1,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during messageObject (characterArray1) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL SendMessageToNode(                      &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage1,    &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = structMessage1,       &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

!-------------end of send message---------------------------------------

  ELSE IF (myMPINodeID == 0) THEN

!--------------------recv message--------------------------------------

   sendingNodeID = 1
   recvingNodeID = 0
   messageTag = 1

   CALL ChangeMessageSendingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         sendingNodeID     = sendingNodeID,     &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageRecvingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         recvingNodeID     = recvingNodeID,     &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL RecvMessageFromNode(                    &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = structMessage2,       &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

!------------------------end of recv message--------------------------------

! check for successful completion

   isEqual = .TRUE.
   messageLength = SIZE(charArray1Data,1)

   IF (isEqual .AND. (charVar1Data == charVar2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (characterVariable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (characterVariable) call.'
    WRITE(6,*) 'PASS:  WaitForMessageCompletion (characterVariable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (characterVariable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (characterVariable) call.'
    WRITE(6,*) 'FAIL:  WaitForMessageCompletion (characterVariable) call.'
    WRITE(6,*) ' expected ',charVar1Data,'; recved ',charVar2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=charArrayMin,charArrayMax,charArrayDelta
    IF (isEqual  &
         .AND.  &
        (charArray1Data(n) == charArray2Data(n)) &
         ) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (characterArray) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (characterArray) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (characterArray) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (characterArray) call.'
    WRITE(6,*) ' expected ',charArray2Data,'; recved ',charArray1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (isEqual .AND. (logicalVar1Data .EQV. logicalVar2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (logicalVariable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (logicalVariable) call.'
    WRITE(6,*) 'PASS:  TestForMessageCompletion (logicalVariable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (logicalVariable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (logicalVariable) call.'
    WRITE(6,*) 'FAIL:  TestForMessageCompletion (logicalVariable) call.'
    WRITE(6,*) ' expected ',logicalVar2Data,'; recved ',logicalVar1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=logicalArrayMin,logicalArrayMax,logicalArrayDelta
    IF (isEqual  &
         .AND.  &
    (logicalArray1Data(n) .EQV. logicalArray2Data(n)) &
         ) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (logicalArray) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (logicalArray) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (logicalArray) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (logicalArray) call.'
    WRITE(6,*) ' expected ',logicalArray1Data,'; recved ',logicalArray2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (isEqual .AND. (int4Var1Data == int4Var2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (int4Variable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (int4Variable) call.'
    WRITE(6,*) 'PASS:  TestForMessageCompletion (int4Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (int4Variable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (int4Variable) call.'
    WRITE(6,*) 'FAIL:  TestForMessageCompletion (int4Variable) call.'
    WRITE(6,*) ' expected ',int4Var2Data,'; recved ',int4Var1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=int4ArrayMin,int4ArrayMax,int4ArrayDelta
    IF (isEqual .AND. (int4Array1Data(n) == int4Array2Data(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (integer4Array) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (integer4Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (integer4Array) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (integer4Array) call.'
    WRITE(6,*) ' expected ',int4Array1Data,'; recved ',int4Array2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (runInt8Tests) THEN

    IF (isEqual .AND. (int8Var1Data == int8Var2Data)) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF

    IF (isEqual) THEN
     WRITE(6,*) 'PASS:  SendMessageToNode (int8Variable) call.'
     WRITE(6,*) 'PASS:  RecvMessageFromNode (int8Variable) call.'
     WRITE(6,*) 'PASS:  TestForMessageCompletion (int8Variable) call.'
    ELSE
     WRITE(6,*) 'FAIL:  SendMessageToNode (int8Variable) call.'
     WRITE(6,*) 'FAIL:  RecvMessageFromNode (int8Variable) call.'
     WRITE(6,*) 'FAIL:  TestForMessageCompletion (int8Variable) call.'
     WRITE(6,*) ' expected ',int8Var2Data,'; recved ',int8Var1Data
     passedTest = .FALSE.
     GO TO 100
    END IF

    DO n=int8ArrayMin,int8ArrayMax,int8ArrayDelta
     IF (isEqual .AND. (int8Array1Data(n) == int8Array2Data(n))) THEN
      isEqual = .TRUE.
     ELSE
      isEqual = .FALSE.
     END IF
    END DO

    IF (isEqual) THEN
     WRITE(6,*) 'PASS:  SendMessageToNode (integer8Array) call.'
     WRITE(6,*) 'PASS:  RecvMessageFromNode (integer8Array) call.'
    ELSE
     WRITE(6,*) 'FAIL:  SendMessageToNode (integer8Array) call.'
     WRITE(6,*) 'FAIL:  RecvMessageFromNode (integer8Array) call.'
     WRITE(6,*) ' expected ',int8Array1Data,'; recved ',int8Array2Data
     passedTest = .FALSE.
     GO TO 100
    END IF

   END IF

   IF (isEqual .AND. (real4Var1Data == real4Var2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (real4Variable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (real4Variable) call.'
    WRITE(6,*) 'PASS:  TestForMessageCompletion (real4Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (real4Variable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (real4Variable) call.'
    WRITE(6,*) 'FAIL:  TestForMessageCompletion (real4Variable) call.'
    WRITE(6,*) ' expected ',real4Var2Data,'; recved ',real4Var1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=real4ArrayMin,real4ArrayMax,real4ArrayDelta
    IF (isEqual .AND. (real4Array1Data(n) == real4Array2Data(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (real4Array) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (real4Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (real4Array) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (real4Array) call.'
    WRITE(6,*) ' expected ',real4Array1Data,'; recved ',real4Array2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=real8ArrayMin,real8ArrayMax,real8ArrayDelta
    IF (isEqual .AND. (real8Array1Data(n) == real8Array2Data(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (real8Array) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (real8Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (real8Array) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (real8Array) call.'
    WRITE(6,*) ' expected ',real8Array1Data,'; recved ',real8Array2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (isEqual .AND. (real8Var1Data == real8Var2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (real8Variable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (real8Variable) call.'
    WRITE(6,*) 'PASS:  TestForMessageCompletion (real8Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (real8Variable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (real8Variable) call.'
    WRITE(6,*) 'FAIL:  TestForMessageCompletion (real8Variable) call.'
    WRITE(6,*) ' expected ',real8Var2Data,'; recved ',real8Var1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   charVar1Data = 'E'
   charVar2Data = 'F'
   charArray1Data = (/'E','F','G','H','I'/)
   charArray2Data = (/'F','G','H','I','J'/)
   logicalVar1Data = .FALSE.
   logicalVar2Data = .TRUE.
   logicalArray1Data = (/.TRUE.,.TRUE.,.TRUE.,.FALSE./)
   logicalArray2Data = (/.FALSE.,.TRUE.,.TRUE.,.TRUE./)
   int4Var1Data = 203_4
   int4Var2Data = 420_4
   int4Array1Data = (/3_4, 4_4, 5_4, 6_4, 7_4/)
   int4Array2Data = (/4_4, 5_4, 6_4, 7_4, 8_4/)
   int8Var1Data = 203_8
   int8Var2Data = 820_8
   int8Array1Data = (/3_8, 8_8, 5_8, 6_8, 7_8/)
   int8Array2Data = (/8_8, 5_8, 6_8, 7_8, 8_8/)
   real4Var1Data = 203.1_4
   real4Var2Data = 420.1_4
   real4Array1Data = (/3.1_4, 4.1_4, 5.1_4, 6.1_4, 7.1_4/)
   real4Array2Data = (/4.1_4, 5.1_4, 6.1_4, 7.1_4, 8.1_4/)
   real8Var1Data = 203.23_8
   real8Var2Data = 420.23_8
   real8Array1Data = (/3.23_8, 4.23_8, 5.23_8, 6.23_8, 7.23_8/)
   real8Array2Data = (/4.23_8, 5.23_8, 6.23_8, 7.23_8, 8.23_8/)
  ELSE
   charVar1Data = 'G'
   charVar2Data = 'H'
   charArray1Data = (/'G','H','I','J','K'/)
   charArray2Data = (/'H','I','J','K','L'/)
   logicalVar1Data = .TRUE.
   logicalVar2Data = .FALSE.
   logicalArray1Data = (/.TRUE.,.FALSE.,.TRUE.,.FALSE./)
   logicalArray2Data = (/.FALSE.,.TRUE.,.FALSE.,.TRUE./)
   int4Var1Data = -23_4
   int4Var2Data = -42_4
   int4Array1Data = (/1_4, 2_4, 3_4, 4_4, 5_4/)
   int4Array2Data = (/2_4, 3_4, 4_4, 5_4, 6_4/)
   int8Var1Data = -23_8
   int8Var2Data = -82_8
   int8Array1Data = (/1_8, 2_8, 3_8, 8_8, 5_8/)
   int8Array2Data = (/2_8, 3_8, 8_8, 5_8, 6_8/)
   real4Var1Data = -23.1_4
   real4Var2Data = -42.1_4
   real4Array1Data = (/1.1_4, 2.1_4, 3.1_4, 4.1_4, 5.1_4/)
   real4Array2Data = (/2.1_4, 3.1_4, 4.1_4, 5.1_4, 6.1_4/)
   real8Var1Data = -23.23_8
   real8Var2Data = -42.23_8
   real8Array1Data = (/1.23_8, 2.23_8, 3.23_8, 4.23_8, 5.23_8/)
   real8Array2Data = (/2.23_8, 3.23_8, 4.23_8, 5.23_8, 6.23_8/)
  END IF

!-------------broadcast message-------------------------------

  broadcastingNodeID = 1
  messageLength = 5
  messageTag = 25

   CALL ChangeMessageSendingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         sendingNodeID     = sendingNodeID,     &
         enableChecking    = enableChecking,    &
         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,    &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  CALL BroadcastMessage(nodeMPIDataObject    = mpiData,            &
                        messageObject        = structMessage2,     &
                        enableChecking       = enableChecking,     &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   IF (masterNode .AND. verbose)  &
      WRITE(6,*) 'PASS:  BroadcastMessage (characterArray) call.'

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = structMessage2,       &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

!----------------------------end of broadcast message----------------------

  IF (myMPINodeID == 1) THEN
   sendingNodeID = 1
   recvingNodeID = 0
   messageTag = 2

   CALL ChangeMessageSendingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         sendingNodeID     = sendingNodeID,     &
         enableChecking    = enableChecking,    &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageRecvingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         recvingNodeID     = recvingNodeID,     &
         enableChecking    = enableChecking,    &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage2,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,    &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL SendMessageToNode(                       &
         nodeMPIDataObject = mpiData,            &
         messageObject     = structMessage2,     &
         enableChecking    = enableChecking,     &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = structMessage2,       &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  ELSE IF (myMPINodeID == 0) THEN

   sendingNodeID = 1
   recvingNodeID = 0
   messageTag = 2

   CALL ChangeMessageSendingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage1,    &
         sendingNodeID     = sendingNodeID,     &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageRecvingNodeID(             &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage1,    &
         recvingNodeID     = recvingNodeID,     &
         enableChecking    = enableChecking,    &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = structMessage1,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,     &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL RecvMessageFromNode(                                          &
         nodeMPIDataObject             = mpiData,                     &
         messageObject                 = structMessage1,              &
         enableChecking                = enableChecking,     &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = structMessage1,       &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   messageLength = SIZE(charArray1Data,1)

   IF (isEqual .AND. (charVar1Data == charVar2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (characterVariable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (characterVariable) call.'
    WRITE(6,*) 'PASS:  WaitForMessageCompletion (characterVariable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (characterVariable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (characterVariable) call.'
    WRITE(6,*) 'FAIL:  WaitForMessageCompletion (characterVariable) call.'
    WRITE(6,*) ' expected ',charVar1Data,'; recved ',charVar2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=charArrayMin,charArrayMax,charArrayDelta
    IF (isEqual  &
         .AND.  &
        (charArray1Data(n) == charArray2Data(n)) &
         ) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (characterArray) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (characterArray) call.'
    WRITE(6,*) 'PASS:  WaitForMessageCompletion (characterArray) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (characterArray) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (characterArray) call.'
    WRITE(6,*) 'FAIL:  WaitForMessageCompletion (characterArray) call.'
    WRITE(6,*) ' expected ',charArray2Data,'; recved ',charArray1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (isEqual .AND. (logicalVar1Data .EQV. logicalVar2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (logicalVariable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (logicalVariable) call.'
    WRITE(6,*) 'PASS:  TestForMessageCompletion (logicalVariable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (logicalVariable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (logicalVariable) call.'
    WRITE(6,*) 'FAIL:  TestForMessageCompletion (logicalVariable) call.'
    WRITE(6,*) ' expected ',logicalVar2Data,'; recved ',logicalVar1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=logicalArrayMin,logicalArrayMax,logicalArrayDelta
    IF (isEqual  &
         .AND.  &
    (logicalArray1Data(n) .EQV. logicalArray2Data(n)) &
         ) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (logicalArray) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (logicalArray) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (logicalArray) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (logicalArray) call.'
    WRITE(6,*) ' expected ',logicalArray1Data,'; recved ',logicalArray2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (isEqual .AND. (int4Var1Data == int4Var2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (int4Variable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (int4Variable) call.'
    WRITE(6,*) 'PASS:  TestForMessageCompletion (int4Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (int4Variable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (int4Variable) call.'
    WRITE(6,*) 'FAIL:  TestForMessageCompletion (int4Variable) call.'
    WRITE(6,*) ' expected ',int4Var2Data,'; recved ',int4Var1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=int4ArrayMin,int4ArrayMax,int4ArrayDelta
    IF (isEqual .AND. (int4Array1Data(n) == int4Array2Data(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (integer4Array) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (integer4Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (integer4Array) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (integer4Array) call.'
    WRITE(6,*) ' expected ',int4Array1Data,'; recved ',int4Array2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (runInt8Tests) THEN

    IF (isEqual .AND. (int8Var1Data == int8Var2Data)) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF

    IF (isEqual) THEN
     WRITE(6,*) 'PASS:  SendMessageToNode (int8Variable) call.'
     WRITE(6,*) 'PASS:  RecvMessageFromNode (int8Variable) call.'
     WRITE(6,*) 'PASS:  TestForMessageCompletion (int8Variable) call.'
    ELSE
     WRITE(6,*) 'FAIL:  SendMessageToNode (int8Variable) call.'
     WRITE(6,*) 'FAIL:  RecvMessageFromNode (int8Variable) call.'
     WRITE(6,*) 'FAIL:  TestForMessageCompletion (int8Variable) call.'
     WRITE(6,*) ' expected ',int8Var2Data,'; recved ',int8Var1Data
     passedTest = .FALSE.
     GO TO 100
    END IF

    DO n=int8ArrayMin,int8ArrayMax,int8ArrayDelta
     IF (isEqual .AND. (int8Array1Data(n) == int8Array2Data(n))) THEN
      isEqual = .TRUE.
     ELSE
      isEqual = .FALSE.
     END IF
    END DO

    IF (isEqual) THEN
     WRITE(6,*) 'PASS:  SendMessageToNode (integer8Array) call.'
     WRITE(6,*) 'PASS:  RecvMessageFromNode (integer8Array) call.'
    ELSE
     WRITE(6,*) 'FAIL:  SendMessageToNode (integer8Array) call.'
     WRITE(6,*) 'FAIL:  RecvMessageFromNode (integer8Array) call.'
     WRITE(6,*) ' expected ',int8Array1Data,'; recved ',int8Array2Data
     passedTest = .FALSE.
     GO TO 100
    END IF

   END IF

   IF (isEqual .AND. (real4Var1Data == real4Var2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (real4Variable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (real4Variable) call.'
    WRITE(6,*) 'PASS:  TestForMessageCompletion (real4Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (real4Variable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (real4Variable) call.'
    WRITE(6,*) 'FAIL:  TestForMessageCompletion (real4Variable) call.'
    WRITE(6,*) ' expected ',real4Var2Data,'; recved ',real4Var1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=real4ArrayMin,real4ArrayMax,real4ArrayDelta
    IF (isEqual .AND. (real4Array1Data(n) == real4Array2Data(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (real4Array) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (real4Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (real4Array) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (real4Array) call.'
    WRITE(6,*) ' expected ',real4Array1Data,'; recved ',real4Array2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (isEqual .AND. (real8Var1Data == real8Var2Data)) THEN
    isEqual = .TRUE.
   ELSE
    isEqual = .FALSE.
   END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (real8Variable) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (real8Variable) call.'
    WRITE(6,*) 'PASS:  TestForMessageCompletion (real8Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (real8Variable) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (real8Variable) call.'
    WRITE(6,*) 'FAIL:  TestForMessageCompletion (real8Variable) call.'
    WRITE(6,*) ' expected ',real8Var2Data,'; recved ',real8Var1Data
    passedTest = .FALSE.
    GO TO 100
   END IF

   DO n=real8ArrayMin,real8ArrayMax,real4ArrayDelta
    IF (isEqual .AND. (real8Array1Data(n) == real8Array2Data(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (real8Array) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (real8Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (real8Array) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (real8Array) call.'
    WRITE(6,*) ' expected ',real8Array1Data,'; recved ',real8Array2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! finally, we test all possible permutations for 2D, 3D, 4D arrays.

! 2D first



!622 CONTINUE

  nodesAreSynchronized = SynchronizeMPINodes(mpiData = mpiData)

!-------------all done; destroy the message objects

  CALL DestroyObject(                         &
        object            = structMessage1, &
        nodeMPIDataObject = mpiData,                     &
        enableChecking    = enableChecking,     &
        errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (DestroyObject (1)) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  CALL DestroyObject(                         &
        object           = structMessage2, &
        nodeMPIDataObject         = mpiData,                     &
        enableChecking   = enableChecking,     &
        errorInfoObject  = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (DestroyObject (1)) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

! try the 4D arrays

  IF (masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'About to test the 4D array passing.'
   WRITE(6,*)
  END IF 
 
  iOrder1(1) = 1
  iOrder1(2) = 2
  iOrder1(3) = 3
  iOrder1(4) = 4

  iOrder2(1) = 4
  iOrder2(2) = 3
  iOrder2(3) = 1
  iOrder2(4) = 2

  iOrderT(1) = 1
  iOrderT(2) = 2
  iOrderT(3) = 3
  iOrderT(4) = 4

  iStart1(1) = 1
  iStart1(2) = 1
  iStart1(3) = 1
  iStart1(4) = 1
   
  iEnd1(1) = 4
  iEnd1(2) = 3
  iEnd1(3) = 2
  iEnd1(4) = 5

  iDelta1(1) = 1
  iDelta1(2) = 1
  iDelta1(3) = 1
  iDelta1(4) = 1
   
  iStart2(1) = 2
  iStart2(2) = 1
  iStart2(3) = 1
  iStart2(4) = 4
   
  iEnd2(1) = 1
  iEnd2(2) = 5
  iEnd2(3) = 3
  iEnd2(4) = 1

  iDelta2(1) = -1
  iDelta2(2) =  1
  iDelta2(3) =  1
  iDelta2(4) = -1
   
  iStartT(1) = 1
  iStartT(2) = 1
  iStartT(3) = 1
  iStartT(4) = 1
   
  iEndT(1) = 2
  iEndT(2) = 5
  iEndT(3) = 3
  iEndT(4) = 4

  iDeltaT(1) = 1
  iDeltaT(2) = 1
  iDeltaT(3) = 1
  iDeltaT(4) = 1
   
! REAL(KIND=8), DIMENSION(4,3,2,5) :: real8Array14DData
! REAL(KIND=8), DIMENSION(2,5,3,4) :: real8Array24DData, &
!                                     real8Array24DTest

! initialize arrays with random numbers.
!
! 4D arrays : 
!  array 1 -> 2:  i => -l
!                 j =>  k 
!                 k => -i
!                 l =>  j

  DO i=1,4
   DO j=1,4
    tMat(i,j) = 0
   END DO
  END DO

  tMat(4,1) = -1
  tMat(3,2) =  1
  tMat(2,4) =  1
  tMat(1,3) = -1

  DO l=iStart1(4),iEnd1(4),iDelta1(4)
   DO k=iStart1(3),iEnd1(3),iDelta1(3)
    DO j=iStart1(2),iEnd1(2),iDelta1(2)
     DO i=iStart1(1),iEnd1(1),iDelta1(1)
      CALL RANDOM_NUMBER(real8Array14DData(i,j,k,l))
      dI1 = i-iStart1(1)
      dJ1 = j-iStart1(2)
      dK1 = k-iStart1(3)
      dL1 = l-iStart1(4)

      dI2 = tMat(1,1)*dI1 &
          + tMat(1,2)*dJ1 &
          + tMat(1,3)*dK1 &
          + tMat(1,4)*dL1

      dJ2 = tMat(2,1)*dI1 &
          + tMat(2,2)*dJ1 &
          + tMat(2,3)*dK1 &
          + tMat(2,4)*dL1

      dK2 = tMat(3,1)*dI1 &
          + tMat(3,2)*dJ1 &
          + tMat(3,3)*dK1 &
          + tMat(3,4)*dL1

      dL2 = tMat(4,1)*dI1 &
          + tMat(4,2)*dJ1 &
          + tMat(4,3)*dK1 &
          + tMat(4,4)*dL1

      ii = iStart2(1) + dI2
      jj = iStart2(2) + dJ2
      kk = iStart2(3) + dK2
      ll = iStart2(4) + dL2

      CALL RANDOM_NUMBER(real8Array24DData(ii,jj,kk,ll))
      real8Array24DTest(ii,jj,kk,ll) = real8Array14DData(i,j,k,l)
     END DO
    END DO
   END DO
  END DO

! passing real8Array1 to real8Array2, then passing
!  real8Array2Test to real8Array2Test and checking.
!
  
  IF (masterNode) WRITE(6,*) 'creating struct4DMessage1.'

! WRITE(6,*) 'Before'
! IF (masterNode) THEN

! try the 4D arrays
!  DO l=iStartT(4),iEndT(4),iDeltaT(4)
!   DO k=iStartT(3),iEndT(3),iDeltaT(3)
!    DO j=iStartT(2),iEndT(2),iDeltaT(2)
!     DO i=iStartT(1),iEndT(1),iDeltaT(1)

!      WRITE(6,*) i,j,k,l,real8Array24DData(i,j,k,l),real8Array24DTest(i,j,k,l)

!     END DO
!    END DO
!   END DO
!  END DO
! END IF
! WRITE(6,*) 

  sendingNodeID = 1
  recvingNodeID = 0

  CALL CreateObject(object            = struct4DMessage1,   &
                    nodeMPIDataObject = mpiData,            &
                    messageTag        = messageTag,         &
                    messageTypeID     = messageTypeID2,     &
                    sendingNodeID     = sendingNodeID,      & 
                    recvingNodeID     = recvingNodeID,      &
                    enableChecking    = enableChecking,     &
                    errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject creation (structMessage2) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct4DMessage1,    &
                                       messageData           = real8Array14DData,   &
                                       iOrder                = iOrder1,             &
                                       iStart                = iStart1,             &
                                       iEnd                  = iEnd1,               &
                                       deltaI                = iDelta1,             &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! message data is complete

  CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                        messageObject     = struct4DMessage1,    &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (structMessage1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! next message

  IF (masterNode) WRITE(6,*) 'creating struct4DMessage2.'

  sendingNodeID = 1
  recvingNodeID = 0

  CALL CreateObject(object            = struct4DMessage2,   &
                    nodeMPIDataObject = mpiData,            &
                    messageTag        = messageTag,         &
                    messageTypeID     = messageTypeID2,     &
                    sendingNodeID     = sendingNodeID,      & 
                    recvingNodeID     = recvingNodeID,      &
                    enableChecking    = enableChecking,     &
                    errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject creation (structMessage2) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct4DMessage2,    &
                                       messageData           = real8Array24DData,   &
                                       iOrder                = iOrder2,             &
                                       iStart                = iStart2,             &
                                       iEnd                  = iEnd2,               &
                                       deltaI                = iDelta2,             &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! message data is complete

  CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                   messageObject     = struct4DMessage2,    &
                                   enableChecking    = enableChecking,      &
                                   errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (structMessage1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (masterNode) WRITE(6,*) 'creating struct4DMessageT.'

  sendingNodeID = 1
  recvingNodeID = 0

  CALL CreateObject(object            = struct4DMessageT,   &
                    nodeMPIDataObject = mpiData,            &
                    messageTag        = messageTag,         &
                    messageTypeID     = messageTypeID2,     &
                    sendingNodeID     = sendingNodeID,      & 
                    recvingNodeID     = recvingNodeID,      &
                    enableChecking    = enableChecking,     &
                    errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject creation (structMessage2) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct4DMessageT,    &
                                       messageData           = real8Array24DTest,   &
                                       iOrder                = iOrderT,             &
                                       iStart                = iStartT,             &
                                       iEnd                  = iEndT,               &
                                       deltaI                = iDeltaT,             &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition (characterArray2) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! message data is complete

  CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                        messageObject     = struct4DMessageT,    &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (structMessage1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! check the sizes

  CALL GetMPIStructureSize(nodeMPIDataObject = mpiData,             &
                           messageObject     = struct4DMessageT,    &
                           structureSize     = structureSize,       &
                           enableChecking    = enableChecking,      &
                           errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (structMessage1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (masterNode) THEN
   WRITE(6,*) ' The structure size for struct4DMessageT is ',structureSize,' bytes.'
  END IF

! send messages

  IF (myMPINodeID == 1) THEN

   messageTag = 5

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = struct4DMessage1,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,     &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   messageTag = 2

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = struct4DMessageT,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,     &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL SendMessageToNode(                       &
         nodeMPIDataObject = mpiData,            &
         messageObject     = struct4DMessage1,   &
         enableChecking    = enableChecking,     &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL SendMessageToNode(                       &
         nodeMPIDataObject = mpiData,            &
         messageObject     = struct4DMessageT,   &
         enableChecking    = enableChecking,     &
         errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = struct4DMessage1,     &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = struct4DMessageT,     &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  ELSE IF (myMPINodeID == 0) THEN

! recv messages
   messageTag = 5

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = struct4DMessage2,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,     &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   messageTag = 2

   CALL ChangeMessageTag(                       &
         nodeMPIDataObject = mpiData,           &
         messageObject     = struct4DMessageT,    &
         messageTag        = messageTag,        &
         enableChecking    = enableChecking,     &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL RecvMessageFromNode(                                          &
         nodeMPIDataObject             = mpiData,                     &
         messageObject                 = struct4DMessage2,            &
         enableChecking                = enableChecking,              &
         errorInfoObject               = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL RecvMessageFromNode(                                          &
         nodeMPIDataObject             = mpiData,                     &
         messageObject                 = struct4DMessageT,            &
         enableChecking                = enableChecking,              &
         errorInfoObject               = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = struct4DMessage2,     &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                     &
                         nodeMPIDataObject    = mpiData,              &
                         messageObject        = struct4DMessageT,     &
                         maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                         enableChecking       = enableChecking,       &
                         errorInfoObject      = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  END IF

! test the data

  IF (masterNode) THEN

   CALL CheckDataValue(dataValue         = real8Array24DData, &
                       expectedDataValue = real8Array24DTest, &
                       correctValue      = isEqual,           &
                       errorLocation     = errorLoc5D,        &
                       errorInfoObject   = errorInfoObject)

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  SendMessageToNode (array4D) call.'
    WRITE(6,*) 'PASS:  RecvMessageFromNode (array4D) call.'
   ELSE
    WRITE(6,*) 'FAIL:  SendMessageToNode (array4D) call.'
    WRITE(6,*) 'FAIL:  RecvMessageFromNode (array4D) call.'
!   WRITE(6,*) ' expected ',real8Array1Data,'; recved ',real8Array2Data
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! all done

  CALL DestroyObject(                         &
        object            = struct4DMessage1, &
        nodeMPIDataObject = mpiData,          &
        enableChecking    = enableChecking,   &
        errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (DestroyObject (1)) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  CALL DestroyObject(                         &
        object            = struct4DMessage2, &
        nodeMPIDataObject = mpiData,          &
        enableChecking    = enableChecking,   &
        errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (DestroyObject (1)) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  CALL DestroyObject(                         &
        object            = struct4DMessageT, &
        nodeMPIDataObject = mpiData,          &
        enableChecking    = enableChecking,   &
        errorInfoObject   = errorInfoObject)

   IF (CheckForGlobalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (DestroyObject (1)) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  MPI_STRUCT class message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'About to fully test the 1D array passing (real data).'
   WRITE(6,*)
  END IF 

  CALL Test1DArrayPassing(mpiData              = mpiData,              &
                          masterNode           = masterNode,           &
                          passedTest           = passedTest,           &
                          verbose              = verbose,              &
                          maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                          enableChecking       = enableChecking,       &
                          errorInfoObject      = errorInfoObject)

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  1D array passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'About to fully test the 2D array passing (real data).'
   WRITE(6,*)
  END IF 

  CALL Test2DArrayPassing(mpiData              = mpiData,              &
                          masterNode           = masterNode,           &
                          passedTest           = passedTest,           &
                          verbose              = verbose,              &
                          maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                          enableChecking       = enableChecking,       &
                          errorInfoObject      = errorInfoObject)

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  2D array passing.'
   WRITE(6,*)
  END IF

  IF (test3D) THEN

   IF (masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'About to fully test the 3D array passing (real data).'
    WRITE(6,*)
   END IF 

   CALL Test3DArrayPassing(mpiData              = mpiData,              &
                           masterNode           = masterNode,           &
                           passedTest           = passedTest,           &
                           verbose              = verbose,              &
                           maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                           enableChecking       = enableChecking,       &
                           errorInfoObject      = errorInfoObject)

   IF (passedTest .AND. masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'PASS:  3D array passing.'
    WRITE(6,*)
   END IF

  ELSE
   IF (masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'Skipping the 3D array passing test (real data).'
    WRITE(6,*)
   END IF 
  END IF

  IF (test4D) THEN
   IF (masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'About to fully test the 4D array passing (real data).'
    WRITE(6,*)
   END IF 

   CALL Test4DArrayPassing(mpiData              = mpiData,              &
                           masterNode           = masterNode,           &
                           passedTest           = passedTest,           &
                           verbose              = verbose,              &
                           maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                           enableChecking       = enableChecking,       &
                           errorInfoObject      = errorInfoObject)

   IF (passedTest .AND. masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'PASS:  4D array passing.'
    WRITE(6,*)
   END IF
  ELSE
   IF (masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'Skipping the 4D array passing test (real data).'
    WRITE(6,*)
   END IF 
  END IF

  IF (test5D) THEN
   IF (masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'About to fully test the 5D array passing (real data).'
    WRITE(6,*)
   END IF 

   CALL Test5DArrayPassing(mpiData              = mpiData,              &
                           masterNode           = masterNode,           &
                           passedTest           = passedTest,           &
                           verbose              = verbose,              &
                           maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                           enableChecking       = enableChecking,       &
                           errorInfoObject      = errorInfoObject)

   IF (passedTest .AND. masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'PASS:  5D array passing.'
    WRITE(6,*)
   END IF
  ELSE
   IF (masterNode) THEN
    WRITE(6,*)
    WRITE(6,*) 'Skipping the 5D array passing test (real data).'
    WRITE(6,*)
   END IF 
  END IF

 100 CONTINUE

  nodesAreSynchronized = SynchronizeMPINodes(mpiData = mpiData)

  IF (nodesAreSynchronized) THEN
   IF (masterNode) WRITE(6,*) 'PASS:  SynchronizeMPINodes call.' 
  ELSE
   passedTest = .FALSE.
!  GO TO 100
   IF (masterNode) WRITE(6,*) 'FAIL:  SynchronizeMPINodes call.' 
  END IF

800 CONTINUE

  IF (CheckForGlobalError(errorInfoObject)) THEN
   CALL WriteObject(object = errorInfoObject)
  END IF

  finalTime = GetMPITimerValue(mpiData = mpiData)
  IF (masterNode) WRITE(6,102) finalTime - initialTime

  finalTime = GetMPITotalTimeSinceInit(mpiData = mpiData)
  IF (masterNode) WRITE(6,1025) finalTime 

  IF (verbose .AND. masterNode) WRITE(6,*) 'Testing MPIDataClass shutdown'

  CALL DestroyObject(object           = mpiData,        &
                     verbose          = verbose,        &
                     enableChecking   = enableChecking, &
                     errorDetected    = errorDetected,  &
                     errorInformation = errorInformation)

  IF (errorDetected) THEN
   passedTest = .FALSE.
   IF (verbose .AND. masterNode)  THEN
     WRITE(6,*) 'FAIL:  Error detected when destroying MPIDataObject: ', &
       TRIM(errorInformation)
   END IF
  END IF

  IF (passedTest) THEN
   IF (masterNode) THEN
    WRITE(6,999)
    OPEN(33,FILE = 'MPI2DataClassNoErrClassTest.PASSED', FORM = 'FORMATTED')
    WRITE(33,999)
    CLOSE(33)
   END IF
  ELSE
   IF (masterNode) THEN
    WRITE(6,998)
    OPEN(33,FILE = 'MPI2DataClassNoErrClassTest.FAILED', FORM = 'FORMATTED')
    WRITE(33,998)
    CLOSE(33)
   END IF
  END IF

 101 FORMAT(1x,'MPI timing precision = ',e13.5,' seconds.')
 102 FORMAT(1x,'Testing program took ',f10.5,' seconds (from time calls).')
 1025 FORMAT(1x,'Testing program took ',f10.5,' seconds (from total time routine).')
 103 FORMAT(1x,/,&
 1x,' ----------------------------------------- ',/,&
 1x,'| MPI Data Object testing routine         |',/,&
 1x,'|  copyright 2007 by Hixon Technology     |',/,&
 1x,'|  All rights reserved.                   |',/,&
 1x,'|  No warranty for this code express      |',/,&
 1x,'|    or implied.                          |',/,&
 1x,'|  contact:  Ray Hixon                    |',/,&
 1x,'|   email: rhixon@wideopenwest.com        |',/,&
 1x,' ----------------------------------------- ',/,&
 1x,' -WARNING:  testing routine is for TWO   - ',/,&
 1x,' - nodes only.                           - ',/,&
 1x,' ----------------------------------------- ',/,&
 1x)
 104 FORMAT(1x,'Number of MPI nodes detected = ',i8)
 105 FORMAT(1x,'MPI Version = ',i5,'.',i5)

 998 FORMAT(1x,'FAIL:  Testing routine FAILED test.',/)
 999 FORMAT(1x,'PASS:  Testing routine PASSED all tests.',/)

  STOP
END PROGRAM MPI2DataClassNoErrClassTest
