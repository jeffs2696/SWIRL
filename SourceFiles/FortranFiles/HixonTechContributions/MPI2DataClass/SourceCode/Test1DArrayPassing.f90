MODULE Test1DArrayPassingModule

  USE MPI2DataClassNoErrClass           ! MPI2DataClassNoErrClass.f90
  USE ErrorInformationClass             ! ErrorInformationClass.f90
  USE Check1DData

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Test1DArrayPassing

CONTAINS

SUBROUTINE Test1DArrayPassing(mpiData,              &
                              masterNode,           &
                              passedTest,           &
                              verbose,              &
                              maxWaitTimeInSeconds, &
                              enableChecking,       &
                              errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(INOUT) :: mpiData
  LOGICAL, INTENT(IN) :: masterNode, verbose, enableChecking
  LOGICAL, INTENT(INOUT) :: passedTest
  DOUBLE PRECISION, INTENT(IN) :: maxWaitTimeInSeconds
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  TYPE(CharacterStringType) :: charStringObject

  LOGICAL :: isEqual

  TYPE(structMessageObjectType) :: struct1DMessage1 !, &
!                                   struct1DMessage2

  REAL(KIND=8), DIMENSION(15) :: real8Array1DData1,    &
                                 real8Array1DData2,    &
                                 real8Array1DDataRecv, &
                                 real8Array1DDataExpected

  INTEGER :: messageTag,    &
             messageTypeID, &
             sendingNodeID, &
             recvingNodeID

  INTEGER :: n,       &
             nn,      &
             nDir,    &
             nStride, &
             nStart,  &
             nEnd

  INTEGER, DIMENSION(1) :: errorLoc1D

  INTEGER :: iStart1D1, &
             iEnd1D1,   &
             iDelta1D1 !, &
!            iStart1D2, &
!            iEnd1D2,   &
!            iDelta1D2, &
!            iStart1DT, &
!            iEnd1DT,   &
!            iDelta1DT


  INTEGER, DIMENSION(2) :: start1D,  &
                           end1D,    &
                           stride1D, &
                           direction1D

  CONTINUE ! execution starts here

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*) '--STARTING 1D Array Testing (Double Precision)--'
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*)
  END IF

  messageTag = 1
  messageTypeID = 1

  isEqual = .TRUE.

! generate the arrays and send out to the nodes

  start1D(1) = 1
  start1D(2) = 3
  end1D(1)   = 15
  end1D(2)   = 11

  stride1D(1) = 1
  stride1D(2) = 2

  direction1D(1) =  1
  direction1D(2) = -1

  DO n=1,15
   CALL RANDOM_NUMBER(real8Array1DData1(n))
   CALL RANDOM_NUMBER(real8Array1DData2(n))
  END DO

! both are doing the same thing

  sendingNodeID = 0
  recvingNodeID = 1

  CALL CreateObject(object            = struct1DMessage1,   &
                    nodeMPIDataObject = mpiData,            &
                    messageTag        = messageTag,         &
                    messageTypeID     = messageTypeID,      &
                    sendingNodeID     = sendingNodeID,      &
                    recvingNodeID     = recvingNodeID,      &
                    enableChecking    = enableChecking,     &
                    errorInfoObject   = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject creation (struct1DMessage1) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  iStart1D1 = 1
  iEnd1D1   = 15
  iDelta1D1 = 1

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct1DMessage1,    &
                                       messageData           = real8Array1DData1,   &
                                       iStart                = iStart1D1,           &
                                       iEnd                  = iEnd1D1,             &
                                       deltaI                = iDelta1D1,           &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition () call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  iStart1D1 = 1
  iEnd1D1   = 15
  iDelta1D1 = 1

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct1DMessage1,    &
                                       messageData           = real8Array1DData2,   &
                                       iStart                = iStart1D1,           &
                                       iEnd                  = iEnd1D1,             &
                                       deltaI                = iDelta1D1,           &
                                       enableChecking        = enableChecking,      &
                                       errorInfoObject       = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data addition () call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! message data is complete

  CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                        messageObject     = struct1DMessage1,    &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (struct1DMessage1) call.'
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  IF (CheckForGlobalError(errorInfoObject)) THEN
   GO TO 100
  END IF

! and send the data

  IF (masterNode) THEN

   CALL SendMessageToNode(                      &
         nodeMPIDataObject = mpiData,           &
         messageObject     = struct1DMessage1,  &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during struct1DMessage1 send call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                    &
                        nodeMPIDataObject    = mpiData,              &
                        messageObject        = struct1DMessage1,     &
                        maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                        enableChecking       = enableChecking,       &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF


  ELSE ! recv

   CALL RecvMessageFromNode(                    &
         nodeMPIDataObject = mpiData,           &
         messageObject     = struct1DMessage1,  &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
      'FAIL:  Error detected during (characterArray) call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                    &
                        nodeMPIDataObject    = mpiData,              &
                        messageObject        = struct1DMessage1,     &
                        maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                        enableChecking       = enableChecking,       &
                        errorInfoObject      = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
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

  IF (CheckForGlobalError(errorInfoObject)) THEN
   GO TO 100
  END IF

! destroy the object

  CALL DestroyObject(                         &
        object            = struct1DMessage1, &
        nodeMPIDataObject = mpiData,          &
        enableChecking    = enableChecking,   &
        errorInfoObject   = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during (DestroyObject (init)) call: '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

! At this point, both nodes have the same data in the real8Array1DData1
!   and real8Array1DData2 arrays.

  DO nDir = 1,2   
   DO nStride = 1,2   
    DO nStart = 1,2   
     DO nEnd = 1,2   

! both are doing the same thing

      sendingNodeID = 1
      recvingNodeID = 0

      CALL CreateObject(object            = struct1DMessage1,   &
                        nodeMPIDataObject = mpiData,            &
                        messageTag        = messageTag,         &
                        messageTypeID     = messageTypeID,      &
                        sendingNodeID     = sendingNodeID,      &
                        recvingNodeID     = recvingNodeID,      &
                        enableChecking    = enableChecking,     &
                        errorInfoObject   = errorInfoObject)

      IF (CheckForLocalError(errorInfoObject)) THEN
       passedTest = .FALSE.
       IF (verbose)  THEN
        WRITE(charStringObject%charString,'(a80)') &
         'FAIL:  Error detected during messageObject creation (struct1DMessage1) call. '
        CALL AddErrorInformation(object          = errorInfoObject, &
                                 errorInfoString = charStringObject)
       END IF
       GO TO 100
      END IF

      IF (masterNode) THEN
       DO n=1,15
        real8Array1DDataRecv(n)     = real8Array1DData1(n) 
        real8Array1DDataExpected(n) = real8Array1DData1(n) 
       END DO
       IF (direction1D(nDir) > 0) THEN
        iStart1D1 = start1D(nStart)
        iEnd1D1   = end1D(nEnd)
        iDelta1D1 = stride1D(nStride)
        nn = start1D(nStart)
        DO n=iStart1D1,iEnd1D1,iDelta1D1
         real8Array1DDataExpected(n) = real8Array1DData2(n)
        END DO
       ELSE
        iStart1D1 = start1D(nStart)
        iEnd1D1   = end1D(nEnd)
        iDelta1D1 = stride1D(nStride)
        nn = end1D(nEnd)
       END IF

!      WRITE(0,'(1x,a21,3(1x,i3),a7)') 'MASTER:Array1DTest (',       &
!                                       iStart1D1,                   &
!                                       iEnd1D1,                     &
!                                       iDelta1D1*direction1D(nDir), &
!                                       ') call.'

       DO n=iStart1D1,iEnd1D1,iDelta1D1
!       WRITE(0,*) n,nn
        real8Array1DDataExpected(n) = real8Array1DData2(nn)
        nn = nn + iDelta1D1*direction1D(nDir)
       END DO

       CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,              &
                                            structMessageObject   = struct1DMessage1,     &
                                            messageData           = real8Array1DDataRecv, &
                                            iStart                = iStart1D1,            &
                                            iEnd                  = iEnd1D1,              &
                                            deltaI                = iDelta1D1,            &
                                            enableChecking        = enableChecking,       &
                                            errorInfoObject       = errorInfoObject)

      ELSE ! node
       IF (direction1D(nDir) > 0) THEN
        iStart1D1 = start1D(nStart)
        iEnd1D1   = end1D(nEnd)
        iDelta1D1 = stride1D(nStride)
       ELSE
        iStart1D1 = end1D(nEnd)
        iEnd1D1   = start1D(nStart)
        iDelta1D1 = (stride1D(nStride)*direction1D(nDir))       
       END IF

       CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                            structMessageObject   = struct1DMessage1,    &
                                            messageData           = real8Array1DData2,   &
                                            iStart                = iStart1D1,           &
                                            iEnd                  = iEnd1D1,             &
                                            deltaI                = iDelta1D1,           &
                                            enableChecking        = enableChecking,      &
                                            errorInfoObject       = errorInfoObject)

      END IF

      IF (CheckForLocalError(errorInfoObject)) THEN
       passedTest = .FALSE.
       IF (verbose)  THEN
        WRITE(charStringObject%charString,'(a80)') &
         'FAIL:  Error detected during messageObject data addition () call.'
        CALL AddErrorInformation(object          = errorInfoObject, &
                                 errorInfoString = charStringObject)
       END IF
       GO TO 100
      END IF

! message data is complete

      CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                            messageObject     = struct1DMessage1,    &
                                            enableChecking    = enableChecking,      &
                                            errorInfoObject   = errorInfoObject)

      IF (CheckForLocalError(errorInfoObject)) THEN
       passedTest = .FALSE.
       IF (verbose)  THEN
        WRITE(charStringObject%charString,'(a80)') &
         'FAIL:  Error detected during messageObject data completion (struct1DMessage1) call.'
        CALL AddErrorInformation(object          = errorInfoObject, &
                                 errorInfoString = charStringObject)
       END IF
       GO TO 100
      END IF

      IF (CheckForGlobalError(errorInfoObject)) THEN
       GO TO 100
      END IF

! send the message

      IF (masterNode) THEN

       CALL RecvMessageFromNode(                    &
             nodeMPIDataObject = mpiData,           &
             messageObject     = struct1DMessage1,  &
             enableChecking    = enableChecking,    &
             errorInfoObject   = errorInfoObject)

       IF (CheckForLocalError(errorInfoObject)) THEN
        passedTest = .FALSE.
        IF (verbose)  THEN
         WRITE(charStringObject%charString,'(a80)') &
         'FAIL:  Error detected during struct1DMessage1 send call: '
         CALL AddErrorInformation(object          = errorInfoObject, &
                                  errorInfoString = charStringObject)
        END IF
        GO TO 100
       END IF

       CALL WaitForMessageCompletion(                                    &
                            nodeMPIDataObject    = mpiData,              &
                            messageObject        = struct1DMessage1,     &
                            maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                            enableChecking       = enableChecking,       &
                            errorInfoObject      = errorInfoObject)

       IF (CheckForLocalError(errorInfoObject)) THEN
        passedTest = .FALSE.
        IF (verbose)  THEN
         WRITE(charStringObject%charString,'(a80)') &
          'FAIL:  Error detected during (characterArray) call: '
         CALL AddErrorInformation(object          = errorInfoObject, &
                                  errorInfoString = charStringObject)
        END IF
        GO TO 100
       END IF

      ELSE ! recv

       CALL SendMessageToNode(                      &
             nodeMPIDataObject = mpiData,           &
             messageObject     = struct1DMessage1,  &
             enableChecking    = enableChecking,    &
             errorInfoObject   = errorInfoObject)

       IF (CheckForLocalError(errorInfoObject)) THEN
        passedTest = .FALSE.
        IF (verbose)  THEN
         WRITE(charStringObject%charString,'(a80)') &
          'FAIL:  Error detected during (characterArray) call: '
         CALL AddErrorInformation(object          = errorInfoObject, &
                                  errorInfoString = charStringObject)
        END IF
        GO TO 100
       END IF

       CALL WaitForMessageCompletion(                                    &
                            nodeMPIDataObject    = mpiData,              &
                            messageObject        = struct1DMessage1,     &
                            maxWaitTimeInSeconds = maxWaitTimeInSeconds, &
                            enableChecking       = enableChecking,       &
                            errorInfoObject      = errorInfoObject)

       IF (CheckForLocalError(errorInfoObject)) THEN
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

      IF (CheckForGlobalError(errorInfoObject)) THEN
       GO TO 100
      END IF

! destroy the object

      CALL DestroyObject(                         &
            object            = struct1DMessage1, &
            nodeMPIDataObject = mpiData,          &
            enableChecking    = enableChecking,   &
            errorInfoObject   = errorInfoObject)

      IF (CheckForGlobalError(errorInfoObject)) THEN
       passedTest = .FALSE.
       IF (verbose)  THEN
        WRITE(charStringObject%charString,'(a80)') &
         'FAIL:  Error detected during (DestroyObject (init)) call: '
        CALL AddErrorInformation(object          = errorInfoObject, &
                                 errorInfoString = charStringObject)
       END IF
       GO TO 100
      END IF

      IF (masterNode) THEN

       IF (direction1D(nDir) > 0) THEN
        iStart1D1 = start1D(nStart)
        iEnd1D1   = end1D(nEnd)
        iDelta1D1 = stride1D(nStride)
       ELSE
        iStart1D1 = end1D(nEnd)
        iEnd1D1   = start1D(nStart)
        iDelta1D1 = (stride1D(nStride)*direction1D(nDir))       
       END IF

       CALL CheckDataValue(dataValue         = real8Array1DDataRecv,     &
                           expectedDataValue = real8Array1DDataExpected, &
                           correctValue      = isEqual,                  &
                           errorLocation     = errorLoc1D,               &
                           errorInfoObject   = errorInfoObject)

       IF (isEqual) THEN
        WRITE(0,'(1x,a21,3(1x,i3),a7)') 'PASS:  Array1DTest (', &
                                         iStart1D1,             &
                                         iEnd1D1,               &
                                         iDelta1D1,             &
                                         ') call.'
       ELSE
        WRITE(0,'(1x,a21,3(1x,i3),a7)') 'FAIL:  Array1DTest (', &
                                         iStart1D1,             &
                                         iEnd1D1,               &
                                         iDelta1D1,             &
                                         ') call.'
        WRITE(0,*) 'Error location = ',errorLoc1D(1)
        WRITE(0,*) ' Found:    ',real8Array1DDataRecv(errorLoc1D(1))
        WRITE(0,*) ' Expected: ',real8Array1DDataExpected(errorLoc1D(1))
        DO n=1,15
         WRITE(0,*) n,real8Array1DDataRecv(n), &
                      real8Array1DDataExpected(n), &
                      real8Array1DData1(n), &
                      real8Array1DData2(n)
        END DO
        passedTest = .FALSE.
        GO TO 100
       END IF

      END IF

     END DO
    END DO
   END DO
  END DO

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*) '---PASSED 1D Array Testing (Double Precision)--'
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*)
  END IF

  RETURN

100 CONTINUE
  RETURN

END SUBROUTINE Test1DArrayPassing

END MODULE Test1DArrayPassingModule
