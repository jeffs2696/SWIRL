MODULE Test2DArrayPassingModule

  USE MPI2DataClassNoErrClass           ! MPI2DataClassNoErrClass.f90
  USE ErrorInformationClass             ! ErrorInformationClass.f90
  USE TMatModule
  USE Check2DData

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Test2DArrayPassing

CONTAINS

SUBROUTINE Test2DArrayPassing(mpiData,              &
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

  TYPE(structMessageObjectType) :: struct2DMessage1 !, &
!                                   struct2DMessage2

  INTEGER, PARAMETER :: matrixSize = 15

  REAL(KIND=8), DIMENSION(matrixSize,matrixSize) :: real8Array2DData1,    &
                                    real8Array2DData2,    &
                                    real8Array2DDataRecv, &
                                    real8Array2DDataExpected

  INTEGER :: messageTag,    &
             messageTypeID, &
             sendingNodeID, &
             recvingNodeID

  INTEGER :: i,       &
             j,       &
             n

  INTEGER :: iDirM, &
             jDirM, &
             iStrideM, &
             jStrideM, &
             iStartM, &
             jStartM, &
             iEndM, &
             jEndM

  INTEGER :: nn,       &
             ii,      &
             jj

  INTEGER, DIMENSION(2) :: nDir,        &
                           nStride,     &
                           nStart,      &
                           nEnd

  INTEGER, DIMENSION(2) :: errorLoc2D

  INTEGER, DIMENSION(2) :: iStart2DMaster, &
                           iEnd2DMaster,   &
                           iDelta2DMaster, &
                           iOrder2DMaster, &
                           iOrder2DSign, &
                           iStart2DNode, &
                           iEnd2DNode,   &
                           iDelta2DNode, &
                           iOrder2DNode, &
                           iMatchMaster, &
                           iMatchNode,   &
                           iNode,        &
                           dIMaster,     &
                           dINode !, &
!                           dI

  INTEGER, DIMENSION(2,2) :: start2D,  &
                             end2D,    &
                             stride2D, &
                             direction2D

  INTEGER, DIMENSION(2,2) :: tMat

  CONTINUE ! execution starts here

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*) '--STARTING 2D Array Testing (Double Precision)--'
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*)
  END IF

  messageTag = 1
  messageTypeID = 1

  isEqual = .TRUE.

! generate the arrays and send out to the nodes

  start2D(1,1) = 1
  start2D(1,2) = 3

  start2D(2,1) = 1
  start2D(2,2) = 5

  end2D(1,1)   = matrixSize
  end2D(1,2)   = matrixSize-4

  end2D(2,1)   = matrixSize
  end2D(2,2)   = matrixSize-2

  stride2D(1,1) = 1
  stride2D(1,2) = 2

  stride2D(2,1) = 1
  stride2D(2,2) = 2

  direction2D(1,1) =  1
  direction2D(1,2) = -1

  direction2D(2,1) =  1
  direction2D(2,2) = -1

  DO j=1,matrixSize
   DO i=1,matrixSize
    CALL RANDOM_NUMBER(real8Array2DData1(i,j))
    CALL RANDOM_NUMBER(real8Array2DData2(i,j))
   END DO
  END DO

! both are doing the same thing

  sendingNodeID = 0
  recvingNodeID = 1

  CALL CreateObject(object            = struct2DMessage1,   &
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
     'FAIL:  Error detected during messageObject creation (struct2DMessage1) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  DO n=1,2
   iStart2DMaster(n) = 1
   iEnd2DMaster(n)   = matrixSize
   iDelta2DMaster(n) = 1
   iOrder2DMaster(n) = n
  END DO

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct2DMessage1,    &
                                       messageData           = real8Array2DData1,   &
                                       iOrder                = iOrder2DMaster,            &
                                       iStart                = iStart2DMaster,            &
                                       iEnd                  = iEnd2DMaster,              &
                                       deltaI                = iDelta2DMaster,            &
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

  DO n=1,2
   iStart2DMaster(n) = 1
   iEnd2DMaster(n)   = matrixSize
   iDelta2DMaster(n) = 1
   iOrder2DMaster(n) = n
  END DO

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct2DMessage1,    &
                                       messageData           = real8Array2DData2,   &
                                       iOrder                = iOrder2DMaster,            &
                                       iStart                = iStart2DMaster,            &
                                       iEnd                  = iEnd2DMaster,              &
                                       deltaI                = iDelta2DMaster,            &
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
                                        messageObject     = struct2DMessage1,    &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (struct2DMessage1) call.'
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
         messageObject     = struct2DMessage1,  &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during struct2DMessage1 send call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                    &
                        nodeMPIDataObject    = mpiData,              &
                        messageObject        = struct2DMessage1,     &
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
         messageObject     = struct2DMessage1,  &
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
                        messageObject        = struct2DMessage1,     &
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
        object            = struct2DMessage1, &
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

! At this point, both nodes have the same data in the real8Array2DData1
!   and real8Array2DData2 arrays.

! go through the possible transformations

  DO j= 1,2 ! the sign will be set properly by the GetTMatrix routine

   IF (j == 0) CYCLE

   iOrder2DSign(2) = j

   DO i=-2,2

    IF (i == 0) CYCLE
    IF (ABS(i) == ABS(j)) CYCLE

    iOrder2DSign(1) = i

    DO n=1,2
     iOrder2DNode(n) = ABS(iOrder2DSign(n))
    END DO

    CALL GetTMatrix2D(tMatrix         = tMat,           &
                      iOrderSign      = iOrder2DSign,   &
                      enableChecking  = enableChecking, &
                      errorInfoObject = errorInfoObject)
 
    IF (masterNode) THEN
      WRITE(0,*) '-----------------------------------'
!     WRITE(0,*) 'From the Node: iOrderSign = ',iOrder2DSign(1:2)
      WRITE(0,*) 
      WRITE(0,*) 'To the Master, the transformation matrix is: '
      WRITE(0,*) 
      WRITE(0,*) '|',tMat(1,1),tMat(1,2),'|'
      WRITE(0,*) '|',tMat(2,1),tMat(2,2),'|'
      WRITE(0,*) 
    END IF

! for this transformation matrix, do all of the possibilities...

! i direction

    DO iDirM = 1,2   
     DO iStrideM = 1,2   
      DO iStartM = 1,2   
       DO iEndM = 1,2   
 
        IF (direction2D(1,iDirM) > 0) THEN
         iStart2DMaster(1) = start2D(1,iStartM)
         iEnd2DMaster(1)   = end2D(1,iEndM)
         iDelta2DMaster(1) = stride2D(1,iStrideM)
        ELSE
         iStart2DMaster(1) = end2D(1,iEndM)
         iEnd2DMaster(1)   = start2D(1,iStartM)
         iDelta2DMaster(1) = -stride2D(1,iStrideM)
        END IF

        nDir(1)    = iDirM
        nStride(1) = iStrideM
        nStart(1)  = iStartM
        nEnd(1)    = iEndM

! j direction

        DO jDirM = 1,2   
         DO jStrideM = 1,2   
          DO jStartM = 1,2   
           DO jEndM = 1,2   

!           IF (masterNode) WRITE(0,'(1x,a9,8(i3))') 'testing ', &
!                           iDirM,iStrideM,iStartM,iEndM, &
!                           jDirM,jStrideM,jStartM,jEndM

            IF (direction2D(2,jDirM) > 0) THEN
             iStart2DMaster(2) = start2D(2,jStartM)
             iEnd2DMaster(2)   = end2D(2,jEndM)
             iDelta2DMaster(2) = stride2D(2,jStrideM)
            ELSE
             iStart2DMaster(2) = end2D(2,jEndM)
             iEnd2DMaster(2)   = start2D(2,jStartM)
             iDelta2DMaster(2) = -stride2D(2,jStrideM)
            END IF

            nDir(2)    = jDirM
            nStride(2) = jStrideM
            nStart(2)  = jStartM
            nEnd(2)    = jEndM

! on the master, the start, end, stride, and directions are set...

! ... and on the master, we're loading in i,j order...

! this is where it gets needlessly interesting.

            iMatchMaster(1) = 1
            iMatchMaster(2) = 1

            DO jj=1,2
             DO ii=1,2
              IF (tMat(jj,ii) /= 0) THEN
               IF (tMat(jj,ii) > 0) THEN
                iMatchNode(ii) = 1
               ELSE
                iMatchNode(ii) = matrixSize ! both i and j are matrixSize long
               END IF
              END IF
             END DO
            END DO
              
!           IF (masterNode) THEN
!            WRITE(0,*) 'iMatchMaster: ',iMatchMaster(1:2)
!            WRITE(0,*) 'iMatchNode:   ',iMatchNode(1:2)
!           END IF

            DO n=1,2
             iOrder2DMaster(n) = n
             nn = iOrder2DNode(n) ! which coordinate direction on node is data
                                  !  coming from?
        
             IF (tMat(n,nn) > 0) THEN ! same direction
              iDelta2DNode(nn) = iDelta2DMaster(n)
             ELSE ! turn it around
              iDelta2DNode(nn) = -iDelta2DMaster(n)
             END IF

            END DO

! get the start locations

            dIMaster(1) = iStart2DMaster(1)-1
            dIMaster(2) = iStart2DMaster(2)-1

            dINode(1) = (dIMaster(1)*tMat(1,1) &
                        +dIMaster(2)*tMat(2,1))  
            dINode(2) = (dIMaster(1)*tMat(1,2) &
                        +dIMaster(2)*tMat(2,2))  

            iStart2DNode(1) = iMatchNode(1) + dINode(1)
            iStart2DNode(2) = iMatchNode(2) + dINode(2)
             
            dIMaster(1) = iEnd2DMaster(1)-1
            dIMaster(2) = iEnd2DMaster(2)-1

            dINode(1) = (dIMaster(1)*tMat(1,1) &
                        +dIMaster(2)*tMat(2,1))  
            dINode(2) = (dIMaster(1)*tMat(1,2) &
                        +dIMaster(2)*tMat(2,2))  

            iEnd2DNode(1) = iMatchNode(1) + dINode(1)
            iEnd2DNode(2) = iMatchNode(2) + dINode(2)

! now generate the messages

!           IF (masterNode) THEN
!            WRITE(0,*) 'on the Master, the data arrives in this order: ', &
!              iOrder2DMaster(1:2)
!            WRITE(0,'(6(1x,i4))') (iStart2DMaster(n), &
!                                   iEnd2DMaster(n),   &
!                                   iDelta2DMaster(n),n=1,2) 
!            WRITE(0,*) 'on the Node, the data is pulled in this order: ', &
!              iOrder2DNode(1:2)
!            WRITE(0,'(6(1x,i4))') (iStart2DNode(n), &
!                                   iEnd2DNode(n),   &
!                                   iDelta2DNode(n),n=1,2) 

!            WRITE(0,*)
!           END IF

!           GO TO 100

! both are doing the same thing

            sendingNodeID = 1
            recvingNodeID = 0

            CALL CreateObject(object            = struct2DMessage1,   &
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
               'FAIL:  Error detected during messageObject creation (struct2DMessage1) call. '
              CALL AddErrorInformation(object          = errorInfoObject, &
                                       errorInfoString = charStringObject)
             END IF
             GO TO 100
            END IF

            IF (masterNode) THEN
             DO jj=1,matrixSize
              DO ii=1,matrixSize
               real8Array2DDataRecv(ii,jj)     = real8Array2DData1(ii,jj) 
               real8Array2DDataExpected(ii,jj) = real8Array2DData1(ii,jj) 
              END DO
             END DO

! generate the _expected_ array

             DO jj=iStart2DMaster(2),iEnd2DMaster(2),iDelta2DMaster(2)
              DO ii=iStart2DMaster(1),iEnd2DMaster(1),iDelta2DMaster(1)

               dIMaster(1) = ii-1
               dIMaster(2) = jj-1

               dINode(1) = (dIMaster(1)*tMat(1,1) &
                           +dIMaster(2)*tMat(2,1))  
               dINode(2) = (dIMaster(1)*tMat(1,2) &
                           +dIMaster(2)*tMat(2,2))  

               iNode(1) = iMatchNode(1) + dINode(1)
               iNode(2) = iMatchNode(2) + dINode(2)

               real8Array2DDataExpected(ii,jj) = real8Array2DData2(iNode(1),iNode(2)) 
               
              END DO
             END DO

! generate the message

             CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,              &
                                                  structMessageObject   = struct2DMessage1,     &
                                                  messageData           = real8Array2DDataRecv, &
                                                  iOrder                = iOrder2DMaster,       &
                                                  iStart                = iStart2DMaster,       &
                                                  iEnd                  = iEnd2DMaster,         &
                                                  deltaI                = iDelta2DMaster,       &
                                                  enableChecking        = enableChecking,       &
                                                  errorInfoObject       = errorInfoObject)

            ELSE ! node
             CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                                  structMessageObject   = struct2DMessage1,    &
                                                  messageData           = real8Array2DData2,   &
                                                  iOrder                = iOrder2DNode,        &
                                                  iStart                = iStart2DNode,        &
                                                  iEnd                  = iEnd2DNode,          &
                                                  deltaI                = iDelta2DNode,        &
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
                                                  messageObject     = struct2DMessage1,    &
                                                  enableChecking    = enableChecking,      &
                                                  errorInfoObject   = errorInfoObject)

            IF (CheckForLocalError(errorInfoObject)) THEN
             passedTest = .FALSE.
             IF (verbose)  THEN
              WRITE(charStringObject%charString,'(a80)') &
               'FAIL:  Error detected during messageObject data completion (struct2DMessage1) call.'
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
                   messageObject     = struct2DMessage1,  &
                   enableChecking    = enableChecking,    &
                   errorInfoObject   = errorInfoObject)

             IF (CheckForLocalError(errorInfoObject)) THEN
              passedTest = .FALSE.
              IF (verbose)  THEN
               WRITE(charStringObject%charString,'(a80)') &
               'FAIL:  Error detected during struct2DMessage1 send call: '
               CALL AddErrorInformation(object          = errorInfoObject, &
                                        errorInfoString = charStringObject)
              END IF
              GO TO 100
             END IF

             CALL WaitForMessageCompletion(                                    &
                                  nodeMPIDataObject    = mpiData,              &
                                  messageObject        = struct2DMessage1,     &
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
                   messageObject     = struct2DMessage1,  &
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
                                  messageObject        = struct2DMessage1,     &
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
                  object            = struct2DMessage1, &
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

             CALL CheckDataValue(dataValue         = real8Array2DDataRecv,     &
                                 expectedDataValue = real8Array2DDataExpected, &
                                 correctValue      = isEqual,                  &
                                 errorLocation     = errorLoc2D,               &
                                 errorInfoObject   = errorInfoObject)

             IF (isEqual) THEN
!             WRITE(0,'(1x,a21,8(1x,i3),a7)') 'PASS:  Array2DTest (', &
!                           iDirM,iStrideM,iStartM,iEndM, &
!                           jDirM,jStrideM,jStartM,jEndM, &
!                                              ') call.'
             ELSE
              WRITE(0,'(1x,a21,8(1x,i3),a7)') 'FAIL:  Array2DTest (', &
                            iDirM,iStrideM,iStartM,iEndM, &
                            jDirM,jStrideM,jStartM,jEndM, &
                                               ') call.'
              WRITE(0,*) 'Error location = ',errorLoc2D(1:2)
              WRITE(0,*) ' Found:    ',real8Array2DDataRecv(errorLoc2D(1),errorLoc2D(2))
              WRITE(0,*) ' Expected: ',real8Array2DDataExpected(errorLoc2D(1),errorLoc2D(2))
              DO jj=1,matrixSize
               DO ii=1,matrixSize
                WRITE(0,*) ii,jj, &
                           real8Array2DData1(ii,jj), &
                           real8Array2DData2(ii,jj), &
                           real8Array2DDataRecv(ii,jj), &
                           real8Array2DDataExpected(ii,jj)
               END DO
               WRITE(0,*)
              END DO
              passedTest = .FALSE.
              charStringObject%charString = 'Failed 2D array test.'
              CALL SetError(object          = errorInfoObject, &
                            errorInfoString = charStringObject)
             END IF
            END IF

            IF (CheckForGlobalError(errorInfoObject)) THEN
             GO TO 100
            END IF

           END DO
          END DO
         END DO
        END DO

       END DO
      END DO
     END DO
    END DO

! done!

    IF (masterNode) THEN
     WRITE(0,'(1x,a21)') 'PASS:  Array2DTest  '
    END IF


   END DO
  END DO

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*) '---PASSED 2D Array Testing (Double Precision)--'
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*)
  END IF

  RETURN

100 CONTINUE
  RETURN

END SUBROUTINE Test2DArrayPassing

END MODULE Test2DArrayPassingModule
