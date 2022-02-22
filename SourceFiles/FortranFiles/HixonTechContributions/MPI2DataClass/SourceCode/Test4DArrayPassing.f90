MODULE Test4DArrayPassingModule

  USE MPI2DataClassNoErrClass           ! MPI2DataClassNoErrClass.f90
  USE ErrorInformationClass             ! ErrorInformationClass.f90
  USE TMatModule
  USE Check4DData

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Test4DArrayPassing

CONTAINS

SUBROUTINE Test4DArrayPassing(mpiData,              &
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

  TYPE(structMessageObjectType) :: struct4DMessage1 !, &
!                                  struct4DMessage2

  INTEGER, PARAMETER :: numDim = 4
  INTEGER, PARAMETER :: matrixSize = 15

  REAL(KIND=8), DIMENSION(matrixSize,matrixSize,matrixSize,matrixSize) ::  &
                                    real8Array4DData1,    &
                                    real8Array4DData2,    &
                                    real8Array4DDataRecv, &
                                    real8Array4DDataExpected

  INTEGER :: messageTag,    &
             messageTypeID, &
             sendingNodeID, &
             recvingNodeID

  INTEGER :: i,       &
             j,       &
             k,       &
             l,       &
             n

  INTEGER :: iDirM, &
             jDirM, &
             kDirM, &
             lDirM, &
             iStrideM, &
             jStrideM, &
             kStrideM, &
             lStrideM, &
             iStartM, &
             jStartM, &
             kStartM, &
             lStartM, &
             iEndM, &
             jEndM, &
             kEndM, &
             lEndM

  INTEGER :: nn,       &
             ii,      &
             jj,      &
             kk,      &
             ll

  INTEGER, DIMENSION(numDim) :: nDir,        &
                                nStride,     &
                                nStart,      &
                                nEnd

  INTEGER, DIMENSION(numDim) :: errorLoc4D

  INTEGER, DIMENSION(numDim) :: iStart4DMaster, &
                                iEnd4DMaster,   &
                                iDelta4DMaster, &
                                iOrder4DMaster, &
                                iOrder4DSign, &
                                iStart4DNode, &
                                iEnd4DNode,   &
                                iDelta4DNode, &
                                iOrder4DNode, &
                                iMatchMaster, &
                                iMatchNode,   &
                                iNode,        &
                                dIMaster,     &
                                dINode !, &
!                           dI

  INTEGER, DIMENSION(numDim,2) :: start4D,  &
                                  end4D,    &
                                  stride4D, &
                                  direction4D

  INTEGER, DIMENSION(numDim,numDim) :: tMat

  CONTINUE ! execution starts here

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*) '--STARTING 4D Array Testing (Double Precision)--'
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*)
  END IF

  messageTag = 1
  messageTypeID = 1

  isEqual = .TRUE.

! generate the arrays and send out to the nodes

  start4D(1,1) = 1
  start4D(1,2) = 3

  start4D(2,1) = 1
  start4D(2,2) = 5

  start4D(3,1) = 1
  start4D(3,2) = 7

  start4D(4,1) = 1
  start4D(4,2) = 9

  end4D(1,1)   = matrixSize
  end4D(1,2)   = matrixSize-8

  end4D(2,1)   = matrixSize
  end4D(2,2)   = matrixSize-6

  end4D(3,1)   = matrixSize
  end4D(3,2)   = matrixSize-4

  end4D(4,1)   = matrixSize
  end4D(4,2)   = matrixSize-2

  stride4D(1,1) = 1
  stride4D(1,2) = 2

  stride4D(2,1) = 1
  stride4D(2,2) = 2

  stride4D(3,1) = 1
  stride4D(3,2) = 2

  stride4D(4,1) = 1
  stride4D(4,2) = 2

  direction4D(1,1) =  1
  direction4D(1,2) = -1

  direction4D(2,1) =  1
  direction4D(2,2) = -1

  direction4D(3,1) =  1
  direction4D(3,2) = -1

  direction4D(4,1) =  1
  direction4D(4,2) = -1

  DO l=1,matrixSize
   DO k=1,matrixSize
    DO j=1,matrixSize
     DO i=1,matrixSize
      CALL RANDOM_NUMBER(real8Array4DData1(i,j,k,l))
      CALL RANDOM_NUMBER(real8Array4DData2(i,j,k,l))
     END DO
    END DO
   END DO
  END DO

! both are doing the same thing

  sendingNodeID = 0
  recvingNodeID = 1

  CALL CreateObject(object            = struct4DMessage1,   &
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
     'FAIL:  Error detected during messageObject creation (struct4DMessage1) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  DO n=1,numDim
   iStart4DMaster(n) = 1
   iEnd4DMaster(n)   = matrixSize
   iDelta4DMaster(n) = 1
   iOrder4DMaster(n) = n
  END DO

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct4DMessage1,    &
                                       messageData           = real8Array4DData1,   &
                                       iOrder                = iOrder4DMaster,      &
                                       iStart                = iStart4DMaster,      &
                                       iEnd                  = iEnd4DMaster,        &
                                       deltaI                = iDelta4DMaster,      &
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

  DO n=1,numDim
   iStart4DMaster(n) = 1
   iEnd4DMaster(n)   = matrixSize
   iDelta4DMaster(n) = 1
   iOrder4DMaster(n) = n
  END DO

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct4DMessage1,    &
                                       messageData           = real8Array4DData2,   &
                                       iOrder                = iOrder4DMaster,      &
                                       iStart                = iStart4DMaster,      &
                                       iEnd                  = iEnd4DMaster,        &
                                       deltaI                = iDelta4DMaster,      &
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
                                        messageObject     = struct4DMessage1,    &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (struct4DMessage1) call.'
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
         messageObject     = struct4DMessage1,  &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during struct4DMessage1 send call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                    &
                        nodeMPIDataObject    = mpiData,              &
                        messageObject        = struct4DMessage1,     &
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
         messageObject     = struct4DMessage1,  &
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
                        messageObject        = struct4DMessage1,     &
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
        object            = struct4DMessage1, &
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

! At this point, both nodes have the same data in the real8Array4DData1
!   and real8Array4DData2 arrays.

! go through the possible transformations

  DO l= 1,numDim ! the sign will be set properly by the GetTMatrix routine

   IF (l == 0) CYCLE

   iOrder4DSign(4) = l

   DO k= -numDim,numDim ! the sign will be set properly by the GetTMatrix routine

    IF (k == 0) CYCLE
    IF (ABS(k) == ABS(l)) CYCLE

    iOrder4DSign(3) = k

    DO j= -numDim,numDim ! the sign will be set properly by the GetTMatrix routine

     IF (j == 0) CYCLE
     IF (ABS(j) == ABS(k)) CYCLE
     IF (ABS(j) == ABS(l)) CYCLE

     iOrder4DSign(2) = j

      DO i=-numDim,numDim

       IF (i == 0) CYCLE
       IF (ABS(i) == ABS(j)) CYCLE
       IF (ABS(i) == ABS(k)) CYCLE
       IF (ABS(i) == ABS(l)) CYCLE

       iOrder4DSign(1) = i

       DO n=1,numDim
        iOrder4DNode(n) = ABS(iOrder4DSign(n))
       END DO

       CALL GetTMatrix4D(tMatrix         = tMat,           &
                         iOrderSign      = iOrder4DSign,   &
                         enableChecking  = enableChecking, &
                         errorInfoObject = errorInfoObject)
 
       IF (masterNode) THEN
        WRITE(0,*) '-----------------------------------'
        WRITE(0,*) 'From the Node: iOrderSign = ',iOrder4DSign(1:numDim)
        WRITE(0,*) 
        WRITE(0,*) 'To the Master, the transformation matrix is: '
        WRITE(0,*) 
        DO n=1,numDim
         WRITE(0,*) '|',tMat(n,1:numDim),'|'
        END DO
        WRITE(0,*) 
       END IF

!      CYCLE ! debug

! for this transformation matrix, do all of the possibilities...

! i direction

       DO iDirM = 1,2   
        DO iStrideM = 1,2   
         DO iStartM = 1,2   
          DO iEndM = 1,2   
   
           IF (direction4D(1,iDirM) > 0) THEN
            iStart4DMaster(1) = start4D(1,iStartM)
            iEnd4DMaster(1)   = end4D(1,iEndM)
            iDelta4DMaster(1) = stride4D(1,iStrideM)
           ELSE
            iStart4DMaster(1) = end4D(1,iEndM)
            iEnd4DMaster(1)   = start4D(1,iStartM)
            iDelta4DMaster(1) = -stride4D(1,iStrideM)
           END IF

           nDir(1)    = iDirM
           nStride(1) = iStrideM
           nStart(1)  = iStartM
           nEnd(1)    = iEndM

!  j direction

           DO jDirM = 1,2   
            DO jStrideM = 1,2   
             DO jStartM = 1,2   
              DO jEndM = 1,2   
  
!            IF (masterNode) WRITE(0,'(1x,a9,8(i3))') 'testing ', &
!                            iDirM,iStrideM,iStartM,iEndM, &
!                            jDirM,jStrideM,jStartM,jEndM

               IF (direction4D(2,jDirM) > 0) THEN
                iStart4DMaster(2) = start4D(2,jStartM)
                iEnd4DMaster(2)   = end4D(2,jEndM)
                iDelta4DMaster(2) = stride4D(2,jStrideM)
               ELSE
                iStart4DMaster(2) = end4D(2,jEndM)
                iEnd4DMaster(2)   = start4D(2,jStartM)
                iDelta4DMaster(2) = -stride4D(2,jStrideM)
               END IF

               nDir(2)    = jDirM
               nStride(2) = jStrideM
               nStart(2)  = jStartM
               nEnd(2)    = jEndM

!  k direction

               DO kDirM = 1,2   
                DO kStrideM = 1,2   
                 DO kStartM = 1,2   
                  DO kEndM = 1,2   

!            IF (masterNode) WRITE(0,'(1x,a9,8(i3))') 'testing ', &
!                            iDirM,iStrideM,iStartM,iEndM, &
!                            kDirM,kStrideM,kStartM,kEndM

                   IF (direction4D(3,kDirM) > 0) THEN
                    iStart4DMaster(3) = start4D(3,kStartM)
                    iEnd4DMaster(3)   = end4D(3,kEndM)
                    iDelta4DMaster(3) = stride4D(3,kStrideM)
                   ELSE
                    iStart4DMaster(3) = end4D(3,kEndM)
                    iEnd4DMaster(3)   = start4D(3,kStartM)
                    iDelta4DMaster(3) = -stride4D(3,kStrideM)
                   END IF

                   nDir(3)    = kDirM
                   nStride(3) = kStrideM
                   nStart(3)  = kStartM
                   nEnd(3)    = kEndM

!  l direction

                   DO lDirM = 1,2   
                    DO lStrideM = 1,2   
                     DO lStartM = 1,2   
                      DO lEndM = 1,2   

!                IF (masterNode) WRITE(0,'(1x,a9,16(i3))') 'testing ', &
!                                iDirM,iStrideM,iStartM,iEndM, &
!                                jDirM,jStrideM,jStartM,jEndM, &
!                                kDirM,kStrideM,kStartM,kEndM, &
!                                lDirM,lStrideM,lStartM,lEndM

                       IF (direction4D(4,lDirM) > 0) THEN
                        iStart4DMaster(4) = start4D(4,lStartM)
                        iEnd4DMaster(4)   = end4D(4,lEndM)
                        iDelta4DMaster(4) = stride4D(4,lStrideM)
                       ELSE
                        iStart4DMaster(4) = end4D(4,lEndM)
                        iEnd4DMaster(4)   = start4D(4,lStartM)
                        iDelta4DMaster(4) = -stride4D(4,lStrideM)
                       END IF

                       nDir(4)    = lDirM
                       nStride(4) = lStrideM
                       nStart(4)  = lStartM
                       nEnd(4)    = lEndM

!      on the master, the start, end, stride, and directions are set...

!      ... and on the master, we're loading in i,j,k,l order...

!      this is where it gets needlessly interesting.

                       DO n=1,numDim
                        iMatchMaster(n) = 1
                       END DO

                       DO jj=1,numDim
                        DO ii=1,numDim
                         IF (tMat(jj,ii) /= 0) THEN
                          IF (tMat(jj,ii) > 0) THEN
                           iMatchNode(ii) = 1
                          ELSE
                           iMatchNode(ii) = matrixSize ! all directions are matrixSize long
                          END IF
                         END IF
                        END DO
                       END DO
                   
!                IF (masterNode) THEN
!                 WRITE(0,*) 'iMatchMaster: ',iMatchMaster(1:2)
!                 WRITE(0,*) 'iMatchNode:   ',iMatchNode(1:2)
!                END IF

                       DO n=1,numDim
                        iOrder4DMaster(n) = n
                        nn = iOrder4DNode(n) ! which coordinate direction on node is data
                                             !  coming from?
               
                        IF (tMat(n,nn) > 0) THEN ! same direction
                         iDelta4DNode(nn) = iDelta4DMaster(n)
                        ELSE ! turn it around
                         iDelta4DNode(nn) = -iDelta4DMaster(n)
                        END IF

                       END DO

!        get the start locations

                       DO n=1,numDim
                        dIMaster(n) = iStart4DMaster(n)-1
                       END DO
        
                       DO n=1,numDim
                        dINode(n) = 0
                        DO nn=1,numDim
                         dINode(n) = dINode(n) + dIMaster(nn)*tMat(nn,n)
                        END DO
                        iStart4DNode(n) = iMatchNode(n) + dINode(n)
                       END DO
                    
!        get the end locations

                       DO n=1,numDim
                        dIMaster(n) = iEnd4DMaster(n)-1
                       END DO

                       DO n=1,numDim
                        dINode(n) = 0
                        DO nn=1,numDim
                         dINode(n) = dINode(n) + dIMaster(nn)*tMat(nn,n)
                        END DO
                        iEnd4DNode(n) = iMatchNode(n) + dINode(n)
                       END DO
                  
!      now generate the messages

!                 IF (masterNode) THEN
!                  WRITE(0,*) 'on the Master, the data arrives in this order: ', &
!                    iOrder4DMaster(1:2)
!                  WRITE(0,'(6(1x,i4))') (iStart4DMaster(n), &
!                                         iEnd4DMaster(n),   &
!                                         iDelta4DMaster(n),n=1,2) 
!                  WRITE(0,*) 'on the Node, the data is pulled in this order: ', &
!                    iOrder4DNode(1:2)
!                  WRITE(0,'(6(1x,i4))') (iStart4DNode(n), &
!                                         iEnd4DNode(n),   &
!                                         iDelta4DNode(n),n=1,2) 
      
!                  WRITE(0,*)
!                 END IF

!                 GO TO 100

!           both are doing the same thing

                       sendingNodeID = 1
                       recvingNodeID = 0

                       CALL CreateObject(object            = struct4DMessage1,   &
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
                          'FAIL:  Error detected during messageObject creation (struct4DMessage1) call. '
                         CALL AddErrorInformation(object          = errorInfoObject, &
                                                  errorInfoString = charStringObject)
                        END IF
                        GO TO 100
                       END IF

                       IF (masterNode) THEN
                        DO ll=1,matrixSize
                         DO kk=1,matrixSize
                          DO jj=1,matrixSize
                           DO ii=1,matrixSize
                            real8Array4DDataRecv(ii,jj,kk,ll)     = real8Array4DData1(ii,jj,kk,ll) 
                            real8Array4DDataExpected(ii,jj,kk,ll) = real8Array4DData1(ii,jj,kk,ll) 
                           END DO
                          END DO
                         END DO
                        END DO

!           generate the _expected_ array

                       DO ll=iStart4DMaster(4),iEnd4DMaster(4),iDelta4DMaster(4)
                        DO kk=iStart4DMaster(3),iEnd4DMaster(3),iDelta4DMaster(3)
                         DO jj=iStart4DMaster(2),iEnd4DMaster(2),iDelta4DMaster(2)
                          DO ii=iStart4DMaster(1),iEnd4DMaster(1),iDelta4DMaster(1)

                           dIMaster(1) = ii-1
                           dIMaster(2) = jj-1
                           dIMaster(3) = kk-1
                           dIMaster(4) = ll-1

                           dINode(1) = (dIMaster(1)*tMat(1,1) &
                                       +dIMaster(2)*tMat(2,1) & 
                                       +dIMaster(3)*tMat(3,1) & 
                                       +dIMaster(4)*tMat(4,1))  
                           dINode(2) = (dIMaster(1)*tMat(1,2) &
                                       +dIMaster(2)*tMat(2,2) & 
                                       +dIMaster(3)*tMat(3,2) & 
                                       +dIMaster(4)*tMat(4,2))  
                           dINode(3) = (dIMaster(1)*tMat(1,3) &
                                       +dIMaster(2)*tMat(2,3) & 
                                       +dIMaster(3)*tMat(3,3) & 
                                       +dIMaster(4)*tMat(4,3))  
                           dINode(4) = (dIMaster(1)*tMat(1,4) &
                                       +dIMaster(2)*tMat(2,4) & 
                                       +dIMaster(3)*tMat(3,4) & 
                                       +dIMaster(4)*tMat(4,4))  

                           iNode(1) = iMatchNode(1) + dINode(1)
                           iNode(2) = iMatchNode(2) + dINode(2)
                           iNode(3) = iMatchNode(3) + dINode(3)
                           iNode(4) = iMatchNode(4) + dINode(4)

                           real8Array4DDataExpected(ii,jj,kk,ll) = real8Array4DData2(iNode(1), &
                                                                                     iNode(2), &
                                                                                     iNode(3), &
                                                                                     iNode(4)) 
                         
                          END DO
                         END DO
                        END DO
                       END DO

!           generate the message

                       CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,              &
                                                            structMessageObject   = struct4DMessage1,     &
                                                            messageData           = real8Array4DDataRecv, &
                                                            iOrder                = iOrder4DMaster,       &
                                                            iStart                = iStart4DMaster,       &
                                                            iEnd                  = iEnd4DMaster,         &
                                                            deltaI                = iDelta4DMaster,       &
                                                            enableChecking        = enableChecking,       &
                                                            errorInfoObject       = errorInfoObject)

                      ELSE ! node
                       CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                                            structMessageObject   = struct4DMessage1,    &
                                                            messageData           = real8Array4DData2,   &
                                                            iOrder                = iOrder4DNode,        &
                                                            iStart                = iStart4DNode,        &
                                                            iEnd                  = iEnd4DNode,          &
                                                            deltaI                = iDelta4DNode,        &
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

!           message data is complete

                      CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                                            messageObject     = struct4DMessage1,    &
                                                            enableChecking    = enableChecking,      &
                                                            errorInfoObject   = errorInfoObject)

                      IF (CheckForLocalError(errorInfoObject)) THEN
                       passedTest = .FALSE.
                       IF (verbose)  THEN
                        WRITE(charStringObject%charString,'(a80)') &
                         'FAIL:  Error detected during messageObject data completion (struct4DMessage1) call.'
                        CALL AddErrorInformation(object          = errorInfoObject, &
                                                 errorInfoString = charStringObject)
                       END IF
                       GO TO 100
                      END IF

                      IF (CheckForGlobalError(errorInfoObject)) THEN
                       GO TO 100
                      END IF

!           send the message

                      IF (masterNode) THEN

                       CALL RecvMessageFromNode(                    &
                             nodeMPIDataObject = mpiData,           &
                             messageObject     = struct4DMessage1,  &
                             enableChecking    = enableChecking,    &
                             errorInfoObject   = errorInfoObject)

                       IF (CheckForLocalError(errorInfoObject)) THEN
                        passedTest = .FALSE.
                        IF (verbose)  THEN
                         WRITE(charStringObject%charString,'(a80)') &
                         'FAIL:  Error detected during struct4DMessage1 send call: '
                         CALL AddErrorInformation(object          = errorInfoObject, &
                                                  errorInfoString = charStringObject)
                        END IF
                        GO TO 100
                       END IF

                       CALL WaitForMessageCompletion(                                    &
                                            nodeMPIDataObject    = mpiData,              &
                                            messageObject        = struct4DMessage1,     &
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
                             messageObject     = struct4DMessage1,  &
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
                                            messageObject        = struct4DMessage1,     &
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

!      destroy the object

                      CALL DestroyObject(                         &
                            object            = struct4DMessage1, &
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

                       CALL CheckDataValue(dataValue         = real8Array4DDataRecv,     &
                                           expectedDataValue = real8Array4DDataExpected, &
                                           correctValue      = isEqual,                  &
                                           errorLocation     = errorLoc4D,               &
                                           errorInfoObject   = errorInfoObject)

                       IF (isEqual) THEN
!                       WRITE(0,'(1x,a21,8(1x,i3),a7)') 'PASS:  Array4DTest (', &
!                                     iDirM,iStrideM,iStartM,iEndM, &
!                                     jDirM,jStrideM,jStartM,jEndM, &
!                                                        ') call.'
                       ELSE
                        WRITE(0,'(1x,a21,12(1x,i3),a7)') 'FAIL:  Array4DTest (', &
                                      iDirM,iStrideM,iStartM,iEndM, &
                                      jDirM,jStrideM,jStartM,jEndM, &
                                      kDirM,kStrideM,kStartM,kEndM, &
                                      lDirM,lStrideM,lStartM,lEndM, &
                                                         ') call.'
                        WRITE(0,*) 'Error location = ',errorLoc4D(1:numDim)
                        WRITE(0,*) ' Found:    ',real8Array4DDataRecv(errorLoc4D(1), &
                                                                      errorLoc4D(2), &
                                                                      errorLoc4D(3), &
                                                                      errorLoc4D(4))
                        WRITE(0,*) ' Expected: ',real8Array4DDataExpected(errorLoc4D(1), &
                                                                          errorLoc4D(2), &
                                                                          errorLoc4D(3), &
                                                                          errorLoc4D(4))
                        DO ll=1,matrixSize
                         DO kk=1,matrixSize
                          DO jj=1,matrixSize
                           DO ii=1,matrixSize
                            WRITE(0,*) ii,jj,kk,ll, &
                                       real8Array4DData1(ii,jj,kk,ll), &
                                       real8Array4DData2(ii,jj,kk,ll), &
                                       real8Array4DDataRecv(ii,jj,kk,ll), &
                                       real8Array4DDataExpected(ii,jj,kk,ll)
                           END DO
                           WRITE(0,*)
                          END DO
                          WRITE(0,*)
                         END DO
                         WRITE(0,*)
                        END DO
                        passedTest = .FALSE.
                        charStringObject%charString = 'Failed 4D array test.'
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
       WRITE(0,'(1x,a21)') 'PASS:  Array4DTest  '
      END IF

     END DO
    END DO
   END DO
  END DO

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*) '---PASSED 4D Array Testing (Double Precision)--'
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*)
  END IF

  RETURN

100 CONTINUE
  RETURN

END SUBROUTINE Test4DArrayPassing

END MODULE Test4DArrayPassingModule
