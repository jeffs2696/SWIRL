MODULE Test3DArrayPassingModule

  USE MPI2DataClassNoErrClass           ! MPI2DataClassNoErrClass.f90
  USE ErrorInformationClass             ! ErrorInformationClass.f90
  USE TMatModule
  USE Check3DData

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Test3DArrayPassing

CONTAINS

SUBROUTINE Test3DArrayPassing(mpiData,              &
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

  TYPE(structMessageObjectType) :: struct3DMessage1 !, &
!                                  struct3DMessage2

  INTEGER, PARAMETER :: numDim = 3
  INTEGER, PARAMETER :: matrixSize = 15

  REAL(KIND=8), DIMENSION(matrixSize,matrixSize,matrixSize) ::  &
                                    real8Array3DData1,    &
                                    real8Array3DData2,    &
                                    real8Array3DDataRecv, &
                                    real8Array3DDataExpected

  INTEGER :: messageTag,    &
             messageTypeID, &
             sendingNodeID, &
             recvingNodeID

  INTEGER :: i,       &
             j,       &
             k,       &
             n

  INTEGER :: iDirM, &
             jDirM, &
             kDirM, &
             iStrideM, &
             jStrideM, &
             kStrideM, &
             iStartM, &
             jStartM, &
             kStartM, &
             iEndM, &
             jEndM, &
             kEndM

  INTEGER :: nn,       &
             ii,      &
             jj,      &
             kk

  INTEGER, DIMENSION(numDim) :: nDir,        &
                                nStride,     &
                                nStart,      &
                                nEnd

  INTEGER, DIMENSION(numDim) :: errorLoc3D

  INTEGER, DIMENSION(numDim) :: iStart3DMaster, &
                                iEnd3DMaster,   &
                                iDelta3DMaster, &
                                iOrder3DMaster, &
                                iOrder3DSign, &
                                iStart3DNode, &
                                iEnd3DNode,   &
                                iDelta3DNode, &
                                iOrder3DNode, &
                                iMatchMaster, &
                                iMatchNode,   &
                                iNode,        &
                                dIMaster,     &
                                dINode !, &
!                           dI

  INTEGER, DIMENSION(numDim,2) :: start3D,  &
                                  end3D,    &
                                  stride3D, &
                                  direction3D

  INTEGER, DIMENSION(numDim,numDim) :: tMat

  CONTINUE ! execution starts here

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*) '--STARTING 3D Array Testing (Double Precision)--'
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*)
  END IF

  messageTag = 1
  messageTypeID = 1

  isEqual = .TRUE.

! generate the arrays and send out to the nodes

  start3D(1,1) = 1
  start3D(1,2) = 3

  start3D(2,1) = 1
  start3D(2,2) = 5

  start3D(3,1) = 1
  start3D(3,2) = 7

  end3D(1,1)   = matrixSize
  end3D(1,2)   = matrixSize-4

  end3D(2,1)   = matrixSize
  end3D(2,2)   = matrixSize-2

  end3D(3,1)   = matrixSize
  end3D(3,2)   = matrixSize-6

  stride3D(1,1) = 1
  stride3D(1,2) = 2

  stride3D(2,1) = 1
  stride3D(2,2) = 2

  stride3D(3,1) = 1
  stride3D(3,2) = 2

  direction3D(1,1) =  1
  direction3D(1,2) = -1

  direction3D(2,1) =  1
  direction3D(2,2) = -1

  direction3D(3,1) =  1
  direction3D(3,2) = -1

  DO k=1,matrixSize
   DO j=1,matrixSize
    DO i=1,matrixSize
     CALL RANDOM_NUMBER(real8Array3DData1(i,j,k))
     CALL RANDOM_NUMBER(real8Array3DData2(i,j,k))
    END DO
   END DO
  END DO

! both are doing the same thing

  sendingNodeID = 0
  recvingNodeID = 1

  CALL CreateObject(object            = struct3DMessage1,   &
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
     'FAIL:  Error detected during messageObject creation (struct3DMessage1) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  DO n=1,numDim
   iStart3DMaster(n) = 1
   iEnd3DMaster(n)   = matrixSize
   iDelta3DMaster(n) = 1
   iOrder3DMaster(n) = n
  END DO

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct3DMessage1,    &
                                       messageData           = real8Array3DData1,   &
                                       iOrder                = iOrder3DMaster,      &
                                       iStart                = iStart3DMaster,      &
                                       iEnd                  = iEnd3DMaster,        &
                                       deltaI                = iDelta3DMaster,      &
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
   iStart3DMaster(n) = 1
   iEnd3DMaster(n)   = matrixSize
   iDelta3DMaster(n) = 1
   iOrder3DMaster(n) = n
  END DO

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct3DMessage1,    &
                                       messageData           = real8Array3DData2,   &
                                       iOrder                = iOrder3DMaster,      &
                                       iStart                = iStart3DMaster,      &
                                       iEnd                  = iEnd3DMaster,        &
                                       deltaI                = iDelta3DMaster,      &
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
                                        messageObject     = struct3DMessage1,    &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (struct3DMessage1) call.'
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
         messageObject     = struct3DMessage1,  &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during struct3DMessage1 send call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                    &
                        nodeMPIDataObject    = mpiData,              &
                        messageObject        = struct3DMessage1,     &
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
         messageObject     = struct3DMessage1,  &
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
                        messageObject        = struct3DMessage1,     &
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
        object            = struct3DMessage1, &
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

! At this point, both nodes have the same data in the real8Array3DData1
!   and real8Array3DData2 arrays.

! go through the possible transformations

  DO k= 1,numDim ! the sign will be set properly by the GetTMatrix routine

   IF (k == 0) CYCLE

   iOrder3DSign(3) = k

   DO j= -numDim,numDim ! the sign will be set properly by the GetTMatrix routine

    IF (j == 0) CYCLE
    IF (ABS(j) == ABS(k)) CYCLE

    iOrder3DSign(2) = j

     DO i=-numDim,numDim

      IF (i == 0) CYCLE
      IF (ABS(i) == ABS(j)) CYCLE
      IF (ABS(i) == ABS(k)) CYCLE

      iOrder3DSign(1) = i

      DO n=1,numDim
       iOrder3DNode(n) = ABS(iOrder3DSign(n))
      END DO

      CALL GetTMatrix3D(tMatrix         = tMat,           &
                        iOrderSign      = iOrder3DSign,   &
                        enableChecking  = enableChecking, &
                        errorInfoObject = errorInfoObject)
 
      IF (masterNode) THEN
       WRITE(0,*) '-----------------------------------'
       WRITE(0,*) 'From the Node: iOrderSign = ',iOrder3DSign(1:numDim)
       WRITE(0,*) 
       WRITE(0,*) 'To the Master, the transformation matrix is: '
       WRITE(0,*) 
       DO n=1,numDim
        WRITE(0,*) '|',tMat(n,1:numDim),'|'
       END DO
       WRITE(0,*) 
      END IF

! for this transformation matrix, do all of the possibilities...

! i direction

      DO iDirM = 1,2   
       DO iStrideM = 1,2   
        DO iStartM = 1,2   
         DO iEndM = 1,2   
  
          IF (direction3D(1,iDirM) > 0) THEN
           iStart3DMaster(1) = start3D(1,iStartM)
           iEnd3DMaster(1)   = end3D(1,iEndM)
           iDelta3DMaster(1) = stride3D(1,iStrideM)
          ELSE
           iStart3DMaster(1) = end3D(1,iEndM)
           iEnd3DMaster(1)   = start3D(1,iStartM)
           iDelta3DMaster(1) = -stride3D(1,iStrideM)
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

              IF (direction3D(2,jDirM) > 0) THEN
               iStart3DMaster(2) = start3D(2,jStartM)
               iEnd3DMaster(2)   = end3D(2,jEndM)
               iDelta3DMaster(2) = stride3D(2,jStrideM)
              ELSE
               iStart3DMaster(2) = end3D(2,jEndM)
               iEnd3DMaster(2)   = start3D(2,jStartM)
               iDelta3DMaster(2) = -stride3D(2,jStrideM)
              END IF

              nDir(2)    = jDirM
              nStride(2) = jStrideM
              nStart(2)  = jStartM
              nEnd(2)    = jEndM

! k direction

              DO kDirM = 1,2   
               DO kStrideM = 1,2   
                DO kStartM = 1,2   
                 DO kEndM = 1,2   

!           IF (masterNode) WRITE(0,'(1x,a9,12(i3))') 'testing ', &
!                           iDirM,iStrideM,iStartM,iEndM, &
!                           jDirM,jStrideM,jStartM,jEndM, &
!                           kDirM,kStrideM,kStartM,kEndM

                  IF (direction3D(3,kDirM) > 0) THEN
                   iStart3DMaster(3) = start3D(3,kStartM)
                   iEnd3DMaster(3)   = end3D(3,kEndM)
                   iDelta3DMaster(3) = stride3D(3,kStrideM)
                  ELSE
                   iStart3DMaster(3) = end3D(3,kEndM)
                   iEnd3DMaster(3)   = start3D(3,kStartM)
                   iDelta3DMaster(3) = -stride3D(3,kStrideM)
                  END IF

                  nDir(3)    = kDirM
                  nStride(3) = kStrideM
                  nStart(3)  = kStartM
                  nEnd(3)    = kEndM

! on the master, the start, end, stride, and directions are set...

! ... and on the master, we're loading in i,j,k order...

! this is where it gets needlessly interesting.

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
              
!           IF (masterNode) THEN
!            WRITE(0,*) 'iMatchMaster: ',iMatchMaster(1:2)
!            WRITE(0,*) 'iMatchNode:   ',iMatchNode(1:2)
!           END IF

                  DO n=1,numDim
                   iOrder3DMaster(n) = n
                   nn = iOrder3DNode(n) ! which coordinate direction on node is data
                                        !  coming from?
          
                   IF (tMat(n,nn) > 0) THEN ! same direction
                    iDelta3DNode(nn) = iDelta3DMaster(n)
                   ELSE ! turn it around
                    iDelta3DNode(nn) = -iDelta3DMaster(n)
                   END IF

                  END DO

!   get the start locations

                  DO n=1,numDim
                   dIMaster(n) = iStart3DMaster(n)-1
                  END DO
   
                  DO n=1,numDim
                   dINode(n) = 0
                   DO nn=1,numDim
                    dINode(n) = dINode(n) + dIMaster(nn)*tMat(nn,n)
                   END DO
                   iStart3DNode(n) = iMatchNode(n) + dINode(n)
                  END DO
               
!   get the end locations

                  DO n=1,numDim
                   dIMaster(n) = iEnd3DMaster(n)-1
                  END DO

                  DO n=1,numDim
                   dINode(n) = 0
                   DO nn=1,numDim
                    dINode(n) = dINode(n) + dIMaster(nn)*tMat(nn,n)
                   END DO
                   iEnd3DNode(n) = iMatchNode(n) + dINode(n)
                  END DO
             
! now generate the messages

!            IF (masterNode) THEN
!             WRITE(0,*) 'on the Master, the data arrives in this order: ', &
!               iOrder3DMaster(1:2)
!             WRITE(0,'(6(1x,i4))') (iStart3DMaster(n), &
!                                    iEnd3DMaster(n),   &
!                                    iDelta3DMaster(n),n=1,2) 
!             WRITE(0,*) 'on the Node, the data is pulled in this order: ', &
!               iOrder3DNode(1:2)
!             WRITE(0,'(6(1x,i4))') (iStart3DNode(n), &
!                                    iEnd3DNode(n),   &
!                                    iDelta3DNode(n),n=1,2) 
 
!             WRITE(0,*)
!            END IF

!            GO TO 100

!      both are doing the same thing

                  sendingNodeID = 1
                  recvingNodeID = 0

                  CALL CreateObject(object            = struct3DMessage1,   &
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
                     'FAIL:  Error detected during messageObject creation (struct3DMessage1) call. '
                    CALL AddErrorInformation(object          = errorInfoObject, &
                                             errorInfoString = charStringObject)
                   END IF
                   GO TO 100
                  END IF

                  IF (masterNode) THEN
                   DO kk=1,matrixSize
                    DO jj=1,matrixSize
                     DO ii=1,matrixSize
                      real8Array3DDataRecv(ii,jj,kk)     = real8Array3DData1(ii,jj,kk) 
                      real8Array3DDataExpected(ii,jj,kk) = real8Array3DData1(ii,jj,kk) 
                     END DO
                    END DO
                   END DO

!      generate the _expected_ array

                  DO kk=iStart3DMaster(3),iEnd3DMaster(3),iDelta3DMaster(3)
                   DO jj=iStart3DMaster(2),iEnd3DMaster(2),iDelta3DMaster(2)
                    DO ii=iStart3DMaster(1),iEnd3DMaster(1),iDelta3DMaster(1)

                     dIMaster(1) = ii-1
                     dIMaster(2) = jj-1
                     dIMaster(3) = kk-1

                     dINode(1) = (dIMaster(1)*tMat(1,1) &
                                 +dIMaster(2)*tMat(2,1) & 
                                 +dIMaster(3)*tMat(3,1))  
                     dINode(2) = (dIMaster(1)*tMat(1,2) &
                                 +dIMaster(2)*tMat(2,2) & 
                                 +dIMaster(3)*tMat(3,2))  
                     dINode(3) = (dIMaster(1)*tMat(1,3) &
                                 +dIMaster(2)*tMat(2,3) & 
                                 +dIMaster(3)*tMat(3,3))  

                     iNode(1) = iMatchNode(1) + dINode(1)
                     iNode(2) = iMatchNode(2) + dINode(2)
                     iNode(3) = iMatchNode(3) + dINode(3)

                     real8Array3DDataExpected(ii,jj,kk) = real8Array3DData2(iNode(1),iNode(2),iNode(3)) 
                    
                    END DO
                   END DO
                  END DO

!      generate the message

                  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,              &
                                                       structMessageObject   = struct3DMessage1,     &
                                                       messageData           = real8Array3DDataRecv, &
                                                       iOrder                = iOrder3DMaster,       &
                                                       iStart                = iStart3DMaster,       &
                                                       iEnd                  = iEnd3DMaster,         &
                                                       deltaI                = iDelta3DMaster,       &
                                                       enableChecking        = enableChecking,       &
                                                       errorInfoObject       = errorInfoObject)

                 ELSE ! node
                  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                                       structMessageObject   = struct3DMessage1,    &
                                                       messageData           = real8Array3DData2,   &
                                                       iOrder                = iOrder3DNode,        &
                                                       iStart                = iStart3DNode,        &
                                                       iEnd                  = iEnd3DNode,          &
                                                       deltaI                = iDelta3DNode,        &
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

!      message data is complete

                 CALL MPIStructureDefinitionIsComplete(nodeMPIDataObject = mpiData,             &
                                                       messageObject     = struct3DMessage1,    &
                                                       enableChecking    = enableChecking,      &
                                                       errorInfoObject   = errorInfoObject)

                 IF (CheckForLocalError(errorInfoObject)) THEN
                  passedTest = .FALSE.
                  IF (verbose)  THEN
                   WRITE(charStringObject%charString,'(a80)') &
                    'FAIL:  Error detected during messageObject data completion (struct3DMessage1) call.'
                   CALL AddErrorInformation(object          = errorInfoObject, &
                                            errorInfoString = charStringObject)
                  END IF
                  GO TO 100
                 END IF

                 IF (CheckForGlobalError(errorInfoObject)) THEN
                  GO TO 100
                 END IF

!      send the message

                 IF (masterNode) THEN

                  CALL RecvMessageFromNode(                    &
                        nodeMPIDataObject = mpiData,           &
                        messageObject     = struct3DMessage1,  &
                        enableChecking    = enableChecking,    &
                        errorInfoObject   = errorInfoObject)

                  IF (CheckForLocalError(errorInfoObject)) THEN
                   passedTest = .FALSE.
                   IF (verbose)  THEN
                    WRITE(charStringObject%charString,'(a80)') &
                    'FAIL:  Error detected during struct3DMessage1 send call: '
                    CALL AddErrorInformation(object          = errorInfoObject, &
                                             errorInfoString = charStringObject)
                   END IF
                   GO TO 100
                  END IF

                  CALL WaitForMessageCompletion(                                    &
                                       nodeMPIDataObject    = mpiData,              &
                                       messageObject        = struct3DMessage1,     &
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
                        messageObject     = struct3DMessage1,  &
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
                                       messageObject        = struct3DMessage1,     &
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
                       object            = struct3DMessage1, &
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

                  CALL CheckDataValue(dataValue         = real8Array3DDataRecv,     &
                                      expectedDataValue = real8Array3DDataExpected, &
                                      correctValue      = isEqual,                  &
                                      errorLocation     = errorLoc3D,               &
                                      errorInfoObject   = errorInfoObject)

                  IF (isEqual) THEN
!                  WRITE(0,'(1x,a21,8(1x,i3),a7)') 'PASS:  Array3DTest (', &
!                                iDirM,iStrideM,iStartM,iEndM, &
!                                jDirM,jStrideM,jStartM,jEndM, &
!                                                   ') call.'
                  ELSE
                   WRITE(0,'(1x,a21,12(1x,i3),a7)') 'FAIL:  Array3DTest (', &
                                 iDirM,iStrideM,iStartM,iEndM, &
                                 jDirM,jStrideM,jStartM,jEndM, &
                                 kDirM,kStrideM,kStartM,kEndM, &
                                                    ') call.'
                   WRITE(0,*) 'Error location = ',errorLoc3D(1:2)
                   WRITE(0,*) ' Found:    ',real8Array3DDataRecv(errorLoc3D(1),errorLoc3D(2),errorLoc3D(3))
                   WRITE(0,*) ' Expected: ',real8Array3DDataExpected(errorLoc3D(1),errorLoc3D(2),errorLoc3D(3))
                   DO kk=1,matrixSize
                    DO jj=1,matrixSize
                     DO ii=1,matrixSize
                      WRITE(0,*) ii,jj,kk, &
                                 real8Array3DData1(ii,jj,kk), &
                                 real8Array3DData2(ii,jj,kk), &
                                 real8Array3DDataRecv(ii,jj,kk), &
                                 real8Array3DDataExpected(ii,jj,kk)
                     END DO
                     WRITE(0,*)
                    END DO
                    WRITE(0,*)
                   END DO
                   passedTest = .FALSE.
                   charStringObject%charString = 'Failed 3D array test.'
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

! done!

     IF (masterNode) THEN
      WRITE(0,'(1x,a21)') 'PASS:  Array3DTest  '
     END IF

    END DO
   END DO
  END DO

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*) '---PASSED 3D Array Testing (Double Precision)--'
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*)
  END IF

  RETURN

100 CONTINUE
  RETURN

END SUBROUTINE Test3DArrayPassing

END MODULE Test3DArrayPassingModule
