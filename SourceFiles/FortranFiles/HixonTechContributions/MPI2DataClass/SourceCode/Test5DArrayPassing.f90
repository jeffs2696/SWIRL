MODULE Test5DArrayPassingModule

  USE MPI2DataClassNoErrClass           ! MPI2DataClassNoErrClass.f90
  USE ErrorInformationClass             ! ErrorInformationClass.f90
  USE TMatModule
  USE Check5DData

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Test5DArrayPassing

CONTAINS

SUBROUTINE Test5DArrayPassing(mpiData,              &
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

  TYPE(structMessageObjectType) :: struct5DMessage1 !, &
!                                  struct5DMessage2

  INTEGER, PARAMETER :: numDim = 5
  INTEGER, PARAMETER :: matrixSize = 15

  REAL(KIND=8), DIMENSION(matrixSize, &
                          matrixSize, &
                          matrixSize, &
                          matrixSize, &
                          matrixSize) ::  &
                                    real8Array5DData1,    &
                                    real8Array5DData2,    &
                                    real8Array5DDataRecv, &
                                    real8Array5DDataExpected

  INTEGER :: messageTag,    &
             messageTypeID, &
             sendingNodeID, &
             recvingNodeID

  INTEGER :: i,        &
             j,        &
             k,        &
             l,        &
             m,        &
             n,        &
             nTest,    &
             nTestTot, &
             nTMat,    &
             nTMatTot

  INTEGER :: iDirM, &
             jDirM, &
             kDirM, &
             lDirM, &
             mDirM, &
             iStrideM, &
             jStrideM, &
             kStrideM, &
             lStrideM, &
             mStrideM, &
             iStartM, &
             jStartM, &
             kStartM, &
             lStartM, &
             mStartM, &
             iEndM, &
             jEndM, &
             kEndM, &
             lEndM, &
             mEndM

  INTEGER :: nn,       &
             ii,      &
             jj,      &
             kk,      &
             ll,      &
             mm

  INTEGER, DIMENSION(numDim) :: nDir,        &
                                nStride,     &
                                nStart,      &
                                nEnd

  INTEGER, DIMENSION(numDim) :: errorLoc5D

  INTEGER, DIMENSION(numDim) :: iStart5DMaster, &
                                iEnd5DMaster,   &
                                iDelta5DMaster, &
                                iOrder5DMaster, &
                                iOrder5DSign, &
                                iStart5DNode, &
                                iEnd5DNode,   &
                                iDelta5DNode, &
                                iOrder5DNode, &
                                iMatchMaster, &
                                iMatchNode,   &
                                iNode,        &
                                dIMaster,     &
                                dINode !, &
!                           dI

  INTEGER, DIMENSION(numDim,2) :: start5D,  &
                                  end5D,    &
                                  stride5D, &
                                  direction5D

  INTEGER, DIMENSION(numDim,numDim) :: tMat

  CONTINUE ! execution starts here

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*) '--STARTING 5D Array Testing (Double Precision)--'
   WRITE(0,*) '------------------------------------------------'
   WRITE(0,*)
  END IF

  messageTag = 1
  messageTypeID = 1

  isEqual = .TRUE.

! generate the arrays and send out to the nodes

  start5D(1,1) = 1
  start5D(1,2) = 3

  start5D(2,1) = 1
  start5D(2,2) = 5

  start5D(3,1) = 1
  start5D(3,2) = 7

  start5D(4,1) = 1
  start5D(4,2) = 9

  start5D(5,1) = 1
  start5D(5,2) = 11

  end5D(1,1)   = matrixSize
  end5D(1,2)   = matrixSize-8

  end5D(2,1)   = matrixSize
  end5D(2,2)   = matrixSize-6

  end5D(3,1)   = matrixSize
  end5D(3,2)   = matrixSize-4

  end5D(4,1)   = matrixSize
  end5D(4,2)   = matrixSize-2

  end5D(5,1)   = matrixSize
  end5D(5,2)   = matrixSize-8

  stride5D(1,1) = 1
  stride5D(1,2) = 2

  stride5D(2,1) = 1
  stride5D(2,2) = 2

  stride5D(3,1) = 1
  stride5D(3,2) = 2

  stride5D(4,1) = 1
  stride5D(4,2) = 2

  stride5D(5,1) = 1
  stride5D(5,2) = 2

  direction5D(1,1) =  1
  direction5D(1,2) = -1

  direction5D(2,1) =  1
  direction5D(2,2) = -1

  direction5D(3,1) =  1
  direction5D(3,2) = -1

  direction5D(4,1) =  1
  direction5D(4,2) = -1

  direction5D(5,1) =  1
  direction5D(5,2) = -1

  DO m=1,matrixSize
   DO l=1,matrixSize
    DO k=1,matrixSize
     DO j=1,matrixSize
      DO i=1,matrixSize
       CALL RANDOM_NUMBER(real8Array5DData1(i,j,k,l,m))
       CALL RANDOM_NUMBER(real8Array5DData2(i,j,k,l,m))
      END DO
     END DO
    END DO
   END DO
  END DO

! both are doing the same thing

  sendingNodeID = 0
  recvingNodeID = 1

  CALL CreateObject(object            = struct5DMessage1,   &
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
     'FAIL:  Error detected during messageObject creation (struct5DMessage1) call. '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF
   GO TO 100
  END IF

  DO n=1,numDim
   iStart5DMaster(n) = 1
   iEnd5DMaster(n)   = matrixSize
   iDelta5DMaster(n) = 1
   iOrder5DMaster(n) = n
  END DO

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct5DMessage1,    &
                                       messageData           = real8Array5DData1,   &
                                       iOrder                = iOrder5DMaster,      &
                                       iStart                = iStart5DMaster,      &
                                       iEnd                  = iEnd5DMaster,        &
                                       deltaI                = iDelta5DMaster,      &
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
   iStart5DMaster(n) = 1
   iEnd5DMaster(n)   = matrixSize
   iDelta5DMaster(n) = 1
   iOrder5DMaster(n) = n
  END DO

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                       structMessageObject   = struct5DMessage1,    &
                                       messageData           = real8Array5DData2,   &
                                       iOrder                = iOrder5DMaster,      &
                                       iStart                = iStart5DMaster,      &
                                       iEnd                  = iEnd5DMaster,        &
                                       deltaI                = iDelta5DMaster,      &
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
                                        messageObject     = struct5DMessage1,    &
                                        enableChecking    = enableChecking,      &
                                        errorInfoObject   = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   IF (verbose)  THEN
    WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during messageObject data completion (struct5DMessage1) call.'
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
         messageObject     = struct5DMessage1,  &
         enableChecking    = enableChecking,    &
         errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    passedTest = .FALSE.
    IF (verbose)  THEN
     WRITE(charStringObject%charString,'(a80)') &
     'FAIL:  Error detected during struct5DMessage1 send call: '
     CALL AddErrorInformation(object          = errorInfoObject, &
                              errorInfoString = charStringObject)
    END IF
    GO TO 100
   END IF

   CALL WaitForMessageCompletion(                                    &
                        nodeMPIDataObject    = mpiData,              &
                        messageObject        = struct5DMessage1,     &
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
         messageObject     = struct5DMessage1,  &
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
                        messageObject        = struct5DMessage1,     &
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
        object            = struct5DMessage1, &
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

! At this point, both nodes have the same data in the real8Array5DData1
!   and real8Array5DData2 arrays.

! go through the possible transformations

  nTMatTot = 5*4*3*2
  nTMat = 1

  DO m= 1,numDim ! the sign will be set properly by the GetTMatrix routine

   IF (m == 0) CYCLE

   iOrder5DSign(5) = m

   DO l= -numDim,numDim ! the sign will be set properly by the GetTMatrix routine

    IF (l == 0) CYCLE
    IF (ABS(l) == ABS(m)) CYCLE

    iOrder5DSign(4) = l

   DO k= -numDim,numDim ! the sign will be set properly by the GetTMatrix routine

    IF (k == 0) CYCLE
    IF (ABS(k) == ABS(l)) CYCLE
    IF (ABS(k) == ABS(m)) CYCLE

    iOrder5DSign(3) = k

    DO j= -numDim,numDim ! the sign will be set properly by the GetTMatrix routine

     IF (j == 0) CYCLE
     IF (ABS(j) == ABS(k)) CYCLE
     IF (ABS(j) == ABS(l)) CYCLE
     IF (ABS(j) == ABS(m)) CYCLE

     iOrder5DSign(2) = j

      DO i=-numDim,numDim

       IF (i == 0) CYCLE
       IF (ABS(i) == ABS(j)) CYCLE
       IF (ABS(i) == ABS(k)) CYCLE
       IF (ABS(i) == ABS(l)) CYCLE
       IF (ABS(i) == ABS(m)) CYCLE

       iOrder5DSign(1) = i

       DO n=1,numDim
        iOrder5DNode(n) = ABS(iOrder5DSign(n))
       END DO

       CALL GetTMatrix5D(tMatrix         = tMat,           &
                         iOrderSign      = iOrder5DSign,   &
                         enableChecking  = enableChecking, &
                         errorInfoObject = errorInfoObject)
 
       IF (masterNode) THEN
        WRITE(0,*) '-----------------------------------'
        WRITE(0,*) 'From the Node: iOrderSign = ',iOrder5DSign(1:numDim)
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

       nTestTot = 2**20

! i direction

       nTest = 1
       DO iDirM = 1,2   
        DO iStrideM = 1,2   
         DO iStartM = 1,2   
          DO iEndM = 1,2   
   
           IF (direction5D(1,iDirM) > 0) THEN
            iStart5DMaster(1) = start5D(1,iStartM)
            iEnd5DMaster(1)   = end5D(1,iEndM)
            iDelta5DMaster(1) = stride5D(1,iStrideM)
           ELSE
            iStart5DMaster(1) = end5D(1,iEndM)
            iEnd5DMaster(1)   = start5D(1,iStartM)
            iDelta5DMaster(1) = -stride5D(1,iStrideM)
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

               IF (direction5D(2,jDirM) > 0) THEN
                iStart5DMaster(2) = start5D(2,jStartM)
                iEnd5DMaster(2)   = end5D(2,jEndM)
                iDelta5DMaster(2) = stride5D(2,jStrideM)
               ELSE
                iStart5DMaster(2) = end5D(2,jEndM)
                iEnd5DMaster(2)   = start5D(2,jStartM)
                iDelta5DMaster(2) = -stride5D(2,jStrideM)
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

                   IF (direction5D(3,kDirM) > 0) THEN
                    iStart5DMaster(3) = start5D(3,kStartM)
                    iEnd5DMaster(3)   = end5D(3,kEndM)
                    iDelta5DMaster(3) = stride5D(3,kStrideM)
                   ELSE
                    iStart5DMaster(3) = end5D(3,kEndM)
                    iEnd5DMaster(3)   = start5D(3,kStartM)
                    iDelta5DMaster(3) = -stride5D(3,kStrideM)
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

                       IF (direction5D(4,lDirM) > 0) THEN
                        iStart5DMaster(4) = start5D(4,lStartM)
                        iEnd5DMaster(4)   = end5D(4,lEndM)
                        iDelta5DMaster(4) = stride5D(4,lStrideM)
                       ELSE
                        iStart5DMaster(4) = end5D(4,lEndM)
                        iEnd5DMaster(4)   = start5D(4,lStartM)
                        iDelta5DMaster(4) = -stride5D(4,lStrideM)
                       END IF

                       nDir(4)    = lDirM
                       nStride(4) = lStrideM
                       nStart(4)  = lStartM
                       nEnd(4)    = lEndM

!  m direction

                       DO mDirM = 1,2   
                        DO mStrideM = 1,2   
                         DO mStartM = 1,2   
                          DO mEndM = 1,2   

!                IF (masterNode) WRITE(0,'(1x,a9,16(i3))') 'testing ', &
!                                iDirM,iStrideM,iStartM,iEndM, &
!                                jDirM,jStrideM,jStartM,jEndM, &
!                                kDirM,kStrideM,kStartM,kEndM, &
!                                lDirM,lStrideM,lStartM,lEndM

                           IF (direction5D(5,lDirM) > 0) THEN
                            iStart5DMaster(5) = start5D(5,lStartM)
                            iEnd5DMaster(5)   = end5D(5,lEndM)
                            iDelta5DMaster(5) = stride5D(5,lStrideM)
                           ELSE
                            iStart5DMaster(5) = end5D(5,lEndM)
                            iEnd5DMaster(5)   = start5D(5,lStartM)
                            iDelta5DMaster(5) = -stride5D(5,lStrideM)
                           END IF

                           nDir(5)    = lDirM
                           nStride(5) = lStrideM
                           nStart(5)  = lStartM
                           nEnd(5)    = lEndM

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
                            iOrder5DMaster(n) = n
                            nn = iOrder5DNode(n) ! which coordinate direction on node is data
                                                 !  coming from?
               
                            IF (tMat(n,nn) > 0) THEN ! same direction
                             iDelta5DNode(nn) = iDelta5DMaster(n)
                            ELSE ! turn it around
                             iDelta5DNode(nn) = -iDelta5DMaster(n)
                            END IF
    
                           END DO

!        get the start locations

                           DO n=1,numDim
                            dIMaster(n) = iStart5DMaster(n)-1
                           END DO
        
                           DO n=1,numDim
                            dINode(n) = 0
                            DO nn=1,numDim
                             dINode(n) = dINode(n) + dIMaster(nn)*tMat(nn,n)
                            END DO
                            iStart5DNode(n) = iMatchNode(n) + dINode(n)
                           END DO
                    
!        get the end locations

                           DO n=1,numDim
                            dIMaster(n) = iEnd5DMaster(n)-1
                           END DO

                           DO n=1,numDim
                            dINode(n) = 0
                            DO nn=1,numDim
                             dINode(n) = dINode(n) + dIMaster(nn)*tMat(nn,n)
                            END DO
                            iEnd5DNode(n) = iMatchNode(n) + dINode(n)
                           END DO
                  
!      now generate the messages

!                 IF (masterNode) THEN
!                  WRITE(0,*) 'on the Master, the data arrives in this order: ', &
!                    iOrder5DMaster(1:5)
!                  DO n=1,5
!                   nn = iOrder5DMaster(n)
!                   WRITE(0,'(6(1x,i4))') n,nn,iStart5DMaster(nn), &
!                                              iEnd5DMaster(nn),   &
!                                              iDelta5DMaster(nn)
!                  END DO
!                  WRITE(0,*)
!                  WRITE(0,*) 'on the Node, the data is pulled in this order: ', &
!                    iOrder5DNode(1:5)
!                  DO n=1,5
!                   nn = iOrder5DNode(n)
!                   WRITE(0,'(6(1x,i4))') n,nn,iStart5DNode(nn), &
!                                              iEnd5DNode(nn),   &
!                                              iDelta5DNode(nn)
!                  END DO
!     
!                  WRITE(0,*)
!                 END IF

!                 GO TO 100

! looks correct for the first message, at least.

!           both are doing the same thing

                           sendingNodeID = 1
                           recvingNodeID = 0

                           CALL CreateObject(object            = struct5DMessage1,   &
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
                              'FAIL:  Error detected during messageObject creation (struct5DMessage1) call. '
                             CALL AddErrorInformation(object          = errorInfoObject, &
                                                      errorInfoString = charStringObject)
                            END IF
                            GO TO 100
                           END IF

                           IF (masterNode) THEN
                            DO mm=1,matrixSize
                             DO ll=1,matrixSize
                              DO kk=1,matrixSize
                               DO jj=1,matrixSize
                                DO ii=1,matrixSize
                                 real8Array5DDataRecv(ii,jj,kk,ll,mm)     = real8Array5DData1(ii,jj,kk,ll,mm) 
                                 real8Array5DDataExpected(ii,jj,kk,ll,mm) = real8Array5DData1(ii,jj,kk,ll,mm) 
                                END DO
                               END DO
                              END DO
                             END DO
                            END DO

!           generate the _expected_ array

                            DO mm=iStart5DMaster(5),iEnd5DMaster(5),iDelta5DMaster(5)
                             DO ll=iStart5DMaster(4),iEnd5DMaster(4),iDelta5DMaster(4)
                              DO kk=iStart5DMaster(3),iEnd5DMaster(3),iDelta5DMaster(3)
                               DO jj=iStart5DMaster(2),iEnd5DMaster(2),iDelta5DMaster(2)
                                DO ii=iStart5DMaster(1),iEnd5DMaster(1),iDelta5DMaster(1)

                                 dIMaster(1) = ii-1
                                 dIMaster(2) = jj-1
                                 dIMaster(3) = kk-1
                                 dIMaster(4) = ll-1
                                 dIMaster(5) = mm-1
 
                                 dINode(1) = (dIMaster(1)*tMat(1,1) &
                                             +dIMaster(2)*tMat(2,1) & 
                                             +dIMaster(3)*tMat(3,1) & 
                                             +dIMaster(4)*tMat(4,1) & 
                                             +dIMaster(5)*tMat(5,1))  
                                 dINode(2) = (dIMaster(1)*tMat(1,2) &
                                             +dIMaster(2)*tMat(2,2) & 
                                             +dIMaster(3)*tMat(3,2) & 
                                             +dIMaster(4)*tMat(4,2) & 
                                             +dIMaster(5)*tMat(5,2))  
                                 dINode(3) = (dIMaster(1)*tMat(1,3) &
                                             +dIMaster(2)*tMat(2,3) & 
                                             +dIMaster(3)*tMat(3,3) & 
                                             +dIMaster(4)*tMat(4,3) & 
                                             +dIMaster(5)*tMat(5,3))  
                                 dINode(4) = (dIMaster(1)*tMat(1,4) &
                                             +dIMaster(2)*tMat(2,4) & 
                                             +dIMaster(3)*tMat(3,4) & 
                                             +dIMaster(4)*tMat(4,4) & 
                                             +dIMaster(5)*tMat(5,4))  
                                 dINode(5) = (dIMaster(1)*tMat(1,5) &
                                             +dIMaster(2)*tMat(2,5) & 
                                             +dIMaster(3)*tMat(3,5) & 
                                             +dIMaster(4)*tMat(4,5) & 
                                             +dIMaster(5)*tMat(5,5))  

                                iNode(1) = iMatchNode(1) + dINode(1)
                                iNode(2) = iMatchNode(2) + dINode(2)
                                iNode(3) = iMatchNode(3) + dINode(3)
                                iNode(4) = iMatchNode(4) + dINode(4)
                                iNode(5) = iMatchNode(5) + dINode(5)

                                real8Array5DDataExpected(ii,jj,kk,ll,mm) = real8Array5DData2(iNode(1), &
                                                                                             iNode(2), &
                                                                                             iNode(3), &
                                                                                             iNode(4), &
                                                                                             iNode(5)) 
                         
                                END DO
                               END DO
                              END DO
                             END DO
                            END DO

!           generate the message

                            CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,              &
                                                                 structMessageObject   = struct5DMessage1,     &
                                                                 messageData           = real8Array5DDataRecv, &
                                                                 iOrder                = iOrder5DMaster,       &
                                                                 iStart                = iStart5DMaster,       &
                                                                 iEnd                  = iEnd5DMaster,         &
                                                                 deltaI                = iDelta5DMaster,       &
                                                                 enableChecking        = enableChecking,       &
                                                                 errorInfoObject       = errorInfoObject)

                           ELSE ! node
                            CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = mpiData,             &
                                                                 structMessageObject   = struct5DMessage1,    &
                                                                 messageData           = real8Array5DData2,   &
                                                                 iOrder                = iOrder5DNode,        &
                                                                 iStart                = iStart5DNode,        &
                                                                 iEnd                  = iEnd5DNode,          &
                                                                 deltaI                = iDelta5DNode,        &
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
                                                                 messageObject     = struct5DMessage1,    &
                                                                 enableChecking    = enableChecking,      &
                                                                 errorInfoObject   = errorInfoObject)
     
                           IF (CheckForLocalError(errorInfoObject)) THEN
                            passedTest = .FALSE.
                            IF (verbose)  THEN
                             WRITE(charStringObject%charString,'(a80)') &
                              'FAIL:  Error detected during messageObject data completion (struct5DMessage1) call.'
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
                                  messageObject     = struct5DMessage1,  &
                                  enableChecking    = enableChecking,    &
                                  errorInfoObject   = errorInfoObject)

                            IF (CheckForLocalError(errorInfoObject)) THEN
                             passedTest = .FALSE.
                             IF (verbose)  THEN
                              WRITE(charStringObject%charString,'(a80)') &
                              'FAIL:  Error detected during struct5DMessage1 send call: '
                              CALL AddErrorInformation(object          = errorInfoObject, &
                                                       errorInfoString = charStringObject)
                             END IF
                             GO TO 100
                            END IF

                            CALL WaitForMessageCompletion(                                    &
                                                 nodeMPIDataObject    = mpiData,              &
                                                 messageObject        = struct5DMessage1,     &
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
                                  messageObject     = struct5DMessage1,  &
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
                                                 messageObject        = struct5DMessage1,     &
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

                           END IF ! send/recv?

                           IF (CheckForGlobalError(errorInfoObject)) THEN
                            GO TO 100
                           END IF

!      destroy the object

                           CALL DestroyObject(                         &
                                 object            = struct5DMessage1, &
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

                            CALL CheckDataValue(dataValue         = real8Array5DDataRecv,     &
                                                expectedDataValue = real8Array5DDataExpected, &
                                                correctValue      = isEqual,                  &
                                                errorLocation     = errorLoc5D,               &
                                                errorInfoObject   = errorInfoObject)

                            IF (isEqual) THEN
                             WRITE(0,'(1x,a,i9,a,i9,a,i3,a,i3)') 'PASS:  Test ',nTest,' of ',nTestTot, &
                               ' for transformation matrix ',nTMat,' of ',nTMatTot
                             CONTINUE
                            ELSE
                             WRITE(0,'(1x,a21,20(1x,i3),a7)') 'FAIL:  Array5DTest (', &
                                           iDirM,iStrideM,iStartM,iEndM, &
                                           jDirM,jStrideM,jStartM,jEndM, &
                                           kDirM,kStrideM,kStartM,kEndM, &
                                           lDirM,lStrideM,lStartM,lEndM, &
                                           mDirM,mStrideM,mStartM,mEndM, &
                                                              ') call.'
                             WRITE(0,*) 'Error location = ',errorLoc5D(1:numDim)
                             WRITE(0,*) ' Found:    ',real8Array5DDataRecv(errorLoc5D(1), &
                                                                           errorLoc5D(2), &
                                                                           errorLoc5D(3), &
                                                                           errorLoc5D(4), &
                                                                           errorLoc5D(5))
                             WRITE(0,*) ' Expected: ',real8Array5DDataExpected(errorLoc5D(1), &
                                                                               errorLoc5D(2), &
                                                                               errorLoc5D(3), &
                                                                               errorLoc5D(4), &
                                                                               errorLoc5D(5))
                             DO mm=1,matrixSize
                              DO ll=1,matrixSize
                               DO kk=1,matrixSize
                                DO jj=1,matrixSize
                                 DO ii=1,matrixSize
                                  WRITE(0,*) ii,jj,kk,ll,mm, &
                                             real8Array5DData1(ii,jj,kk,ll,mm), &
                                             real8Array5DData2(ii,jj,kk,ll,mm), &
                                             real8Array5DDataRecv(ii,jj,kk,ll,mm), &
                                             real8Array5DDataExpected(ii,jj,kk,ll,mm)
                                 END DO ! ii
                                 WRITE(0,*)
                                END DO  ! jj
                                WRITE(0,*)
                               END DO   ! kk
                               WRITE(0,*)
                              END DO    ! ll
                              WRITE(0,*)
                             END DO     ! mm
                             passedTest = .FALSE.
                             charStringObject%charString = 'Failed 5D array test.'
                             CALL SetError(object          = errorInfoObject, &
                                           errorInfoString = charStringObject)
                            END IF ! isEqual?
                           END IF ! master node?

                           IF (CheckForGlobalError(errorInfoObject)) THEN
                            GO TO 100
                           END IF

                           nTest = nTest + 1
                          END DO ! m
                         END DO
                        END DO
                       END DO

                      END DO ! l
                     END DO
                    END DO
                   END DO

                  END DO ! k
                 END DO
                END DO
               END DO

              END DO ! j
             END DO
            END DO
           END DO

          END DO ! i
         END DO
        END DO
       END DO

! done!

       IF (masterNode) THEN
        WRITE(0,'(1x,a21,i3)') 'PASS:  Array5DTest for transformation matrix:',nTMat
       END IF

       nTMat = nTMat + 1
      END DO ! transformation matrices
     END DO 
    END DO
   END DO
  END DO

  IF (masterNode) THEN
   WRITE(0,*)
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*) '---PASSED 5D Array Testing (Double Precision)--'
   WRITE(0,*) '-----------------------------------------------'
   WRITE(0,*)
  END IF

  RETURN

100 CONTINUE
  RETURN

END SUBROUTINE Test5DArrayPassing

END MODULE Test5DArrayPassingModule
