
  i1 = iOrder(1)
  i2 = iOrder(2)
  i3 = iOrder(3)
  i4 = iOrder(4)

  IF ((i1 == 1) .AND. (deltaI(1) == 1)) THEN ! special case where data length is not 1
   i = iStart(1)
   numEntries = iEnd(1)+1-iStart(1)

   IF (i2 == 2) THEN  ! ij

    IF (i3 == 3) THEN ! ijkl

     DO l=iStart(4),iEnd(4),deltaI(4)
      DO k=iStart(3),iEnd(3),deltaI(3)
       DO j=iStart(2),iEnd(2),deltaI(2)

        numEntries = iEnd(1)+1-iStart(1)

        CALL AddLLEntry(object          = object,               &
                        messageData     = messageData(i,j,k,l), &
                        numberOfEntries = numEntries,           &
                        errorInfoObject = errorInfoObject)

        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO

    ELSE ! ijlk

     DO k=iStart(3),iEnd(3),deltaI(3)
      DO l=iStart(4),iEnd(4),deltaI(4)
       DO j=iStart(2),iEnd(2),deltaI(2)

        numEntries = iEnd(1)+1-iStart(1)

        CALL AddLLEntry(object          = object,               &
                        messageData     = messageData(i,j,k,l), &
                        numberOfEntries = numEntries,           &
                        errorInfoObject = errorInfoObject)

        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO

    END IF

   ELSE IF (i2 == 3) THEN ! ik

    IF (i3 == 4) THEN ! iklj

     DO j=iStart(2),iEnd(2),deltaI(2)
      DO l=iStart(4),iEnd(4),deltaI(4)
       DO k=iStart(3),iEnd(3),deltaI(3)

        numEntries = iEnd(1)+1-iStart(1)

        CALL AddLLEntry(object          = object,               &
                        messageData     = messageData(i,j,k,l), &
                        numberOfEntries = numEntries,           &
                        errorInfoObject = errorInfoObject)

        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO

    ELSE ! ikjl

     DO l=iStart(4),iEnd(4),deltaI(4)
      DO j=iStart(2),iEnd(2),deltaI(2)
       DO k=iStart(3),iEnd(3),deltaI(3)

        numEntries = iEnd(1)+1-iStart(1)

        CALL AddLLEntry(object          = object,               &
                        messageData     = messageData(i,j,k,l), &
                        numberOfEntries = numEntries,           &
                        errorInfoObject = errorInfoObject)

        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO
    END IF
   ELSE ! il
    IF (i3 == 2) THEN ! iljk

     DO k=iStart(3),iEnd(3),deltaI(3)
      DO j=iStart(2),iEnd(2),deltaI(2)
       DO l=iStart(4),iEnd(4),deltaI(4)

        numEntries = iEnd(1)+1-iStart(1)

        CALL AddLLEntry(object          = object,               &
                        messageData     = messageData(i,j,k,l), &
                        numberOfEntries = numEntries,           &
                        errorInfoObject = errorInfoObject)

        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO

    ELSE ! ilkj

     DO j=iStart(2),iEnd(2),deltaI(2)
      DO k=iStart(3),iEnd(3),deltaI(3)
       DO l=iStart(4),iEnd(4),deltaI(4)

        numEntries = iEnd(1)+1-iStart(1)

        CALL AddLLEntry(object          = object,               &
                        messageData     = messageData(i,j,k,l), &
                        numberOfEntries = numEntries,           &
                        errorInfoObject = errorInfoObject)

        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO
    END IF
   END IF
  ELSE ! one data point per entry -- all I have to do is to get it in the right order

   numEntries = 1

! ijkl
! ijlk
! iklj
! ikjl
! iljk
! ilkj

   IF (iOrder(1) == 1) THEN
    IF (iOrder(2) == 2) THEN
     IF (iOrder(3) == 3) THEN ! ijkl

      DO l=iStart(4),iEnd(4),deltaI(4)
       DO k=iStart(3),iEnd(3),deltaI(3)
        DO j=iStart(2),iEnd(2),deltaI(2)
         DO i=iStart(1),iEnd(1),deltaI(1)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! ijlk
      DO k=iStart(3),iEnd(3),deltaI(3)
       DO l=iStart(4),iEnd(4),deltaI(4)
        DO j=iStart(2),iEnd(2),deltaI(2)
         DO i=iStart(1),iEnd(1),deltaI(1)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    ELSE IF (iOrder(2) == 3) THEN ! ik
     IF (iOrder(3) == 2) THEN ! ikjl

      DO l=iStart(4),iEnd(4),deltaI(4)
       DO j=iStart(2),iEnd(2),deltaI(2)
        DO k=iStart(3),iEnd(3),deltaI(3)
         DO i=iStart(1),iEnd(1),deltaI(1)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! iklj
      DO j=iStart(2),iEnd(2),deltaI(2)
       DO l=iStart(4),iEnd(4),deltaI(4)
        DO k=iStart(3),iEnd(3),deltaI(3)
         DO i=iStart(1),iEnd(1),deltaI(1)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    ELSE IF (iOrder(2) == 4) THEN ! il
     IF (iOrder(3) == 2) THEN ! iljk

      DO k=iStart(3),iEnd(3),deltaI(3)
       DO j=iStart(2),iEnd(2),deltaI(2)
        DO l=iStart(4),iEnd(4),deltaI(4)
         DO i=iStart(1),iEnd(1),deltaI(1)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! ilkj
      DO j=iStart(2),iEnd(2),deltaI(2)
       DO k=iStart(3),iEnd(3),deltaI(3)
        DO l=iStart(4),iEnd(4),deltaI(4)
         DO i=iStart(1),iEnd(1),deltaI(1)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF

    END IF
   ELSE IF (iOrder(1) == 2) THEN ! j
    IF (iOrder(2) == 1) THEN     ! ji
     IF (iOrder(3) == 3) THEN    ! jikl

      DO l=iStart(4),iEnd(4),deltaI(4)
       DO k=iStart(3),iEnd(3),deltaI(3)
        DO i=iStart(1),iEnd(1),deltaI(1)
         DO j=iStart(2),iEnd(2),deltaI(2)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! jilk
      DO k=iStart(3),iEnd(3),deltaI(3)
       DO l=iStart(4),iEnd(4),deltaI(4)
        DO i=iStart(1),iEnd(1),deltaI(1)
         DO j=iStart(2),iEnd(2),deltaI(2)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    ELSE IF (iOrder(2) == 3) THEN ! jk
     IF (iOrder(3) == 1) THEN ! jkil

      DO l=iStart(4),iEnd(4),deltaI(4)
       DO i=iStart(1),iEnd(1),deltaI(1)
        DO k=iStart(3),iEnd(3),deltaI(3)
         DO j=iStart(2),iEnd(2),deltaI(2)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! jkli
      DO i=iStart(1),iEnd(1),deltaI(1)
       DO l=iStart(4),iEnd(4),deltaI(4)
        DO k=iStart(3),iEnd(3),deltaI(3)
         DO j=iStart(2),iEnd(2),deltaI(2)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    ELSE IF (iOrder(2) == 4) THEN ! jl
     IF (iOrder(3) == 1) THEN ! jlik

      DO k=iStart(3),iEnd(3),deltaI(3)
       DO i=iStart(1),iEnd(1),deltaI(1)
        DO l=iStart(4),iEnd(4),deltaI(4)
         DO j=iStart(2),iEnd(2),deltaI(2)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! jlki
      DO i=iStart(1),iEnd(1),deltaI(1)
       DO k=iStart(3),iEnd(3),deltaI(3)
        DO l=iStart(4),iEnd(4),deltaI(4)
         DO j=iStart(2),iEnd(2),deltaI(2)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF

    END IF

   ELSE IF (iOrder(1) == 3) THEN ! k
    IF (iOrder(2) == 1) THEN     ! ki
     IF (iOrder(3) == 2) THEN    ! kijl

      DO l=iStart(4),iEnd(4),deltaI(4)
       DO j=iStart(2),iEnd(2),deltaI(2)
        DO i=iStart(1),iEnd(1),deltaI(1)
         DO k=iStart(3),iEnd(3),deltaI(3)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! kilj
      DO j=iStart(2),iEnd(2),deltaI(2)
       DO l=iStart(4),iEnd(4),deltaI(4)
        DO i=iStart(1),iEnd(1),deltaI(1)
         DO k=iStart(3),iEnd(3),deltaI(3)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    ELSE IF (iOrder(2) == 2) THEN ! kj
     IF (iOrder(3) == 1) THEN ! kjil

      DO l=iStart(4),iEnd(4),deltaI(4)
       DO i=iStart(1),iEnd(1),deltaI(1)
        DO j=iStart(2),iEnd(2),deltaI(2)
         DO k=iStart(3),iEnd(3),deltaI(3)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! kjli
      DO i=iStart(1),iEnd(1),deltaI(1)
       DO l=iStart(4),iEnd(4),deltaI(4)
        DO j=iStart(2),iEnd(2),deltaI(2)
         DO k=iStart(3),iEnd(3),deltaI(3)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    ELSE IF (iOrder(2) == 4) THEN ! kl
     IF (iOrder(3) == 1) THEN ! klij

      DO j=iStart(2),iEnd(2),deltaI(2)
       DO i=iStart(1),iEnd(1),deltaI(1)
        DO l=iStart(4),iEnd(4),deltaI(4)
         DO k=iStart(3),iEnd(3),deltaI(3)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! klji
      DO i=iStart(1),iEnd(1),deltaI(1)
       DO j=iStart(2),iEnd(2),deltaI(2)
        DO l=iStart(4),iEnd(4),deltaI(4)
         DO k=iStart(3),iEnd(3),deltaI(3)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    
    END IF

   ELSE ! l
    IF (iOrder(2) == 1) THEN     ! li
     IF (iOrder(3) == 2) THEN    ! lijk

      DO k=iStart(3),iEnd(3),deltaI(3)
       DO j=iStart(2),iEnd(2),deltaI(2)
        DO i=iStart(1),iEnd(1),deltaI(1)
         DO l=iStart(4),iEnd(4),deltaI(4)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! likj
      DO j=iStart(2),iEnd(2),deltaI(2)
       DO k=iStart(3),iEnd(3),deltaI(3)
        DO i=iStart(1),iEnd(1),deltaI(1)
         DO l=iStart(4),iEnd(4),deltaI(4)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    ELSE IF (iOrder(2) == 2) THEN ! lj
     IF (iOrder(3) == 1) THEN ! ljik

      DO k=iStart(3),iEnd(3),deltaI(3)
       DO i=iStart(1),iEnd(1),deltaI(1)
        DO j=iStart(2),iEnd(2),deltaI(2)
         DO l=iStart(4),iEnd(4),deltaI(4)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! ljki
      DO i=iStart(1),iEnd(1),deltaI(1)
       DO k=iStart(3),iEnd(3),deltaI(3)
        DO j=iStart(2),iEnd(2),deltaI(2)
         DO l=iStart(4),iEnd(4),deltaI(4)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF
    ELSE IF (iOrder(2) == 3) THEN ! lk
     IF (iOrder(3) == 1) THEN ! lkij

      DO j=iStart(2),iEnd(2),deltaI(2)
       DO i=iStart(1),iEnd(1),deltaI(1)
        DO k=iStart(3),iEnd(3),deltaI(3)
         DO l=iStart(4),iEnd(4),deltaI(4)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     ELSE ! lkji
      DO i=iStart(1),iEnd(1),deltaI(1)
       DO j=iStart(2),iEnd(2),deltaI(2)
        DO k=iStart(3),iEnd(3),deltaI(3)
         DO l=iStart(4),iEnd(4),deltaI(4)

          CALL AddLLEntry(object          = object,               &
                          messageData     = messageData(i,j,k,l), &
                          numberOfEntries = numEntries,           &
                          errorInfoObject = errorInfoObject)

          IF (CheckForLocalError(errorInfoObject)) THEN
           GO TO 101
          ELSE
           CONTINUE
          END IF
         END DO
        END DO
       END DO
      END DO
     END IF ! 3
    END IF  ! 2
   END IF   ! 1
  END IF    ! special case? 


