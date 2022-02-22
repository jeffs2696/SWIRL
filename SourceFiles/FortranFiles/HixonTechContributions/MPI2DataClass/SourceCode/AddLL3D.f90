
  i1 = iOrder(1)
  i2 = iOrder(2)
  i3 = iOrder(3)

  IF ((i1 == 1) .AND. (deltaI(1) == 1)) THEN ! special case where data length is not 1
   i = iStart(1)
   IF (i2 == 2) THEN
    DO k=iStart(3),iEnd(3),deltaI(3)
     DO j=iStart(2),iEnd(2),deltaI(2)

      numEntries = iEnd(1)+1-iStart(1)

      CALL AddLLEntry(object          = object,             &
                      messageData     = messageData(i,j,k), &
                      numberOfEntries = numEntries,         &
                      errorInfoObject = errorInfoObject)

      IF (CheckForLocalError(errorInfoObject)) THEN
       GO TO 101
      ELSE
       CONTINUE
      END IF
     END DO
    END DO
   ELSE
    DO j=iStart(2),iEnd(2),deltaI(2)
     DO k=iStart(3),iEnd(3),deltaI(3)

      numEntries = iEnd(1)+1-iStart(1)

      CALL AddLLEntry(object          = object,             &
                      messageData     = messageData(i,j,k), &
                      numberOfEntries = numEntries,         &
                      errorInfoObject = errorInfoObject)

      IF (CheckForLocalError(errorInfoObject)) THEN
       GO TO 101
      ELSE
       CONTINUE
      END IF
     END DO
    END DO
   END IF
  ELSE   
   numEntries = 1

   IF (iOrder(1) == 1) THEN
    IF (iOrder(2) == 2) THEN ! ijk
     DO k=iStart(3),iEnd(3),deltaI(3)
      DO j=iStart(2),iEnd(2),deltaI(2)
       DO i=iStart(1),iEnd(1),deltaI(1)
        CALL AddLLEntry(object          = object,             &
                        messageData     = messageData(i,j,k), &
                        numberOfEntries = numEntries,         &
                        errorInfoObject = errorInfoObject)
  
        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO
    ELSE ! ikj
     DO j=iStart(2),iEnd(2),deltaI(2)
      DO k=iStart(3),iEnd(3),deltaI(3)
       DO i=iStart(1),iEnd(1),deltaI(1)
        CALL AddLLEntry(object          = object,             &
                        messageData     = messageData(i,j,k), &
                        numberOfEntries = numEntries,         &
                        errorInfoObject = errorInfoObject)
  
        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO
    END IF ! i first
   ELSE IF (iOrder(1) == 2) THEN
    IF (iOrder(2) == 3) THEN ! jki
     DO i=iStart(1),iEnd(1),deltaI(1)
      DO k=iStart(3),iEnd(3),deltaI(3)
       DO j=iStart(2),iEnd(2),deltaI(2)
        CALL AddLLEntry(object          = object,             &
                        messageData     = messageData(i,j,k), &
                        numberOfEntries = numEntries,         &
                        errorInfoObject = errorInfoObject)
  
        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO
    ELSE ! jik
     DO k=iStart(3),iEnd(3),deltaI(3)
      DO i=iStart(1),iEnd(1),deltaI(1)
       DO j=iStart(2),iEnd(2),deltaI(2)
        CALL AddLLEntry(object          = object,             &
                        messageData     = messageData(i,j,k), &
                        numberOfEntries = numEntries,         &
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
   ELSE ! k is innermost
    IF (iOrder(2) == 1) THEN ! kij
     DO j=iStart(2),iEnd(2),deltaI(2)
      DO i=iStart(1),iEnd(1),deltaI(1)
       DO k=iStart(3),iEnd(3),deltaI(3)
        CALL AddLLEntry(object          = object,             &
                        messageData     = messageData(i,j,k), &
                        numberOfEntries = numEntries,         &
                        errorInfoObject = errorInfoObject)
  
        IF (CheckForLocalError(errorInfoObject)) THEN
         GO TO 101
        ELSE
         CONTINUE
        END IF
       END DO
      END DO
     END DO
    ELSE ! kji
     DO i=iStart(1),iEnd(1),deltaI(1)
      DO j=iStart(2),iEnd(2),deltaI(2)
       DO k=iStart(3),iEnd(3),deltaI(3)
        CALL AddLLEntry(object          = object,             &
                        messageData     = messageData(i,j,k), &
                        numberOfEntries = numEntries,         &
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
  END IF

