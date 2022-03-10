
  i1 = iOrder(1)
  i2 = iOrder(2)

  IF ((i1 == 1) .AND. (deltaI(1) == 1)) THEN ! special case where data length is not 1
   i = iStart(1)
   DO j=iStart(2),iEnd(2),deltaI(2)

    numEntries = iEnd(1)+1-iStart(1)

    CALL AddLLEntry(object          = object,           &
                    messageData     = messageData(i,j), &
                    numberOfEntries = numEntries,       &
                    errorInfoObject = errorInfoObject)

    IF (CheckForLocalError(errorInfoObject)) THEN
     GO TO 101
    ELSE
     CONTINUE
    END IF
   END DO
  ELSE  
   numEntries = 1

   IF (iOrder(1) == 1) THEN
    DO j=iStart(2),iEnd(2),deltaI(2)
     DO i=iStart(1),iEnd(1),deltaI(1)
      CALL AddLLEntry(object          = object,           &
                      messageData     = messageData(i,j), &
                      numberOfEntries = numEntries,       &
                      errorInfoObject = errorInfoObject)
 
      IF (CheckForLocalError(errorInfoObject)) THEN
       GO TO 101
      ELSE
       CONTINUE
      END IF
     END DO
    END DO

   ELSE ! other way
    DO i=iStart(1),iEnd(1),deltaI(1)
     DO j=iStart(2),iEnd(2),deltaI(2)
      CALL AddLLEntry(object          = object,           &
                      messageData     = messageData(i,j), &
                      numberOfEntries = numEntries,       &
                      errorInfoObject = errorInfoObject)
 
      IF (CheckForLocalError(errorInfoObject)) THEN
       GO TO 101
      ELSE
       CONTINUE
      END IF
     END DO
    END DO

   END IF
  END IF

