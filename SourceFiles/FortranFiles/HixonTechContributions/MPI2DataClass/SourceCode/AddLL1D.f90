  IF (deltaI == 1) THEN

   numEntries = iEnd+1-iStart

   CALL AddLLEntry(object          = object,              &
                   messageData     = messageData(iStart), &
                   numberOfEntries = numEntries,          &
                   errorInfoObject = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE ! have to do this one by one
   numEntries = 1
   DO i=iStart,iEnd,deltaI
    CALL AddLLEntry(object          = object,         &
                    messageData     = messageData(i), &
                    numberOfEntries = numEntries,     &
                    errorInfoObject = errorInfoObject)
 
    IF (CheckForLocalError(errorInfoObject)) THEN
     GO TO 101
    ELSE
     CONTINUE
    END IF
   END DO
  END IF 

