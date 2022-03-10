            CALL AddLLEntry(object          = object,                 &
                            messageData     = messageData(i,j,k,l,m), &
                            numberOfEntries = numEntries,             &
                            errorInfoObject = errorInfoObject)

            IF (CheckForLocalError(errorInfoObject)) THEN
             GO TO 101
            ELSE
             CONTINUE
            END IF

