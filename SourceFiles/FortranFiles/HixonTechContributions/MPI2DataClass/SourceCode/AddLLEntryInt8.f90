
SUBROUTINE AddLLEntryInt8(object,          &
                          messageData,     &
                          numberOfEntries, &
                          errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER(KIND=int8Kind), INTENT(INOUT) :: messageData
  INTEGER, INTENT(IN) :: numberOfEntries
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  TYPE(MPIStructLLEntryType), POINTER :: newEntry => NULL()

  INTEGER :: mpiIerror

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryInt8'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  END IF

  IF (object%definitionIsComplete) THEN

   charStringObject%charString = 'MPIStructLLType object is already defined.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   RETURN
  ELSE
   CONTINUE
  END IF

  ALLOCATE(newEntry)
  NULLIFY(newEntry%previousEntry)
  NULLIFY(newEntry%nextEntry)

  object%numberOfLLEntries = object%numberOfLLEntries + 1 

  CALL HTMPI_GET_ADDRESS(messageData,                           &
                         newEntry%entryData%initialDataAddress, &
                         mpiIerror) 

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(charStringObject%charString,'(a40,i5)')  &
      'Error in MPI_GET_ADDRESS call:  IERROR = ',mpiIerror

   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  END IF

  newEntry%entryData%dataLength = numberOfEntries
  newEntry%entryData%dataType = GetMPITypeDef(messageData)

  IF (ASSOCIATED(object%tailOfLL)) THEN
   object%tailOfLL%nextEntry => newEntry
   newEntry%previousEntry => object%tailOfLL
   object%tailOfLL => newEntry 
  ELSE ! no list yet
   object%headOfLL => newEntry
   object%tailOfLL => newEntry
  END IF

  NULLIFY(newEntry)

  RETURN
END SUBROUTINE AddLLEntryInt8

SUBROUTINE AddLLEntryInt80D(object,        &
                            messageData,   &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER(KIND=int8Kind) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryInt80D'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  END IF

  CALL AddLLEntryInt8(object          = object,        &
                      messageData     = messageData,   &
                      numberOfEntries = 1,             &
                      errorInfoObject = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  ELSE
   CONTINUE
  END IF

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE AddLLEntryInt80D

SUBROUTINE AddLLEntryInt81D(object,        &
                            messageData,   &
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: iStart,iEnd,deltaI
  INTEGER(KIND=int8Kind), DIMENSION(:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: numEntries,i

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryInt1D'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  END IF

include 'AddLL1D.f90'

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE AddLLEntryInt81D

SUBROUTINE AddLLEntryInt82D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  INTEGER(KIND=int8Kind), DIMENSION(:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,numEntries,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryInt82D'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  END IF

include 'AddLL2D.f90'

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE AddLLEntryInt82D

SUBROUTINE AddLLEntryInt83D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  INTEGER(KIND=int8Kind), DIMENSION(:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,numEntries,i,j,k

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryInt83D'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  END IF

include 'AddLL3D.f90'

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE AddLLEntryInt83D

SUBROUTINE AddLLEntryInt84D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  INTEGER(KIND=int8Kind), DIMENSION(:,:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,i4,numEntries,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryInt84D'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  END IF

include 'AddLL4D.f90'

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE AddLLEntryInt84D

SUBROUTINE AddLLEntryInt85D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  INTEGER(KIND=int8Kind), DIMENSION(:,:,:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,i4,i5,numEntries,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryInt85D'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  END IF

include 'AddLL5D.f90'

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE AddLLEntryInt85D

