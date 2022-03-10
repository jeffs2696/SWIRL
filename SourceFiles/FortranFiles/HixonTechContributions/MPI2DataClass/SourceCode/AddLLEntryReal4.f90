
SUBROUTINE AddLLEntryReal4(object,          &
                           messageData,     &
                           numberOfEntries, &
                           errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  REAL(KIND=real4Kind), INTENT(INOUT) :: messageData
  INTEGER, INTENT(IN) :: numberOfEntries
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  TYPE(MPIStructLLEntryType), POINTER :: newEntry => NULL()

  INTEGER :: mpiIerror

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryReal4'

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
END SUBROUTINE AddLLEntryReal4

SUBROUTINE AddLLEntryReal40D(object,        &
                             messageData,   &
                             errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  REAL(KIND=real4Kind) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryReal40D'

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

  CALL AddLLEntryReal4(object          = object,        &
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
END SUBROUTINE AddLLEntryReal40D

SUBROUTINE AddLLEntryReal41D(object,        &
                             messageData,   &
                             iStart,        &
                             iEnd,          &
                             deltaI,        &
                             errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: iStart,iEnd,deltaI
  REAL(KIND=real4Kind), DIMENSION(:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: numEntries,i

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryReal41D'

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
END SUBROUTINE AddLLEntryReal41D

SUBROUTINE AddLLEntryReal42D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  REAL(KIND=real4Kind), DIMENSION(:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,numEntries,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryReal42D'

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
END SUBROUTINE AddLLEntryReal42D

SUBROUTINE AddLLEntryReal43D(object,        &
                             messageData,   &
                             iOrder,        &  
                             iStart,        &
                             iEnd,          &
                             deltaI,        &
                             errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  REAL(KIND=real4Kind), DIMENSION(:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,numEntries,i,j,k

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryReal43D'

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
END SUBROUTINE AddLLEntryReal43D

SUBROUTINE AddLLEntryReal44D(object,        &
                             messageData,   &
                             iOrder,        &  
                             iStart,        &
                             iEnd,          &
                             deltaI,        &
                             errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  REAL(KIND=real4Kind), DIMENSION(:,:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,i4,numEntries,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryReal44D'

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
END SUBROUTINE AddLLEntryReal44D

SUBROUTINE AddLLEntryReal45D(object,        &
                             messageData,   &
                             iOrder,        &  
                             iStart,        &
                             iEnd,          &
                             deltaI,        &
                             errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  REAL(KIND=real4Kind), DIMENSION(:,:,:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,i4,i5,numEntries,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryReal45D'

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
END SUBROUTINE AddLLEntryReal45D

