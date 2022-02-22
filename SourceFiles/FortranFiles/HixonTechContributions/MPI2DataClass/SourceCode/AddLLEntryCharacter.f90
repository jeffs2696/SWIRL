
SUBROUTINE AddLLEntryChar(object,          &
                          messageData,     &
                          numberOfEntries, &
                          errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  CHARACTER, INTENT(INOUT) :: messageData
  INTEGER, INTENT(IN) :: numberOfEntries
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  TYPE(MPIStructLLEntryType), POINTER :: newEntry => NULL()

  INTEGER :: mpiIerror
! INTEGER(KIND=MPI_ADDRESS_KIND) :: addressDelta

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryChar0D'

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

! IF (ASSOCIATED(object%headOfLL)) THEN ! check the addressDelta
!  addressDelta = newEntry%entryData%initialDataAddress &
!               - object%headOfLL%entryData%initialDataAddress
!  WRITE(6,*) 'addressData(Char0D): ',               &
!      object%headOfLL%entryData%initialDataAddress, &
!             newEntry%entryData%initialDataAddress, &
!             addressDelta

!  IF (ABS(addressDelta) > INT(object%maxAddressDelta,MPI_ADDRESS_KIND)) THEN
!   charStringObject%charString = 'deltaAddress is too large: '

!   CALL SetError(object          = errorInfoObject, &
!                 errorInfoString = charStringObject)

!   WRITE(charStringObject%charString,'(a40,i21)')        &
!      'headOfLL initialDataAddress:             ',       &
!           object%headOfLL%entryData%initialDataAddress

!   CALL AddErrorInformation(object          = errorInfoObject, &
!                            errorInfoString = charStringObject)

!   WRITE(charStringObject%charString,'(a40,i21)')        &
!      'newEntry initialDataAddress:             ',       &
!           newEntry%entryData%initialDataAddress

!   CALL AddErrorInformation(object          = errorInfoObject, &
!                            errorInfoString = charStringObject)

!   WRITE(charStringObject%charString,'(a40,i21)')        &
!      'addressDelta:                            ',       &
!           addressDelta

!   CALL AddErrorInformation(object          = errorInfoObject, &
!                            errorInfoString = charStringObject)

!   WRITE(charStringObject%charString,'(a40,i21)')        &
!      'maxAddressDelta:                         ',       &
!           object%maxAddressDelta

!   CALL AddErrorInformation(object          = errorInfoObject, &
!                            errorInfoString = charStringObject)

!   charStringObject%charString = ' in '//location
!   CALL AddErrorInformation(object          = errorInfoObject, &
!                            errorInfoString = charStringObject)

!   charStringObject%charString = ' in '//moduleLocation
!   CALL AddErrorInformation(object          = errorInfoObject, &
!                            errorInfoString = charStringObject)

!   RETURN
!  ELSE
!   CONTINUE ! all is well
!  END IF
! ELSE
!  CONTINUE ! all is well -- addressDelta = 0
! END IF

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
END SUBROUTINE AddLLEntryChar

SUBROUTINE AddLLEntryChar0D(object,        &
                            messageData,   &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  CHARACTER :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryChar0D'

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

  CALL AddLLEntryChar(object          = object,        &
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
END SUBROUTINE AddLLEntryChar0D

SUBROUTINE AddLLEntryChar1D(object,        &
                            messageData,   &
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: iStart,iEnd,deltaI
  CHARACTER, DIMENSION(:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: numEntries,i

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryChar1D'

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
END SUBROUTINE AddLLEntryChar1D

SUBROUTINE AddLLEntryChar2D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  CHARACTER, DIMENSION(:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,numEntries,i,j

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryChar2D'

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
END SUBROUTINE AddLLEntryChar2D

SUBROUTINE AddLLEntryChar3D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  CHARACTER, DIMENSION(:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,numEntries,i,j,k

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryChar3D'

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
END SUBROUTINE AddLLEntryChar3D

SUBROUTINE AddLLEntryChar4D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  CHARACTER, DIMENSION(:,:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,i4,numEntries,i,j,k,l

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryChar4D'

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
END SUBROUTINE AddLLEntryChar4D

SUBROUTINE AddLLEntryChar5D(object,        &
                            messageData,   &
                            iOrder,        &  
                            iStart,        &
                            iEnd,          &
                            deltaI,        &
                            errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  CHARACTER, DIMENSION(:,:,:,:,:) :: messageData
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i1,i2,i3,i4,i5,numEntries,i,j,k,l,m

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryChar5D'

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
END SUBROUTINE AddLLEntryChar5D

