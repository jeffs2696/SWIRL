MODULE MPIStructLL

  USE MessagePassingInterface ! MessagePassingInterface.f90
  USE ErrorInformationClass   ! ErrorInformationClass.f90

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: MPIStructLLType,               &
            CreateObject,                  &
            AddDataToMPIStruct,            &
            MPIStructDefinitionIsComplete, &
            GetMPIStructArrays,            &
            DestroyObject

TYPE MPIStructLLEntryDataType
  PRIVATE

  INTEGER(KIND=MPI_ADDRESS_KIND) :: initialDataAddress = 0_MPI_ADDRESS_KIND
        
  INTEGER :: dataLength, &
             dataType

END TYPE MPIStructLLEntryDataType

TYPE MPIStructLLEntryType
  PRIVATE

  TYPE(MPIStructLLEntryType), POINTER :: previousEntry => NULL(), &
                                         nextEntry     => NULL()

  TYPE(MPIStructLLEntryDataType) :: entryData

END TYPE MPIStructLLEntryType

TYPE MPIStructLLType
  PRIVATE

  LOGICAL :: isInitialized        = .FALSE., &
             definitionIsComplete = .FALSE. 
  INTEGER :: numberOfLLEntries = 0, &
             maxAddressDelta = 0

  TYPE(MPIStructLLEntryType), POINTER :: headOfLL => NULL(), &
                                         tailOfLL => NULL()
END TYPE MPIStructLLType

INTEGER, PARAMETER :: mpiSuccess    = MPI_SUCCESS

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE MPIStructLL: '

INTERFACE CreateObject
  MODULE PROCEDURE CreateLLObject
END INTERFACE CreateObject

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyLLObject
END INTERFACE DestroyObject

INTERFACE AddDataToMPIStruct
  MODULE PROCEDURE AddLLEntryChar0D
  MODULE PROCEDURE AddLLEntryChar1D
  MODULE PROCEDURE AddLLEntryChar2D
  MODULE PROCEDURE AddLLEntryChar3D
  MODULE PROCEDURE AddLLEntryChar4D
  MODULE PROCEDURE AddLLEntryChar5D
  MODULE PROCEDURE AddLLEntryInt40D
  MODULE PROCEDURE AddLLEntryInt41D
  MODULE PROCEDURE AddLLEntryInt42D
  MODULE PROCEDURE AddLLEntryInt43D
  MODULE PROCEDURE AddLLEntryInt44D
  MODULE PROCEDURE AddLLEntryInt45D
  MODULE PROCEDURE AddLLEntryInt80D
  MODULE PROCEDURE AddLLEntryInt81D
  MODULE PROCEDURE AddLLEntryInt82D
  MODULE PROCEDURE AddLLEntryInt83D
  MODULE PROCEDURE AddLLEntryInt84D
  MODULE PROCEDURE AddLLEntryInt85D
  MODULE PROCEDURE AddLLEntryLogical0D
  MODULE PROCEDURE AddLLEntryLogical1D
  MODULE PROCEDURE AddLLEntryLogical2D
  MODULE PROCEDURE AddLLEntryLogical3D
  MODULE PROCEDURE AddLLEntryLogical4D
  MODULE PROCEDURE AddLLEntryLogical5D
  MODULE PROCEDURE AddLLEntryReal40D
  MODULE PROCEDURE AddLLEntryReal41D
  MODULE PROCEDURE AddLLEntryReal42D
  MODULE PROCEDURE AddLLEntryReal43D
  MODULE PROCEDURE AddLLEntryReal44D
  MODULE PROCEDURE AddLLEntryReal45D
  MODULE PROCEDURE AddLLEntryReal80D
  MODULE PROCEDURE AddLLEntryReal81D
  MODULE PROCEDURE AddLLEntryReal82D
  MODULE PROCEDURE AddLLEntryReal83D
  MODULE PROCEDURE AddLLEntryReal84D
  MODULE PROCEDURE AddLLEntryReal85D
END INTERFACE AddDataToMPIStruct

INTERFACE AddLLEntry
  MODULE PROCEDURE AddLLEntryChar
  MODULE PROCEDURE AddLLEntryInt4
  MODULE PROCEDURE AddLLEntryInt8
  MODULE PROCEDURE AddLLEntryReal4
  MODULE PROCEDURE AddLLEntryReal8
  MODULE PROCEDURE AddLLEntryLogical
END INTERFACE AddLLEntry

INTERFACE MPIStructDefinitionIsComplete
  MODULE PROCEDURE MPIStructDefComplete
END INTERFACE MPIStructDefinitionIsComplete

INTERFACE GetMPIStructArrays
  MODULE PROCEDURE GenerateStructArrays
END INTERFACE GetMPIStructArrays

CONTAINS

SUBROUTINE CreateLLObject(object,          &
                          messageTypeID,   &
                          maxAddressDelta, &
                          errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
!
! note to myself:  This single integer is what
!   'locates' the struct.
!
  INTEGER, INTENT(IN) :: messageTypeID, &
                         maxAddressDelta
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateLLObject'

  IF (object%isInitialized) THEN

   charStringObject%charString = 'MPIStructLLType object already initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   charStringObject%charString = ' in '//location//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   RETURN
  ELSE
   CONTINUE ! all is well
  END IF

  object%numberOfLLEntries = 0
  object%maxAddressDelta   = maxAddressDelta

  NULLIFY(object%headOfLL)
  NULLIFY(object%tailOfLL)
  object%definitionIsComplete = .FALSE. 

  object%isInitialized = .TRUE.

  CALL AddDataToMPIStruct(object          = object,        &
                          messageData     = messageTypeID, &
                          errorInfoObject = errorInfoObject)
 
  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CreateLLObject

include 'AddLLEntryInt4.f90'
include 'AddLLEntryInt8.f90'
include 'AddLLEntryLogical.f90'
include 'AddLLEntryReal4.f90'
include 'AddLLEntryReal8.f90'
include 'AddLLEntryCharacter.f90'

SUBROUTINE DestroyLLObject(object, &
                           errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyLLObject'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  DO i = 1,object%numberOfLLEntries

   CALL RemoveLLEntryFromTail(object          = object,        &
                              errorInfoObject = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   END IF

  END DO

  object%maxAddressDelta = -1

  object%isInitialized = .FALSE.
  object%definitionIsComplete = .FALSE. 

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE DestroyLLObject

SUBROUTINE RemoveLLEntryFromTail(object,        &
                                 errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  TYPE(MPIStructLLEntryType), POINTER :: currentEntry => NULL()

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemoveLLEntryFromTail'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  IF (ASSOCIATED(object%tailOfLL)) THEN
   currentEntry => object%tailOfLL
   IF (ASSOCIATED(currentEntry%previousEntry)) THEN
    object%tailOfLL => currentEntry%previousEntry
    NULLIFY(object%tailOfLL%nextEntry)
   ELSE ! last entry in LL
    NULLIFY(object%tailOfLL)
    NULLIFY(object%headOfLL)
   END IF

   DEALLOCATE(currentEntry)

  ELSE
   charStringObject%charString = 'MPIStructLLType object has no entries.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101

  END IF

  object%numberOfLLEntries = object%numberOfLLEntries - 1 

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
  
END SUBROUTINE RemoveLLEntryFromTail

SUBROUTINE MPIStructDefComplete(object,            &
                                structArrayLength, &
                                errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object
  INTEGER, INTENT(OUT) :: structArrayLength
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE MPIStructDefComplete'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  IF (object%definitionIsComplete) THEN

   charStringObject%charString = 'MPIStructLLType object is already defined.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  ELSE
   CONTINUE
  END IF

  structArrayLength = object%numberOfLLEntries
  object%definitionIsComplete = .TRUE.

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
  
END SUBROUTINE MPIStructDefComplete

SUBROUTINE GenerateStructArrays(object,                &
                                structArrayLength,     &
                                dataLengthArray,       &
                                dataAddressDeltaArray, &
                                dataTypeArray,         &
                                errorInfoObject)

  TYPE(MPIStructLLType), INTENT(INOUT) :: object

  INTEGER, INTENT(IN) :: structArrayLength

  INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(:), INTENT(INOUT) :: dataAddressDeltaArray

  INTEGER, DIMENSION(:), INTENT(INOUT) :: dataLengthArray, &
                                          dataTypeArray

  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  TYPE(MPIStructLLEntryType), POINTER :: currentEntry => NULL()
  INTEGER :: listEntryID
  INTEGER(KIND=MPI_ADDRESS_KIND) :: initialDataAddress

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GenerateStructArrays'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not initialized.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  IF (object%definitionIsComplete) THEN

   CONTINUE
  ELSE

   charStringObject%charString = 'MPIStructLLType object is not defined.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  IF (structArrayLength /= object%numberOfLLEntries) THEN
   WRITE(charStringObject%charString,'(a11,1x,i5)') ' Found:    ',object%numberOfLLEntries
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   WRITE(charStringObject%charString,'(a11,1x,i5)') ' Expected: ',structArrayLength
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'incoming struct array length is incorrect:'
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   GO TO 101
  ELSE
   CONTINUE
  END IF

! run down the LL, filling the data arrays

  listEntryID = 1
  currentEntry => object%headOfLL 

  initialDataAddress = currentEntry%entryData%initialDataAddress

 10 CONTINUE
  
  dataAddressDeltaArray(listEntryID) = currentEntry%entryData%initialDataAddress &
                                      -initialDataAddress

  dataLengthArray(listEntryID) = currentEntry%entryData%dataLength
  dataTypeArray(listEntryID)   = currentEntry%entryData%dataType

  IF (ASSOCIATED(currentEntry%nextEntry)) THEN
   currentEntry => currentEntry%nextEntry
   listEntryID = listEntryID + 1
   GO TO 10
  ELSE
   CONTINUE
  END IF

  IF (listEntryID /= object%numberOfLLEntries) THEN
   WRITE(charStringObject%charString,'(a11,1x,i5)') ' Found:    ',listEntryID
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   WRITE(charStringObject%charString,'(a11,1x,i5)') ' Expected: ',object%numberOfLLEntries
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = ' LL has the wrong number of entries!'
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   GO TO 101

  ELSE
   CONTINUE ! all is well
  END IF

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
  
END SUBROUTINE GenerateStructArrays

END MODULE MPIStructLL
