MODULE ErrorInformationClass

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE MessagePassingInterface ! MessagePassingInterface.f90

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: ErrorInformationType,    &
            CreateObject,            &
            DestroyObject,           &
            AddErrorInformation,     &
            AddMPIIErrorInformation, &
            SetError,                &
            ObjectIsInitialized,     &
            CheckForError,           &
            CheckForGlobalError,     &
            CheckForLocalError,      &
            CheckForNodeError,       &
            WriteObject,             &
            WriteObjectToFile,       &
            ReadObjectFromFile,      &
            CharacterStringType,     &
            CharacterStringLength,   &
            IsComment,               &
            IsKeyword

INTEGER, PARAMETER :: strLenDef = INT64  ! integer type used for string length
INTEGER(KIND=strLenDef), PARAMETER :: CharacterStringLength = 80

TYPE CharacterStringType
  CHARACTER(LEN=CharacterStringLength) :: charString
END TYPE CharacterStringType

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE ErrorInformationClass: '

TYPE CharacterStringLLType
  TYPE(CharacterStringLLType), POINTER :: prevLLEntry => NULL()
  LOGICAL :: firstLLEntry, &
             lastLLEntry
  INTEGER :: llEntryID
  TYPE(CharacterStringType) :: llEntryData
  TYPE(CharacterStringLLType), POINTER :: nextLLEntry => NULL()
END TYPE CharacterStringLLType

TYPE ErrorInformationType
  PRIVATE
  LOGICAL :: isInitialized = .FALSE.,     &
             nodeErrorDetected = .FALSE., &
             globalErrorDetected = .FALSE., &
             mpiMasterNode = .FALSE.
  LOGICAL, DIMENSION(:), ALLOCATABLE :: masterNodeErrorArray
  INTEGER, DIMENSION(:), ALLOCATABLE :: masterNodeNumStringsArray
  INTEGER :: numberOfLLEntries = 0, &
             mpiMessageTag = 1,     &
             myMPINodeID,           &
             numberOfMPINodes
  TYPE(CharacterStringLLType), POINTER :: headOfList => NULL(), &
                                          tailOfList => NULL()
END TYPE ErrorInformationType

INTERFACE CreateObject
  MODULE PROCEDURE CreateNodeEIObject1
  MODULE PROCEDURE CreateNodeEIObject2
END INTERFACE CreateObject

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyNodeEIObject
END INTERFACE DestroyObject

INTERFACE AddErrorInformation
  MODULE PROCEDURE AddErrorInformation1
END INTERFACE AddErrorInformation

INTERFACE AddMPIIErrorInformation
  MODULE PROCEDURE AddMPIIErrorInformation1
END INTERFACE AddMPIIErrorInformation

INTERFACE WriteObject
  MODULE PROCEDURE WriteErrorInformationToFile1
  MODULE PROCEDURE WriteErrorInformationToScreen1
  MODULE PROCEDURE WriteCharStringToFile1
END INTERFACE WriteObject

INTERFACE WriteObjectToFile
  MODULE PROCEDURE WriteErrorInformationToFile1
  MODULE PROCEDURE WriteCharStringToRestartFile1
END INTERFACE WriteObjectToFile

INTERFACE ReadObjectFromFile
  MODULE PROCEDURE ReadCharStringFromFile1
END INTERFACE ReadObjectFromFile

INTERFACE CheckForError
  MODULE PROCEDURE CheckForGlobalError1
END INTERFACE CheckForError

INTERFACE CheckForGlobalError
  MODULE PROCEDURE CheckForGlobalError1
END INTERFACE CheckForGlobalError

INTERFACE CheckForLocalError
  MODULE PROCEDURE CheckForLocalError1
END INTERFACE CheckForLocalError

INTERFACE CheckForNodeError
  MODULE PROCEDURE CheckForLocalError1
END INTERFACE CheckForNodeError

INTERFACE ObjectIsInitialized
  MODULE PROCEDURE ObjectIsInitialized1
END INTERFACE ObjectIsInitialized

INTERFACE IsComment
  MODULE PROCEDURE IsComment1
END INTERFACE IsComment

INTERFACE IsKeyword
  MODULE PROCEDURE IsKeyword1
END INTERFACE IsKeyword

CONTAINS

SUBROUTINE AddLLEntryToTail(headOfList,          &
                            tailOfList,          &
                            numberOfLLEntries,   &
                            errorInfoCharString)

  TYPE(CharacterStringLLType), POINTER :: headOfList, &
                                          tailOfList
  INTEGER, INTENT(OUT) :: numberOfLLEntries
  TYPE(CharacterStringType), INTENT(IN) :: errorInfoCharString

! local data

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddLLEntryToTail'

! NULLIFY(currentEntry)

! WRITE(6,*) 'AddLLEntryToTail: adding LL entry to list.'
! WRITE(6,*) 'numberOfLLEntries in = ',numberOfLLEntries

  IF (ASSOCIATED(headOfList)) THEN

!  WRITE(6,*) 'AddLLEntryToTail: there is a headOfList'

   ALLOCATE(tailOfList%nextLLEntry)
   tailOfList%nextLLEntry%prevLLEntry => tailOfList
   tailOfList%lastLLEntry = .FALSE.

   tailOfList => tailOfList%nextLLEntry
   tailOfList%firstLLEntry = .FALSE.
   tailOfList%lastLLEntry = .TRUE.
   tailOfList%llEntryID = tailOfList%prevLLEntry%llEntryID + 1
   numberOfLLEntries = tailOfList%llEntryID
   tailOfList%llEntryData = errorInfoCharString
   NULLIFY(tailOfList%nextLLEntry)
  ELSE
!  WRITE(6,*) 'AddLLEntryToTail: there is no headOfList'

   ALLOCATE(headOfList)
   headOfList%firstLLEntry = .TRUE.
   headOfList%lastLLEntry = .TRUE.
   headOfList%llEntryID = 1
   NULLIFY(headOfList%prevLLEntry)
   NULLIFY(headOfList%nextLLEntry)
   headOfList%llEntryData = errorInfoCharString
   tailOfList => headOfList
   numberOfLLEntries = 1
  END IF

! WRITE(6,*) 'numberOfLLEntries in = ',numberOfLLEntries

  RETURN
END SUBROUTINE AddLLEntryToTail

SUBROUTINE RemoveLastLLEntry(headOfList,          &
                             tailOfList,          &
                             numberOfLLEntries)

  TYPE(CharacterStringLLType), POINTER :: headOfList, &
                                          tailOfList
  INTEGER, INTENT(OUT) :: numberOfLLEntries

! local data

  TYPE(CharacterStringLLType), POINTER :: currentEntry
  CHARACTER(LEN=*), PARAMETER :: location = &
      'SUBROUTINE RemoveLastLLEntry'

  NULLIFY(currentEntry)

  IF (numberOfLLEntries == 0) RETURN

  IF (ASSOCIATED(tailOfList)) THEN
   currentEntry => tailOfList
  ELSE
   numberOfLLEntries = 0
   RETURN
  END IF 

  IF (currentEntry%firstLLEntry) THEN ! only one entry?
   DEALLOCATE(currentEntry)
   NULLIFY(headOfList)
   NULLIFY(tailOfList)
   NULLIFY(currentEntry)
   numberOfLLEntries = 0
   RETURN
  ELSE 
   tailOfList => currentEntry%prevLLEntry
   tailOfList%lastLLEntry = .TRUE.
   numberOfLLEntries = tailOfList%llEntryID
   NULLIFY(tailOfList%nextLLEntry)
   DEALLOCATE(currentEntry)
   NULLIFY(currentEntry)
  END IF 

  RETURN
END SUBROUTINE RemoveLastLLEntry

SUBROUTINE GetLLEntry(object,               &
                      llEntryID,            &    
                      errorInformationString)

  TYPE(ErrorInformationType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: llEntryID
  TYPE(CharacterStringType), INTENT(OUT) :: errorInformationString

! local data

  TYPE(CharacterStringLLType), POINTER :: currentEntry
  CHARACTER(LEN=*), PARAMETER :: location = &
     'SUBROUTINE GetLLEntry'
  TYPE(CharacterStringType) :: errorInfo

  NULLIFY(currentEntry)

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE
   CALL CreateObject(object = object)
   object%nodeErrorDetected = .TRUE.
   errorInfo%charString = 'Error object is not initialized.'

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)
  
   errorInfo%charString = location//moduleLocation

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)
  
   RETURN
  END IF

  IF (object%numberOfLLEntries == 0) RETURN

  IF (ASSOCIATED(object%headOfList)) THEN
   currentEntry => object%headOfList
  ELSE
   object%numberOfLLEntries = 0
   errorInformationString%charString = 'Error:  no error information'
   RETURN
  END IF 

 20 CONTINUE

  IF (llEntryID == currentEntry%llEntryID) THEN ! correct entry
   errorInformationString =currentEntry%llEntryData
  ELSE
   IF (currentEntry%lastLLEntry) THEN
    errorInformationString%charString = 'Error:  entry not found'
   ELSE
    currentEntry => currentEntry%nextLLEntry
    GO TO 20
   END IF
  END IF

  NULLIFY(currentEntry)

  RETURN

END SUBROUTINE GetLLEntry

SUBROUTINE CreateNodeEIObject1(object,mpiMessageTag)
  TYPE(ErrorInformationType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: mpiMessageTag

  TYPE(CharacterStringType) :: errorInfo
  CHARACTER(LEN=*), PARAMETER :: location = &
     'SUBROUTINE CreateNodeEIObject1'

  LOGICAL :: mpiFlag
  INTEGER :: mpiError, &
             mpiRank,  &
             i

! check if MPI has been started

  IF (object%isInitialized) THEN
   CALL DestroyObject(object)
   object%isInitialized = .TRUE.
   object%nodeErrorDetected = .TRUE.
   object%mpiMessageTag = mpiMessageTag
   object%numberOfLLEntries = 0
   errorInfo%charString = 'Error object is already initialized.'

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)
  
   errorInfo%charString = location//moduleLocation

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)
  
   RETURN
  ELSE
   object%isInitialized     = .TRUE.
   object%nodeErrorDetected     = .FALSE.
   object%globalErrorDetected   = .FALSE.
   object%numberOfLLEntries = 0
  END IF

  CALL HTMPI_INITIALIZED(mpiFlag, & ! FLAG
                         mpiError)  ! IERROR

  IF (mpiFlag) THEN ! MPI is initialized
   CONTINUE
  ELSE   ! error; need MPI running
   object%nodeErrorDetected = .TRUE.
   errorInfo%charString = ' MPI is not initialized.'

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)

   errorInfo%charString = location//moduleLocation

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)
  
   ALLOCATE(object%masterNodeErrorArray(0))
   ALLOCATE(object%masterNodeNumStringsArray(0))

   RETURN
  END IF

! allocate the array of LOGICALS

  CALL HTMPI_COMM_SIZE(MPI_COMM_WORLD,          & ! COMM
                       object%numberOfMPINodes, & ! SIZE
                       mpiError)                  ! IERROR
  
  CALL HTMPI_COMM_RANK(MPI_COMM_WORLD, & ! COMM
                       mpiRank,        & ! RANK
                       mpiError)         ! IERROR
  
  object%myMPINodeID = mpiRank + 1
  IF (object%myMPINodeID == 1) THEN
   object%mpiMasterNode = .TRUE.
  ELSE
   object%mpiMasterNode = .FALSE.
  END IF

  ALLOCATE(object%masterNodeErrorArray(object%numberOfMPINodes))
  ALLOCATE(object%masterNodeNumStringsArray(object%numberOfMPINodes))
  DO i=1,object%numberOfMPINodes
   object%masterNodeErrorArray(i) = .FALSE.
  END DO

  RETURN
END SUBROUTINE CreateNodeEIObject1

SUBROUTINE CreateNodeEIObject2(object)
  TYPE(ErrorInformationType), INTENT(INOUT) :: object

  INTEGER :: mpiMessageTag = 1
  CHARACTER(LEN=*), PARAMETER :: location = &
     'SUBROUTINE CreateNodeEIObject2'
  TYPE(CharacterStringType) :: errorInfo

  CALL CreateObject(object        = object, &
                    mpiMessageTag = mpiMessageTag)

  IF (CheckForLocalError(object)) THEN
   errorInfo%charString = 'CALLED from: '//location//moduleLocation

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)
   errorInfo%charString = ''
  END IF

  RETURN
END SUBROUTINE CreateNodeEIObject2

SUBROUTINE DestroyNodeEIObject(object)
  TYPE(ErrorInformationType), INTENT(INOUT) :: object

  INTEGER :: n,numEntries
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyNodeEIObject: '

  IF (object%isInitialized) THEN
   numEntries = object%numberOfLLEntries
   DO n = 1,numEntries
    CALL RemoveLastLLEntry(headOfList        = object%headOfList,      &
                           tailOfList        = object%tailOfList,      &
                           numberOfLLEntries = object%numberOfLLEntries)
   END DO
   object%isInitialized = .FALSE.
   DEALLOCATE(object%masterNodeErrorArray)
   DEALLOCATE(object%masterNodeNumStringsArray)
  ELSE 
   CONTINUE
  END IF
  RETURN
END SUBROUTINE DestroyNodeEIObject

SUBROUTINE AddErrorInformation1(object,               &
                                errorInfoString)

  TYPE(ErrorInformationType), INTENT(INOUT) :: object
  TYPE(CharacterStringType), INTENT(IN) :: errorInfoString

  TYPE(CharacterStringType) :: localErrorInfo
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddErrorInformation1'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE
   CALL CreateObject(object = object)
   object%nodeErrorDetected = .TRUE.
   localErrorInfo%charString = 'Error object is not initialized.'
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = localErrorInfo)

   localErrorInfo%charString = location//moduleLocation

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = localErrorInfo)
  
   RETURN
  END IF

  IF (object%nodeErrorDetected) THEN
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfoString)
  ELSE ! set the error
   object%nodeErrorDetected = .TRUE.
   WRITE(localErrorInfo%charString,*) ' Node: ',object%myMPINodeID, &
                                 ': Adding error information without setting error: '
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = localErrorInfo)
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfoString)
  END IF

  RETURN
END SUBROUTINE AddErrorInformation1

SUBROUTINE AddMPIIErrorInformation1(object,               &
                                    mpiIErrorValue)

  TYPE(ErrorInformationType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: mpiIErrorValue

  TYPE(CharacterStringType) :: localErrorInfo, &
                               errorInfoString
  CHARACTER(LEN=800) :: mpiErrorString
  INTEGER :: resultLen,   &
             numStrings,  &
             nS,          &
             iError

  INTEGER(KIND=strLenDef) :: stringStart, &
                             stringEnd,   &
                             stringLen

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddMPIIErrorInformation1'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE
   CALL CreateObject(object = object)
   object%nodeErrorDetected = .TRUE.
   localErrorInfo%charString = 'Error object is not initialized.'
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = localErrorInfo)

   localErrorInfo%charString = location//moduleLocation

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = localErrorInfo)
  
   RETURN
  END IF

  IF (mpiIErrorValue == 0) THEN ! no MPI error, so keep going.
   RETURN
  ELSE
   CONTINUE
  END IF

  CALL HTMPI_ERROR_STRING(ERRORCODE = mpiIErrorValue, &
                          STRING    = mpiErrorString, &
                          RESULTLEN = resultLen,      &
                          IERROR    = iError)

  numStrings = CEILING(REAL(resultLen)/REAL(characterStringLength))

! WRITE(0,*) 'MPI Error string data: ',resultLen,characterStringLength,numStrings

  DO nS = numStrings,1,-1
   stringStart = 1 + characterStringLength*INT(nS-1,strLenDef) 
   stringEnd = MIN(INT(resultLen,strLenDef),characterStringLength*INT(nS,strLenDef))
   stringLen = stringEnd+1-stringStart
!  errorInfoString%charString(1:stringLen) = mpiErrorString(stringStart:stringEnd)
   errorInfoString%charString = mpiErrorString(stringStart:stringEnd)
!  WRITE(0,*) nS,stringStart,stringEnd,stringLen,errorInfoString%charString

   IF (object%nodeErrorDetected) THEN
    CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                          tailOfList          = object%tailOfList,        &
                          numberOfLLEntries   = object%numberOfLLEntries, &
                          errorInfoCharString = errorInfoString)
   ELSE ! set the error
    object%nodeErrorDetected = .TRUE.
    WRITE(localErrorInfo%charString,*) ' Node: ',object%myMPINodeID, &
                                  ': Adding error information without setting error. '
    CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                          tailOfList          = object%tailOfList,        &
                          numberOfLLEntries   = object%numberOfLLEntries, &
                          errorInfoCharString = localErrorInfo)
    CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                          tailOfList          = object%tailOfList,        &
                          numberOfLLEntries   = object%numberOfLLEntries, &
                          errorInfoCharString = errorInfoString)
   END IF
  END DO

  RETURN
END SUBROUTINE AddMPIIErrorInformation1

SUBROUTINE WriteErrorInformationToFile1(object,        &
                                        unitNumber,    &
                                        fileIsFormatted)
  
  TYPE(ErrorInformationType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: unitNumber
  LOGICAL, INTENT(IN) :: fileIsFormatted

  TYPE(CharacterStringType) :: errorInfoString
  INTEGER :: numLLEntries,nEntry,i,mpiIError,ii
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: mpiStatusArray
  CHARACTER(LEN=*), PARAMETER :: location = &
   'SUBROUTINE WriteErrorInformationToFile1'

  CHARACTER, DIMENSION(80) :: mpiCharArray

  IF (object%isInitialized) THEN
   IF (object%numberOfMPINodes > 1) THEN ! need message passing

! do a GATHER call to get the number of LL entries on each node

    CALL HTMPI_GATHER(object%numberOfLLEntries,         & ! SENDBUF
                      1,                                & ! SENDCOUNT
                      MPI_INTEGER,                      & ! SENDTYPE
                      object%masterNodeNumStringsArray, & ! RECVBUF
                      1,                                & ! RECVCOUNT
                      MPI_INTEGER,                      & ! RECVTYPE
                      0,                                & ! ROOT
                      MPI_COMM_WORLD,                   & ! COMM
                      mpiIError)                          ! IERROR

    IF (object%mpiMasterNode) THEN
     errorInfoString%charString = '----------------------------------------------------------------'
     CALL WriteObject(object          = errorInfoString, &
                      unitNumber      = unitNumber,      &
                      fileIsFormatted = fileIsFormatted)
     WRITE(errorInfoString%charString,*) 'MPI node ',1,' Errors: '
     CALL WriteObject(object          = errorInfoString, &
                      unitNumber      = unitNumber,      &
                      fileIsFormatted = fileIsFormatted)
     numLLEntries = object%numberOfLLEntries
     IF (numLLEntries > 0) THEN
      DO nEntry = numLLEntries,1,-1
       CALL GetLLEntry(object                 = object,        &
                       llEntryID              = nEntry,        &    
                       errorInformationString = errorInfoString)

       CALL WriteObject(object          = errorInfoString, &
                        unitNumber      = unitNumber,      &
                        fileIsFormatted = fileIsFormatted)
      END DO
     ELSE
      errorInfoString%charString = location//moduleLocation
      CALL WriteObject(object          = errorInfoString, &
                       unitNumber      = unitNumber,      &
                       fileIsFormatted = fileIsFormatted)
      errorInfoString%charString = ' no error detected.'
      CALL WriteObject(object          = errorInfoString, &
                       unitNumber      = unitNumber,      &
                       fileIsFormatted = fileIsFormatted)
     END IF 

! then run through to see if the other nodes need to send messages

     DO i=2,object%numberOfMPINodes
      errorInfoString%charString = '----------------------------------------------------------------'
      CALL WriteObject(object          = errorInfoString, &
                       unitNumber      = unitNumber,      &
                       fileIsFormatted = fileIsFormatted)
      IF (object%masterNodeNumStringsArray(i) > 0) THEN
       WRITE(errorInfoString%charString,*) 'MPI node ',i,' Errors: '
       CALL WriteObject(object          = errorInfoString, &
                        unitNumber      = unitNumber,      &
                        fileIsFormatted = fileIsFormatted)
       DO nEntry = object%masterNodeNumStringsArray(i),1,-1
! call a blocking recv for master
    
        CALL HTMPI_RECV(mpiCharArray,         & ! BUF
                        80,                   & ! COUNT
                        MPI_CHARACTER,        & ! DATATYPE
                        i-1,                  & ! SOURCE
                        object%mpiMessageTag, & ! TAG
                        MPI_COMM_WORLD,       & ! COMM
                        mpiStatusArray,       & ! STATUS
                        mpiIError)              ! IERROR
    
        DO ii=1,80
         errorInfoString%charString(ii:ii) = mpiCharArray(ii) 
        END DO

        CALL WriteObject(object          = errorInfoString, &
                         unitNumber      = unitNumber,      &
                         fileIsFormatted = fileIsFormatted)

       END DO
      ELSE
       errorInfoString%charString = '----------------------------------------------------------------'
       CALL WriteObject(object          = errorInfoString, &
                        unitNumber      = unitNumber,      &
                        fileIsFormatted = fileIsFormatted)
       WRITE(errorInfoString%charString,*) 'MPI node ',i,' No error detected. '
       CALL WriteObject(object          = errorInfoString, &
                        unitNumber      = unitNumber,      &
                        fileIsFormatted = fileIsFormatted)
      END IF
     END DO

     errorInfoString%charString = '----------------------------------------------------------------'
     CALL WriteObject(object          = errorInfoString, &
                      unitNumber      = unitNumber,      &
                      fileIsFormatted = fileIsFormatted)

    ELSE ! not a master node
     IF (object%numberOfLLEntries > 0) THEN
       DO nEntry = object%numberOfLLEntries,1,-1

        CALL GetLLEntry(object                 = object,        &
                        llEntryID              = nEntry,        &    
                        errorInformationString = errorInfoString)

        DO ii=1,80
         mpiCharArray(ii) = errorInfoString%charString(ii:ii)
        END DO

! call a blocking send to master
        CALL HTMPI_SEND(mpiCharArray,         & ! BUF
                        80,                   & ! COUNT
                        MPI_CHARACTER,        & ! DATATYPE
                        0,                    & ! DEST
                        object%mpiMessageTag, & ! TAG
                        MPI_COMM_WORLD,       & ! COMM
                        mpiIError)              ! IERROR
       END DO
      CONTINUE
     ELSE
      CONTINUE
     END IF
    END IF
   ELSE ! single node
    errorInfoString%charString = '----------------------------------------------------------------'
    CALL WriteObject(object          = errorInfoString, &
                     unitNumber      = unitNumber,      &
                     fileIsFormatted = fileIsFormatted)
    numLLEntries = object%numberOfLLEntries
    IF (numLLEntries > 0) THEN
     DO nEntry = numLLEntries,1,-1
      CALL GetLLEntry(object                 = object,        &
                      llEntryID              = nEntry,        &    
                      errorInformationString = errorInfoString)

      CALL WriteObject(object          = errorInfoString, &
                       unitNumber      = unitNumber,      &
                       fileIsFormatted = fileIsFormatted)
     END DO
    ELSE
     errorInfoString%charString = 'in '//location
     CALL WriteObject(object          = errorInfoString, &
                      unitNumber      = unitNumber,      &
                      fileIsFormatted = fileIsFormatted)
     errorInfoString%charString = moduleLocation
     CALL WriteObject(object          = errorInfoString, &
                      unitNumber      = unitNumber,      &
                      fileIsFormatted = fileIsFormatted)
     errorInfoString%charString = ' no error detected.'
     CALL WriteObject(object          = errorInfoString, &
                      unitNumber      = unitNumber,      &
                      fileIsFormatted = fileIsFormatted)
    END IF 
    errorInfoString%charString = '----------------------------------------------------------------'
    CALL WriteObject(object          = errorInfoString, &
                     unitNumber      = unitNumber,      &
                     fileIsFormatted = fileIsFormatted)
   END IF ! MPI?
  ELSE
   IF (object%mpiMasterNode) THEN
    errorInfoString%charString = '----------------------------------------------------------------'
    CALL WriteObject(object          = errorInfoString, &
                     unitNumber      = unitNumber,      &
                     fileIsFormatted = fileIsFormatted)
    errorInfoString%charString = 'in '//location
    CALL WriteObject(object          = errorInfoString, &
                     unitNumber      = unitNumber,      &
                     fileIsFormatted = fileIsFormatted)
    errorInfoString%charString = moduleLocation
    CALL WriteObject(object          = errorInfoString, &
                     unitNumber      = unitNumber,      &
                     fileIsFormatted = fileIsFormatted)
    errorInfoString%charString = 'Error object is not initialized!'
    CALL WriteObject(object          = errorInfoString, &
                     unitNumber      = unitNumber,      &
                     fileIsFormatted = fileIsFormatted)
    errorInfoString%charString = '----------------------------------------------------------------'
    CALL WriteObject(object          = errorInfoString, &
                     unitNumber      = unitNumber,      &
                     fileIsFormatted = fileIsFormatted)
   END IF 
  END IF 

  RETURN
END SUBROUTINE WriteErrorInformationToFile1

SUBROUTINE WriteCharStringToFile1(object,        &
                                  unitNumber,    &
                                  fileIsFormatted)

  TYPE(CharacterStringType), INTENT(IN) :: object
  INTEGER, INTENT(IN) :: unitNumber
  LOGICAL, INTENT(IN) :: fileIsFormatted
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE WriteCharStringToFile1'

  INTEGER :: stringLength

  stringLength =  LEN(TRIM(ADJUSTL(object%charString)))

  IF (fileIsFormatted) THEN
   WRITE(UNIT=unitNumber,FMT='(a)') &
    object%charString(1:CharacterStringLength)
  ELSE
   WRITE(UNIT=unitNumber) &
    object%charString(1:CharacterStringLength)
  END IF
  RETURN
END SUBROUTINE WriteCharStringToFile1

SUBROUTINE WriteCharStringToRestartFile1(object,        &
                                         unitNumber,    &
                                         fileIsFormatted)

  TYPE(CharacterStringType), INTENT(IN) :: object
  INTEGER, INTENT(IN) :: unitNumber
  LOGICAL, INTENT(IN) :: fileIsFormatted
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE WriteCharStringToRestartFile1'

  INTEGER :: stringLength

  stringLength =  LEN(TRIM(ADJUSTL(object%charString)))

  IF (fileIsFormatted) THEN
   WRITE(UNIT=unitNumber,FMT='(i2.2)') stringLength
   WRITE(UNIT=unitNumber,FMT='(a)') &
    TRIM(ADJUSTL(object%charString))
  ELSE
   WRITE(UNIT=unitNumber) stringLength
   WRITE(UNIT=unitNumber) &
    TRIM(ADJUSTL(object%charString))
  END IF
  RETURN
END SUBROUTINE WriteCharStringToRestartFile1

SUBROUTINE ReadCharStringFromFile1(object,        &
                                   unitNumber,    &
                                   fileIsFormatted)

  TYPE(CharacterStringType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: unitNumber
  LOGICAL, INTENT(IN) :: fileIsFormatted
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE ReadCharStringFromFile1'

  INTEGER(KIND=strLenDef) :: stringLength

! erase the charString, just in case

  object%charString(1:CharacterStringLength) = ' '

! get the string length, then read in

  stringLength =  INT(LEN(TRIM(ADJUSTL(object%charString))),strLenDef)

  IF (fileIsFormatted) THEN
   READ(UNIT=unitNumber,FMT='(i2.2)') stringLength
   READ(UNIT=unitNumber,FMT='(a)') object%charString(1:stringLength)
  ELSE
   READ(UNIT=unitNumber) stringLength
   READ(UNIT=unitNumber) object%charString(1:stringLength)
  END IF
  RETURN
END SUBROUTINE ReadCharStringFromFile1

SUBROUTINE WriteErrorInformationToScreen1(object)
  
  TYPE(ErrorInformationType), INTENT(INOUT) :: object
  INTEGER :: unitNumber
  LOGICAL :: fileIsFormatted

  TYPE(CharacterStringType) :: errorInfoString
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE WriteErrorInformationToScreen1'

  unitNumber      = 0
  fileIsFormatted = .TRUE.

  CALL WriteObject(object          = object,        &
                   unitNumber      = unitNumber,    &
                   fileIsFormatted = fileIsFormatted)

  IF (CheckForLocalError(object)) THEN
   errorInfoString%charString = 'CALLed from '//location
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfoString)

   errorInfoString%charString = '   ...'//moduleLocation
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfoString)

  END IF

  RETURN
END SUBROUTINE WriteErrorInformationToScreen1

LOGICAL FUNCTION CheckForGlobalError1(object)
  TYPE(ErrorInformationType), INTENT(INOUT) :: object

  TYPE(CharacterStringType) :: errorInfoString
  CHARACTER(LEN=*), PARAMETER :: location = &
     'LOGICAL FUNCTION CheckForGlobalError'

  INTEGER :: mpiIError

  IF (object%isInitialized) THEN

!  if there is already an error, you don't need to check again

   IF (object%globalErrorDetected) THEN
    CheckForGlobalError1 = object%globalErrorDetected
    RETURN
   ELSE
    CONTINUE
   END IF

   IF (object%numberOfMPINodes > 1) THEN ! need message passing

! do a GATHER call to get the error flags
! note that the RECVCOUNT on a GATHER call is the number of
!  data entries from _each_ node.

    CALL HTMPI_GATHER(object%nodeErrorDetected,    & ! SENDBUF
                      1,                           & ! SENDCOUNT
                      MPI_LOGICAL,                 & ! SENDTYPE
                      object%masterNodeErrorArray, & ! RECVBUF
                      1,                           & ! RECVCOUNT
                      MPI_LOGICAL,                 & ! RECVTYPE
                      0,                           & ! ROOT
                      MPI_COMM_WORLD,              & ! COMM
                      mpiIError)                     ! IERROR

    IF (object%mpiMasterNode) THEN
     IF (ANY(object%masterNodeErrorArray)) THEN
      object%globalErrorDetected = .TRUE.
     ELSE
      object%globalErrorDetected = .FALSE.
     END IF
    ELSE
     CONTINUE
    END IF

! everybody calls a BCAST to get the global result.

    CALL HTMPI_BCAST(object%globalErrorDetected, & ! BUF
                     1,                          & ! COUNT
                     MPI_LOGICAL,                & ! DATATYPE
                     0,                          & ! ROOT
                     MPI_COMM_WORLD,             & ! COMM
                     mpiIError)                    ! IERROR
       
    CheckForGlobalError1 = object%globalErrorDetected
!
! at this point, need to set a node error if a global error has been
!  detected.
!
    IF (object%globalErrorDetected) THEN
     IF (object%nodeErrorDetected) THEN
      CONTINUE ! already have an error
     ELSE
      object%nodeErrorDetected = .TRUE.
      errorInfoString%charString = 'No error detected by this node.'

      CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                            tailOfList          = object%tailOfList,        &
                            numberOfLLEntries   = object%numberOfLLEntries, &
                            errorInfoCharString = errorInfoString)

     END IF
    ELSE
     CONTINUE ! all is well
    END IF
   ELSE ! only one node
    object%globalErrorDetected = object%nodeErrorDetected
    CheckForGlobalError1       = object%globalErrorDetected
   END IF
  ELSE
   CALL CreateObject(object = object)
   object%nodeErrorDetected = .TRUE.
   object%globalErrorDetected = .TRUE.
   CheckForGlobalError1 = object%globalErrorDetected
   errorInfoString%charString = 'Error object is not initialized.'

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfoString)

   errorInfoString%charString = location//moduleLocation
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfoString)

  END IF

  RETURN
END FUNCTION CheckForGlobalError1

LOGICAL FUNCTION CheckForLocalError1(object)
  TYPE(ErrorInformationType), INTENT(INOUT) :: object

  TYPE(CharacterStringType) :: errorInfoString
  CHARACTER(LEN=*), PARAMETER :: location = &
     'LOGICAL FUNCTION CheckForLocalError'

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE
   CALL CreateObject(object = object)
   object%nodeErrorDetected = .TRUE.
   CheckForLocalError1 = object%nodeErrorDetected
   errorInfoString%charString = 'Error object is not initialized.'

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfoString)

   errorInfoString%charString = location//moduleLocation
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfoString)

   RETURN
  END IF

  CheckForLocalError1 = object%nodeErrorDetected

! check if we have a node error without a global error.
!   If this is the case, call a CheckForGlobalError in order
!    to wait for the other nodes to call a CheckForGlobalError
!    at some point during the run.

  IF (object%nodeErrorDetected) THEN
   IF (object%globalErrorDetected) THEN
    CONTINUE ! all is well; both errors are .TRUE.
   ELSE ! nodeErrorDetected but not globalErrorDetected.
    IF(CheckForGlobalError(object)) THEN ! this should make node wait until 
                                         !  CheckForGlobalError is called by all nodes.
     CONTINUE ! all is well
    ELSE
     errorInfoString%charString =  &
        'Error information problem:  node error found without global error.'

     CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                           tailOfList          = object%tailOfList,        &
                           numberOfLLEntries   = object%numberOfLLEntries, &
                           errorInfoCharString = errorInfoString)
    END IF
   END IF
  ELSE
   CONTINUE
  END IF

  RETURN
END FUNCTION CheckForLocalError1

SUBROUTINE SetError(object,errorInfoString)
  TYPE(ErrorInformationType), INTENT(INOUT) :: object
  TYPE(CharacterStringType), INTENT(IN) :: errorInfoString
  CHARACTER(LEN=*), PARAMETER :: location = &
      'SUBROUTINE SetError'
  TYPE(CharacterStringType) :: errorInfo

  IF (object%isInitialized) THEN
   CONTINUE
  ELSE
   CALL CreateObject(object = object)
   object%nodeErrorDetected = .TRUE.
   errorInfo%charString = 'Error object is not initialized.'

   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)
  
   errorInfo%charString = location//moduleLocation
   CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                         tailOfList          = object%tailOfList,        &
                         numberOfLLEntries   = object%numberOfLLEntries, &
                         errorInfoCharString = errorInfo)

   RETURN
  END IF

  object%nodeErrorDetected = .TRUE.

  CALL AddLLEntryToTail(headOfList          = object%headOfList,        &
                        tailOfList          = object%tailOfList,        &
                        numberOfLLEntries   = object%numberOfLLEntries, &
                        errorInfoCharString = errorInfoString)
  
  RETURN
END SUBROUTINE SetError

LOGICAL FUNCTION ObjectIsInitialized1(object)
  TYPE(ErrorInformationType), INTENT(INOUT) :: object

  ObjectIsInitialized1 = object%isInitialized

  RETURN
END FUNCTION ObjectIsInitialized1

FUNCTION CharStringsAreEqual1(charString1,charString2,stringCheckLength)
  LOGICAL :: CharStringsAreEqual1
  CHARACTER(LEN=*), INTENT(IN) :: charString1,charString2
  INTEGER, INTENT(IN) :: stringCheckLength

! define local variables

  INTEGER, PARAMETER :: &
          lowerToUpperShift = IACHAR("A")-IACHAR("a")
  INTEGER :: n
  CHARACTER(LEN=1) :: workingObject1,workingObject2
  CHARACTER(LEN=*), PARAMETER :: &
          location = 'FUNCTION StringsAreEqual: '

  CharStringsAreEqual1 = .TRUE.

! convert string1 to lowercase

  DO n=1,stringCheckLength
   IF ("a" <= charString1(n:n) .AND. "z" >= charString1(n:n)) THEN
    workingObject1(1:1) = ACHAR(IACHAR(charString1(n:n))+lowerToUpperShift)
   ELSE
    workingObject1(1:1) = charString1(n:n)
   END IF
   IF ("a" <= charString2(n:n) .AND. "z" >= charString2(n:n)) THEN
    workingObject2(1:1) = ACHAR(IACHAR(charString2(n:n))+lowerToUpperShift)
   ELSE
    workingObject2(1:1) = charString2(n:n)
   END IF
   IF (workingObject1 /= workingObject2) THEN
    CharStringsAreEqual1 = .FALSE.
    RETURN
   END IF
  END DO

  RETURN
END FUNCTION CharStringsAreEqual1

FUNCTION IsKeyword1(charString,keyword)
  LOGICAL :: IsKeyword1
  TYPE(CharacterStringType), INTENT(IN) :: charString, &
                                           keyword

! define local variables

  INTEGER :: stringLength1,stringLength2
  CHARACTER(LEN=*), PARAMETER :: &
          location = 'FUNCTION IsKeyword1: '

  IsKeyword1 = .TRUE.

  stringLength1 = LEN_TRIM(charString%charString)
  stringLength2 = LEN_TRIM(keyword%charString)

  IF (stringLength1 /= stringLength2) THEN
   IsKeyword1 = .FALSE.
   RETURN
  END IF

  IsKeyword1 = CharStringsAreEqual1(charString1       = charString%charString, &
                                    charString2       = keyword%charString,    &
                                    stringCheckLength = stringLength1)

  RETURN
END FUNCTION IsKeyword1

FUNCTION IsComment1(charString,commentString)
  LOGICAL :: IsComment1
  TYPE(CharacterStringType), INTENT(IN) :: charString, &
                                           commentString

! define local variables

  INTEGER :: stringLength1
  CHARACTER(LEN=*), PARAMETER :: &
          location = 'FUNCTION IsComment1: '

  stringLength1 = LEN_TRIM(commentString%charString)

  IsComment1 = CharStringsAreEqual1(charString1       = charString%charString,    &
                                    charString2       = commentString%charString, &
                                    stringCheckLength = stringLength1)

  RETURN
END FUNCTION IsComment1

END MODULE ErrorInformationClass
