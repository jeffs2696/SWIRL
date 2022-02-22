!
! this module contains the mpiDataObject,
!   which has the MPI network data.
!
! this module also contains the MPI message send/recv
!   calls, to put the MessagePassingInterface completely
!   behind the MPIDataClass object
!
MODULE MPI2DataClassNEC

  USE MessagePassingInterface ! MessagePassingInterface.f90
  USE ErrorInformationClass   ! ErrorInformationClass.f90
  USE MPIStructLL             ! MPIStructLL.f90

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: mpiDataObjectType,                &
            structMessageObjectType,          &
            CreateObject,                     &
            DestroyObject,                    &
            IsMasterNode,                     & !MPIDataObject calls
            GetMPIGlobalComm,                 & !
            GetMPINodeComm,                   & !
            GetMPIStatusSize,                 & !
            GetMPIIntegerDef,                 & !
            GetMPIRealDef,                    & !
            GetMPICharacterDef,               & !
            GetMPILogicalDef,                 & !
            GetMPIDatatypeDef,                & !
            GetNumberOfNodes,                 & !
            GetMPIMaximumTag,                 & !
            GetNodeID,                        & !
            GetMPIVersion,                    & !
            GetMPISubversion,                 & !
            GetMPITimerIncrement,             & !
            GetMPITotalTimeSinceInit,         & !
            GetMPITimerValue,                 & !
            CheckMPIInitialization,           & !
            SynchronizeMPINodes,              & !
            AddDataToMPIStructureDefinition,  & ! Begin the MPI_STRUCT calls
            GetMPIStructureSize,              & 
            MPIStructureDefinitionIsComplete, &
            TestForMessageCompletion,         &
            WaitForMessageCompletion,         &
            TestForIncomingMessage,           &
            WaitForIncomingMessage,           &
            BroadcastMessage,                 &
            SendMessageToNode,                &
            RecvMessageFromNode,              &
            ChangeMessageSendingNodeID,       &
            ChangeMessageRecvingNodeID,       &
            ChangeMessageTag,                 &
            GetMessageSendingNodeID,          &
            GetMessageRecvingNodeID,          &
            GetMessageTag,                    &
            GetMessageLength,                 &
            MPI_ADDRESS_KIND,                 &
            MPI_INFO_NULL,                    &
            MPI_STATUS_SIZE

TYPE mpiDataObjectType
  PRIVATE
  LOGICAL :: initialized = .FALSE.

  LOGICAL :: mpiInitialized = .FALSE.,   &
             mpiMasterNode  = .FALSE.
  INTEGER :: mpiGlobalComm,    &
             mpiNodeComm,      &
             mpiStatusSize,    &
             numberOfMPINodes, &
             mpiNodeID,        &
             mpiVersion,       &
             mpiSubVersion,    &
             mpiLogicalDef,    &
             mpiCharacterDef,  &
             mpiInt4Def,       &
             mpiInt8Def,       &
             mpiReal4Def,      &
             mpiReal8Def

  INTEGER(KIND=MPI_ADDRESS_KIND) :: mpiMaximumTag

  DOUBLE PRECISION :: mpiTimerIncrement, &
                      mpiInitTime
END TYPE mpiDataObjectType

INTEGER, PARAMETER :: messageStatusDataSize = MPI_STATUS_SIZE

TYPE(CharacterStringType) :: charStringObject

CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE MPI2DataClass: '

INTEGER(KIND=int4Kind) :: int4Variable = 0

INTEGER, PARAMETER :: maxAddressDelta = HUGE(int4Variable)

TYPE structMessageObjectType
  PRIVATE
  LOGICAL :: initialized                 = .FALSE., &
             messageDefinitionIsComplete = .FALSE., &
             messageTransferIsComplete   = .TRUE.

  INTEGER :: messageTag,      &
             messageCount,    &
             messageDatatype, &
             messageTypeID,   &
             sendingNodeID,   &
             recvingNodeID,   &
             structArrayLength

  INTEGER :: messageRequestHandle
  INTEGER, DIMENSION(messageStatusDataSize) :: messageStatusData

! the message tag will be used as the 'initial data point' for
!  the message; all address deltas are keyed to this data location.

  INTEGER :: mpiStructDataType
  
  INTEGER, DIMENSION(:), ALLOCATABLE :: dataLengthArray, &
                                        dataTypeArray

  INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(:),  &
         ALLOCATABLE :: dataAddressDeltaArray

  TYPE(MPIStructLLType) :: mpiStructLL
  
END TYPE structMessageObjectType

INTERFACE CreateObject
  MODULE PROCEDURE CreateMPIDataTypeNEC
  MODULE PROCEDURE CreateStructMessageObject
END INTERFACE

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyMPIDataTypeNEC
  MODULE PROCEDURE DestroyStructMessageObject
END INTERFACE

INTERFACE IsMasterNode
  MODULE PROCEDURE IsMPIMasterProcessNEC
END INTERFACE

INTERFACE GetMPIGlobalComm
  MODULE PROCEDURE GetMPIGlobalCommNEC
END INTERFACE

INTERFACE GetMPINodeComm
  MODULE PROCEDURE GetMPINodeCommNEC
END INTERFACE

INTERFACE GetMPIStatusSize
  MODULE PROCEDURE GetMPIStatusSizeNEC
END INTERFACE

INTERFACE GetMPIIntegerDef
  MODULE PROCEDURE GetMPIInt4DefNEC
  MODULE PROCEDURE GetMPIInt8DefNEC
END INTERFACE

INTERFACE GetMPIRealDef
  MODULE PROCEDURE GetMPIReal4DefNEC
  MODULE PROCEDURE GetMPIReal8DefNEC
END INTERFACE

INTERFACE GetMPICharacterDef
  MODULE PROCEDURE GetMPICharacterDefNEC
END INTERFACE

INTERFACE GetMPILogicalDef
  MODULE PROCEDURE GetMPILogicalDefNEC
END INTERFACE

INTERFACE GetMPIDatatypeDef
  MODULE PROCEDURE GetMPIInt4DefNEC
  MODULE PROCEDURE GetMPIInt8DefNEC
  MODULE PROCEDURE GetMPIReal4DefNEC
  MODULE PROCEDURE GetMPIReal8DefNEC
  MODULE PROCEDURE GetMPICharacterDefNEC
  MODULE PROCEDURE GetMPILogicalDefNEC
END INTERFACE

INTERFACE GetNumberOfNodes
  MODULE PROCEDURE GetNumberOfNodesNEC
END INTERFACE

INTERFACE GetNodeID
  MODULE PROCEDURE GetNodeIDNEC
END INTERFACE

INTERFACE GetMPIVersion
  MODULE PROCEDURE GetMPIVersionNEC
END INTERFACE

INTERFACE GetMPISubversion
  MODULE PROCEDURE GetMPISubversionNEC
END INTERFACE

INTERFACE GetMPIMaximumTag
  MODULE PROCEDURE GetMPIMaximumTagNEC
END INTERFACE

INTERFACE GetMPITimerIncrement
  MODULE PROCEDURE GetMPITimerIncrementNEC
END INTERFACE

INTERFACE GetMPITimerValue
  MODULE PROCEDURE GetMPITimerValueNEC
END INTERFACE

INTERFACE GetMPITotalTimeSinceInit
  MODULE PROCEDURE GetMPITotalTimeSinceInitNEC
END INTERFACE

INTERFACE CheckMPIInitialization
  MODULE PROCEDURE CheckMPIInitializationNEC
END INTERFACE

INTERFACE SynchronizeMPINodes
  MODULE PROCEDURE SynchronizeMPINodesNEC
END INTERFACE

INTERFACE TestForMessageCompletion
  MODULE PROCEDURE TestForMessageCompletion1
END INTERFACE TestForMessageCompletion

INTERFACE WaitForMessageCompletion
  MODULE PROCEDURE WaitForMessageCompletion1
END INTERFACE WaitForMessageCompletion

INTERFACE TestForIncomingMessage
  MODULE PROCEDURE TestForIncomingMessage1
END INTERFACE TestForIncomingMessage

INTERFACE WaitForIncomingMessage
  MODULE PROCEDURE WaitForIncomingMessage1
END INTERFACE WaitForIncomingMessage

INTERFACE BroadcastMessage
  MODULE PROCEDURE BroadcastMessage1
END INTERFACE BroadcastMessage

INTERFACE SendMessageToNode
  MODULE PROCEDURE SendMessageToNode1
END INTERFACE SendMessageToNode

INTERFACE RecvMessageFromNode
  MODULE PROCEDURE RecvMessageFromNode1
END INTERFACE RecvMessageFromNode

INTERFACE AddDataToMPIStructureDefinition
  MODULE PROCEDURE AddChar0DDataToMPIStruct
  MODULE PROCEDURE AddChar1DDataToMPIStruct
  MODULE PROCEDURE AddChar2DDataToMPIStruct
  MODULE PROCEDURE AddChar3DDataToMPIStruct
  MODULE PROCEDURE AddChar4DDataToMPIStruct
  MODULE PROCEDURE AddChar5DDataToMPIStruct
  MODULE PROCEDURE AddInt40DDataToMPIStruct
  MODULE PROCEDURE AddInt41DDataToMPIStruct
  MODULE PROCEDURE AddInt42DDataToMPIStruct
  MODULE PROCEDURE AddInt43DDataToMPIStruct
  MODULE PROCEDURE AddInt44DDataToMPIStruct
  MODULE PROCEDURE AddInt45DDataToMPIStruct
  MODULE PROCEDURE AddInt80DDataToMPIStruct
  MODULE PROCEDURE AddInt81DDataToMPIStruct
  MODULE PROCEDURE AddInt82DDataToMPIStruct
  MODULE PROCEDURE AddInt83DDataToMPIStruct
  MODULE PROCEDURE AddInt84DDataToMPIStruct
  MODULE PROCEDURE AddInt85DDataToMPIStruct
  MODULE PROCEDURE AddLogical0DDataToMPIStruct
  MODULE PROCEDURE AddLogical1DDataToMPIStruct
  MODULE PROCEDURE AddLogical2DDataToMPIStruct
  MODULE PROCEDURE AddLogical3DDataToMPIStruct
  MODULE PROCEDURE AddLogical4DDataToMPIStruct
  MODULE PROCEDURE AddLogical5DDataToMPIStruct
  MODULE PROCEDURE AddReal40DDataToMPIStruct
  MODULE PROCEDURE AddReal41DDataToMPIStruct
  MODULE PROCEDURE AddReal42DDataToMPIStruct
  MODULE PROCEDURE AddReal43DDataToMPIStruct
  MODULE PROCEDURE AddReal44DDataToMPIStruct
  MODULE PROCEDURE AddReal45DDataToMPIStruct
  MODULE PROCEDURE AddReal80DDataToMPIStruct
  MODULE PROCEDURE AddReal81DDataToMPIStruct
  MODULE PROCEDURE AddReal82DDataToMPIStruct
  MODULE PROCEDURE AddReal83DDataToMPIStruct
  MODULE PROCEDURE AddReal84DDataToMPIStruct
  MODULE PROCEDURE AddReal85DDataToMPIStruct
END INTERFACE AddDataToMPIStructureDefinition

INTERFACE GetMPIStructureSize
  MODULE PROCEDURE GetMPIStructureSize1
END INTERFACE GetMPIStructureSize

INTERFACE MPIStructureDefinitionIsComplete
  MODULE PROCEDURE MPIStructureDefinitionIsComplete1
END INTERFACE MPIStructureDefinitionIsComplete

INTERFACE ChangeMessageSendingNodeID
  MODULE PROCEDURE ChangeMessageSendingNodeID1
END INTERFACE ChangeMessageSendingNodeID

INTERFACE ChangeMessageRecvingNodeID
  MODULE PROCEDURE ChangeMessageRecvingNodeID1
END INTERFACE ChangeMessageRecvingNodeID

INTERFACE ChangeMessageTag
  MODULE PROCEDURE ChangeMessageTag1
END INTERFACE ChangeMessageTag

INTERFACE GetMessageSendingNodeID
  MODULE PROCEDURE GetMessageSendingNodeID1
END INTERFACE GetMessageSendingNodeID

INTERFACE GetMessageRecvingNodeID
  MODULE PROCEDURE GetMessageRecvingNodeID1
END INTERFACE GetMessageRecvingNodeID

INTERFACE GetMessageTag
  MODULE PROCEDURE GetMessageTag1
END INTERFACE GetMessageTag

INTERFACE GetMessageLength
  MODULE PROCEDURE GetMessageLength1
END INTERFACE GetMessageLength

CONTAINS

!-------------MPIDataObject calls-----------------------

SUBROUTINE CreateMPIDataTypeNEC(object,          &
                                verbose,         &
                                enableChecking,  &
                                errorDetected,   &
                                errorInformation)

  TYPE(mpiDataObjectType),INTENT(INOUT) :: object
  LOGICAL, INTENT(IN) :: verbose
  LOGICAL, INTENT(IN) :: enableChecking
  LOGICAL, INTENT(OUT) :: errorDetected
  CHARACTER(LEN=*), INTENT(OUT) :: errorInformation

! local variables

  INTEGER :: errorTest ! returns MPI call errors
  LOGICAL :: mpiAttributeFound = .FALSE., &
             nodesAreSynchronized

  CHARACTER(LEN=*), PARAMETER :: &
    location = 'SUBROUTINE CreateMPIDataTypeNEC: '

! happily assume that there won't be any error

  errorDetected = .FALSE.
  errorInformation = location//'no error detected'

! check for previous initialization

  IF (enableChecking) THEN
   IF (object%initialized) THEN
    errorDetected = .TRUE.
    errorInformation = location//'object already initialized'
    RETURN
   END IF
  END IF

  object%initialized = .TRUE.

  IF (enableChecking) THEN

   CALL HTMPI_INITIALIZED(object%mpiInitialized, & ! FLAG/LOGICAL
                          errorTest)               ! IERROR/INTEGER

   IF (errorTest /= 0) THEN
    errorDetected = .TRUE.
    errorInformation = location//'MPI_INITIALIZED call 1'
    RETURN
   END IF
   IF (object%mpiInitialized) THEN
    errorDetected = .TRUE.
    errorInformation = location//'MPI is already running'
    RETURN
   END IF
  END IF

  CALL HTMPI_INIT(errorTest)    ! IERROR/INTEGER

  IF (errorTest /= 0) THEN
   errorDetected = .TRUE.
   errorInformation = location//'MPI_INIT call'
   RETURN
  END IF

  object%mpiInitialized = .TRUE.

! try again

  IF (enableChecking) THEN

   CALL HTMPI_INITIALIZED(object%mpiInitialized, & ! FLAG/LOGICAL
                          errorTest)               ! IERROR/INTEGER

   IF (errorTest /= 0) THEN
    errorDetected = .TRUE.
    errorInformation = location//'MPI_INITIALIZED call 2'
    RETURN
   END IF

   IF (.NOT. object%mpiInitialized) THEN  ! error
    errorDetected = .TRUE.
    errorInformation = location//'failed to initialize MPI'
    RETURN
   END IF

  END IF

! we're initialized at this point; now fill in the data

  object%mpiGlobalComm = MPI_COMM_WORLD
  object%mpiNodeComm   = MPI_COMM_SELF
  object%mpiStatusSize = MPI_STATUS_SIZE

! how many MPI nodes?

  CALL HTMPI_COMM_SIZE(object%mpiGlobalComm,    & ! COMM/INTEGER
                       object%numberOfMPINodes, & ! SIZE/INTEGER
                       errorTest)                 ! IERROR/INTEGER

  IF (errorTest /= 0) THEN
   errorDetected = .TRUE.
   errorInformation = location//'MPI_COMM_SIZE call'
   RETURN
  END IF

! who am I?

  CALL HTMPI_COMM_RANK(object%mpiGlobalComm, & ! COMM/INTEGER
                       object%mpiNodeID,     & ! RANK/INTEGER
                       errorTest)              ! IERROR/INTEGER

  IF (errorTest /= 0) THEN
   errorDetected = .TRUE.
   errorInformation = location//'MPI_COMM_RANK call'
   RETURN
  END IF

! am I the master node?

  IF (object%mpiNodeID /= 0) THEN
   object%mpiMasterNode = .FALSE.
  ELSE
   object%mpiMasterNode = .TRUE.
  END IF

  CALL HTMPI_GET_VERSION(object%mpiVersion,    & ! VERSION/INTEGER
                         object%mpiSubVersion, & ! SUBVERSION/INTEGER
                         errorTest)              ! IERROR/INTEGER

  IF (errorTest /= 0) THEN
   errorDetected = .TRUE.
   errorInformation = location//'MPI_GET_VERSION call'
   RETURN
  END IF

! now set datatypes (new INTEGER and REAL datatypes)

  object%mpiLogicalDef   = MPI_LOGICAL
  object%mpiCharacterDef = MPI_CHARACTER

  object%mpiInt4Def = MPI_INTEGER4
  object%mpiInt8Def = MPI_INTEGER8

  object%mpiReal4Def = MPI_REAL4
  object%mpiReal8Def = MPI_REAL8

  object%mpiMaximumTag = -1_MPI_ADDRESS_KIND

  CALL HTMPI_COMM_GET_ATTR(object%mpiGlobalComm, & ! COMM/INTEGER
                           MPI_TAG_UB,           & ! COMM_KEYVAL/INTEGER
                           object%mpiMaximumTag, & ! ATTRIBUTE_VAL/INTEGER
                           mpiAttributeFound,    & ! FLAG/LOGICAL
                           errorTest)              ! IERROR/INTEGER

  IF (.NOT. mpiAttributeFound) THEN
   errorDetected = .TRUE.
   errorInformation = location &
      //' problem in MPI_COMM_GET_ATTR call:  no attribute value returned'
   RETURN
  END IF

  IF (errorTest /= 0) THEN
   errorDetected = .TRUE.
   errorInformation = location//'MPI_COMM_GET_ATTR call'
   RETURN
  END IF

  object%mpiInitTime       = HTMPI_WTIME() 
  object%mpiTimerIncrement = HTMPI_WTICK() 

  IF (verbose .AND. object%mpiMasterNode) &
    WRITE(6,101) object%initialized,      &
                 object%mpiInitialized,   &
                 object%mpiMasterNode,    &
                 object%numberOfMPINodes, &
                 object%mpiNodeID,        &
                 object%mpiVersion,       &
                 object%mpiSubVersion,    &
                 object%mpiLogicalDef,    &
                 object%mpiCharacterDef,  &
                 object%mpiInt4Def,       &! 
                 object%mpiInt8Def,       &
                 object%mpiReal4Def,      &
                 object%mpiReal8Def,      &
                 object%mpiGlobalComm,    &
                 object%mpiMaximumTag,    &
                 object%mpiStatusSize,    &
                 object%mpiTimerIncrement

101 FORMAT(1x,'MPIDataType Created:       '      ,/, &
           3x,' object%initialized:       ',4x,l5,/, &
           3x,' object%mpiInitialized:    ',4x,l5,/, &
           3x,' object%mpiMasterNode:     ',4x,l5,/, &
           3x,' object%numberOfMPINodes:  ',4x,i15,/, &
           3x,' object%mpiNodeID:         ',4x,i15,/, &
           3x,' object%mpiVersion:        ',4x,i15,/, &
           3x,' object%mpiSubVersion:     ',4x,i15,/, &
           3x,' object%mpiLogicalDef:     ',4x,i15,/, &
           3x,' object%mpiCharacterDef:   ',4x,i15,/, &
           3x,' object%mpiInt4Def:        ',4x,i15,/, & !  
           3x,' object%mpiInt8Def:        ',4x,i15,/, &
           3x,' object%mpiReal4Def:       ',4x,i15,/, &
           3x,' object%mpiReal8Def:       ',4x,i15,/, &
           3x,' object%mpiGlobalComm:     ',4x,i15,/, &
           3x,' object%mpiMaximumTag:     ',4x,i15,/, &
           3x,' object%mpiStatusSize:     ',4x,i15,/,  &
           3x,' object%mpiTimerIncrement =',4x,f20.13,/)

! synchronize

  nodesAreSynchronized = SynchronizeMPINodes(mpiData = object)

  RETURN
  IF (nodesAreSynchronized) CONTINUE
END SUBROUTINE CreateMPIDataTypeNEC

SUBROUTINE DestroyMPIDataTypeNEC(object,         &
                                 verbose,        &
                                 enableChecking, &
                                 errorDetected,  &
                                 errorInformation)

  TYPE(mpiDataObjectType), INTENT(IN) :: object
  LOGICAL, INTENT(IN) :: verbose
  LOGICAL, INTENT(IN) :: enableChecking
  LOGICAL, INTENT(OUT) :: errorDetected
  CHARACTER(LEN=*), INTENT(OUT) :: errorInformation

! define local variables

  CHARACTER(LEN=*), PARAMETER :: location = 'SUBROUTINE DestroyMPIDataTypeNEC: '
  INTEGER :: errorTest,nodeID
  LOGICAL :: mpiInitialized
! LOGICAL :: mpiFinalized

! happily assume that there won't be any error

  errorDetected = .FALSE.
  errorInformation = location//'no error detected'

! check if MPI is initialized

  IF (enableChecking) THEN
   CALL HTMPI_INITIALIZED(mpiInitialized, & ! FLAG/LOGICAL
                          errorTest)        ! IERROR/INTEGER

   IF (errorTest /= 0) THEN
    errorDetected = .TRUE.
    errorInformation = location//' MPI_INITIALIZED call failed'
    RETURN
   END IF 

   IF (.NOT. mpiInitialized) THEN
    errorDetected = .TRUE.
    errorInformation = location//' MPI has not been initialized'
    RETURN 
   END IF
  END IF

  nodeID = object%mpiNodeID

! synchronize here

  CALL HTMPI_BARRIER(object%mpiGlobalComm, & ! COMM/INTEGER
                     errorTest)              ! IERROR/INTEGER

  IF (errorTest /= 0) THEN
   errorDetected = .TRUE.
   errorInformation = location//' MPI_BARRIER call failed'
   RETURN
  END IF 

  CALL HTMPI_FINALIZE(errorTest) ! IERROR/INTEGER

! nothing happens after this

! IF (object%mpiMasterNode) THEN

!  IF (errorTest /= 0) THEN
!   errorDetected = .TRUE.
!   errorInformation = location//' MPI_FINALIZE call failed'
!   RETURN
!  END IF 

! check again that MPI has been shut down

!  IF (enableChecking) THEN
!   IF (verbose) WRITE(6,*) nodeID,' shutting down...'

!   CALL HTMPI_FINALIZED(mpiFinalized, & ! FLAG/LOGICAL
!                      errorTest)      ! IERROR/INTEGER
!
!   IF (errorTest /= 0) THEN
!    errorDetected = .TRUE.
!    errorInformation = location//' MPI_FINALIZED call failed'
!    RETURN
!   END IF 

!   IF (.NOT. mpiFinalized) THEN
!    errorDetected = .TRUE.
!    errorInformation = location//' MPI has not been finalized'
!    RETURN
!   END IF
!  END IF ! checking?

!  IF (verbose) WRITE(6,*) nodeID,' MPI shutdown complete.'

! END IF ! masterNode?

  RETURN
  IF (verbose) CONTINUE
END SUBROUTINE DestroyMPIDataTypeNEC

LOGICAL FUNCTION IsMPIMasterProcessNEC(object)
  TYPE(mpiDataObjectType), INTENT(IN) :: object

  IsMPIMasterProcessNEC = .FALSE.

  IF (.NOT. object%initialized) RETURN
  IF (.NOT. object%mpiInitialized) RETURN

  IsMPIMasterProcessNEC = object%mpiMasterNode

  RETURN
END FUNCTION IsMPIMasterProcessNEC

INTEGER FUNCTION GetNumberOfNodesNEC(object)
  TYPE(mpiDataObjectType), INTENT(IN) :: object

  GetNumberOfNodesNEC = 0

  IF (.NOT. object%initialized) RETURN
  IF (.NOT. object%mpiInitialized) RETURN

  GetNumberOfNodesNEC = object%numberOfMPINodes

  RETURN
END FUNCTION GetNumberOfNodesNEC

INTEGER FUNCTION GetNodeIDNEC(object)
  TYPE(mpiDataObjectType), INTENT(IN) :: object

  GetNodeIDNEC = -1

  IF (.NOT. object%initialized) RETURN
  IF (.NOT. object%mpiInitialized) RETURN

  GetNodeIDNEC = object%mpiNodeID

  RETURN
END FUNCTION GetNodeIDNEC

INTEGER FUNCTION GetMPIVersionNEC(object)
  TYPE(mpiDataObjectType), INTENT(IN) :: object

  GetMPIVersionNEC = -1

  IF (.NOT. object%initialized) RETURN
  IF (.NOT. object%mpiInitialized) RETURN

  GetMPIVersionNEC = object%mpiVersion

  RETURN
END FUNCTION GetMPIVersionNEC

INTEGER FUNCTION GetMPISubVersionNEC(object)
  TYPE(mpiDataObjectType), INTENT(IN) :: object

  GetMPISubVersionNEC = -1

  IF (.NOT. object%initialized) RETURN
  IF (.NOT. object%mpiInitialized) RETURN

  GetMPISubVersionNEC = object%mpiSubVersion

  RETURN
END FUNCTION GetMPISubVersionNEC

INTEGER(KIND=MPI_ADDRESS_KIND) FUNCTION GetMPIMaximumTagNEC(object)
  TYPE(mpiDataObjectType), INTENT(IN) :: object

  GetMPIMaximumTagNEC = -1_MPI_ADDRESS_KIND

  IF (.NOT. object%initialized) RETURN
  IF (.NOT. object%mpiInitialized) RETURN

  GetMPIMaximumTagNEC = object%mpiMaximumTag

  RETURN
END FUNCTION GetMPIMaximumTagNEC

LOGICAL FUNCTION CheckMPIInitializationNEC(object, &
                                    errorDetected, &
                                 errorInformation)

  TYPE(mpiDataObjectType), INTENT(IN) :: object
  LOGICAL, INTENT(OUT) :: errorDetected
  CHARACTER(LEN=*), INTENT(OUT) :: errorInformation

! local variable def

  CHARACTER(LEN=*), PARAMETER :: location = 'FUNCTION CheckMPIInitializationNEC: '

  errorDetected = .FALSE.
  errorInformation = location//'No error detected'

  CheckMPIInitializationNEC = .FALSE.

  IF (.NOT. object%initialized) THEN
   errorDetected = .TRUE.
   errorInformation = location//'object not initialized'
   RETURN
  END IF

  CheckMPIInitializationNEC = object%mpiInitialized

  RETURN
END FUNCTION CheckMPIInitializationNEC

LOGICAL FUNCTION SynchronizeMPINodesNEC(mpiData)

  TYPE(mpiDataObjectType), INTENT(IN) :: mpiData

! local variables

  INTEGER :: errorTest

  SynchronizeMPINodesNEC = .FALSE.

  CALL HTMPI_BARRIER(mpiData%mpiGlobalComm, & ! COMM/INTEGER
                     errorTest)               ! IERROR/INTEGER

  IF (errorTest == MPI_SUCCESS) &
   SynchronizeMPINodesNEC = .TRUE.

  RETURN
END FUNCTION SynchronizeMPINodesNEC

INTEGER FUNCTION GetMPIStatusSizeNEC(mpiData)

  TYPE(mpiDataObjectType), INTENT(IN) :: mpiData

  GetMPIStatusSizeNEC = mpiData%mpiStatusSize

  RETURN
END FUNCTION GetMPIStatusSizeNEC

INTEGER FUNCTION GetMPIGlobalCommNEC(mpiData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData

  GetMPIGlobalCommNEC = mpiData%mpiGlobalComm

  RETURN
END FUNCTION GetMPIGlobalCommNEC

INTEGER FUNCTION GetMPINodeCommNEC(mpiData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData

  GetMPINodeCommNEC = mpiData%mpiNodeComm

  RETURN
END FUNCTION GetMPINodeCommNEC

INTEGER FUNCTION GetMPIInt4DefNEC(mpiData,testData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData
  INTEGER(KIND=int4Kind), INTENT(IN) :: testData

  GetMPIInt4DefNEC = mpiData%mpiInt4Def

  RETURN
  IF (testData /= 4_int4Kind) CONTINUE ! fool compiler

  RETURN
END FUNCTION GetMPIInt4DefNEC

INTEGER FUNCTION GetMPIInt8DefNEC(mpiData,testData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData
  INTEGER(KIND=int8Kind), INTENT(IN) :: testData

  GetMPIInt8DefNEC = mpiData%mpiInt8Def

  RETURN
  IF (testData /= 4_int8Kind) CONTINUE ! fool compiler
  IF (mpiData%mpiInt4Def /= 4_int4Kind) CONTINUE ! fool compiler

  RETURN
END FUNCTION GetMPIInt8DefNEC

INTEGER FUNCTION GetMPIReal4DefNEC(mpiData,testData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData
  REAL(KIND=real4Kind) ,INTENT(IN) :: testData

  GetMPIReal4DefNEC = mpiData%mpiReal4Def

  RETURN
  IF (testData /= 4.0_real4Kind) CONTINUE ! fool compiler
  RETURN
END FUNCTION GetMPIReal4DefNEC

INTEGER FUNCTION GetMPIReal8DefNEC(mpiData,testData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData
  REAL(KIND=real8Kind) ,INTENT(IN) :: testData

  GetMPIReal8DefNEC = mpiData%mpiReal8Def

  RETURN
  IF (testData /= 4.0_real8Kind) CONTINUE ! fool compiler
  RETURN
END FUNCTION GetMPIReal8DefNEC

INTEGER FUNCTION GetMPICharacterDefNEC(mpiData,testData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData
  CHARACTER, INTENT(IN) :: testData

  GetMPICharacterDefNEC = mpiData%mpiCharacterDef

  RETURN
  IF (testData /= 'A') CONTINUE ! fool compiler
  RETURN
END FUNCTION GetMPICharacterDefNEC

INTEGER FUNCTION GetMPILogicalDefNEC(mpiData,testData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData
  LOGICAL, INTENT(IN) :: testData

  GetMPILogicalDefNEC = mpiData%mpiLogicalDef

  RETURN
  IF (testData) CONTINUE ! fool compiler
  RETURN
END FUNCTION GetMPILogicalDefNEC

DOUBLE PRECISION FUNCTION GetMPITimerIncrementNEC(mpiData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData

  GetMPITimerIncrementNEC = mpiData%mpiTimerIncrement

  RETURN
END FUNCTION GetMPITimerIncrementNEC

DOUBLE PRECISION FUNCTION GetMPITimerValueNEC(mpiData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData

  GetMPITimerValueNEC = HTMPI_WTIME()

  RETURN
  IF (mpiData%mpiLogicalDef < 0) CONTINUE
END FUNCTION GetMPITimerValueNEC

DOUBLE PRECISION FUNCTION GetMPITotalTimeSinceInitNEC(mpiData)

  TYPE(mpiDataObjectType),INTENT(IN) :: mpiData

  GetMPITotalTimeSinceInitNEC = HTMPI_WTIME() - mpiData%mpiInitTime

  RETURN
  IF (mpiData%mpiLogicalDef < 0) CONTINUE
END FUNCTION GetMPITotalTimeSinceInitNEC

!-----------------messageObject routines------------------------------

SUBROUTINE CreateStructMessageObject(object, &
                                     nodeMPIDataObject, &
                                     messageTag,        &
                                     messageTypeID,     &
                                     sendingNodeID,     &
                                     recvingNodeID,     &
                                     enableChecking,    &
                                     errorInfoObject)
                                  
  TYPE(structMessageObjectType), INTENT(INOUT) :: object
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  INTEGER, INTENT(IN) :: messageTag
  INTEGER, INTENT(IN) :: messageTypeID
  INTEGER, INTENT(IN) :: sendingNodeID
  INTEGER, INTENT(IN) :: recvingNodeID
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GenerateStructArrays'

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN
    
    WRITE(charStringObject%charString,'(a80)') errorInformation

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF
 
   IF (object%initialized) THEN
    charStringObject%charString = 'structMessageObject is already initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF
  ELSE
   CONTINUE ! feeling lucky!
  END IF

  object%messageTag        = messageTag
  object%messageCount      = 1
  object%messageTypeID     = messageTypeID
  object%sendingNodeID     = sendingNodeID
  object%recvingNodeID     = recvingNodeID
  object%structArrayLength = 0

  object%messageDefinitionIsComplete = .FALSE.
  object%messageTransferIsComplete   = .FALSE.

  CALL CreateObject(object          = object%mpiStructLL,   &
                    messageTypeID   = object%messageTypeID, &
                    maxAddressDelta = maxAddressDelta,      &
                    errorInfoObject = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  object%initialized = .TRUE.

  RETURN

101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CreateStructMessageObject

SUBROUTINE DestroyStructMessageObject(object,            &
                                      nodeMPIDataObject, &
                                      enableChecking,    &
                                      errorInfoObject)

  TYPE(structMessageObjectType), INTENT(INOUT) :: object
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! define local variables

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  INTEGER :: iError

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE DestroyStructMessageObject: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN
    
    WRITE(charStringObject%charString,'(a80)') errorInformation
    GO TO 99
   END IF

   IF (.NOT. object%initialized) THEN
    charStringObject%charString = 'structMessageObject is not initialized.'
    GO TO 99
   END IF

  END IF

  IF (object%messageDefinitionIsComplete) THEN

   IF (object%messageTransferIsComplete) THEN
    CONTINUE ! ok
   ELSE
    charStringObject%charString = 'structMessageObject has an incomplete message transfer.'
    GO TO 99
   END IF

   DEALLOCATE(object%dataLengthArray,       &
              object%dataAddressDeltaArray, &
              object%dataTypeArray,         &
              STAT = iError)

   IF (iError /= 0) THEN
 
     charStringObject%charString = 'DEALLOCATION error for structArrays.'
     GO TO 99
 
   END IF

   CALL HTMPI_TYPE_FREE(object%mpiStructDataType, & ! DATATYPE/INTEGER
                        iError)                     ! IERROR/INTEGER

   IF (iError /= 0) THEN
 
     charStringObject%charString = 'Error in MPI_TYPE_FREE'
     GO TO 99
 
   END IF

  ELSE ! the mpiStructLL may need to be destroyed

   CALL DestroyObject(object          = object%mpiStructLL,       &
                      errorInfoObject = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   END IF
  END IF

  object%initialized                 = .FALSE.
  object%messageDefinitionIsComplete = .FALSE.

  RETURN

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE DestroyStructMessageObject

SUBROUTINE TestForMessageCompletion1(nodeMPIDataObject,         &
                                     messageObject,             &
                                     messageTransferIsComplete, &
                                     enableChecking,            &
                                     errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  LOGICAL, INTENT(OUT) :: messageTransferIsComplete
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  INTEGER :: mpiIerror
  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE TestForMessageCompletion1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN
 
    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99

   END IF
 
   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF

   IF (.NOT. messageObject%messageDefinitionIsComplete) THEN
 
     charStringObject%charString = 'messageObject definition is not completed.'
     GO TO  99
 
   END IF
  END IF

! messageTransferIsComplete = .FALSE.

  IF (messageObject%messageTransferIsComplete) THEN

   messageTransferIsComplete = .TRUE.

  ELSE

   CALL HTMPI_TEST(messageObject%messageRequestHandle,      & ! REQUEST/INTEGER
                   messageObject%messageTransferIsComplete, & ! FLAG/LOGICAL
                   messageObject%messageStatusData,         & ! STATUS/INTEGER ARRAY
                   mpiIerror)                                 ! IERROR/INTEGER

   IF (mpiIerror /= 0) THEN
    charStringObject%charString = 'Error in MPI_TEST call.'
    GO TO  99
   END IF

   messageTransferIsComplete = messageObject%messageTransferIsComplete

! new: call an MPI_REQUEST_FREE if TEST result is .TRUE.

!  IF (messageObject%messageTransferIsComplete) THEN
!   CALL HTMPI_REQUEST_FREE(messageObject%messageRequestHandle,      & ! REQUEST/INTEGER
!                           mpiIerror)                                 ! IERROR/INTEGER

!   IF (mpiIerror /= 0) THEN
!    charStringObject%charString = 'Error in MPI_REQUEST_FREE call.'
!    GO TO  99
!   END IF
!  END IF
  
  END IF

  RETURN 

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE TestForMessageCompletion1

SUBROUTINE WaitForMessageCompletion1(nodeMPIDataObject,    &
                                     messageObject,        &
                                     maxWaitTimeInSeconds, &
                                     enableChecking,       &
                                     errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  DOUBLE PRECISION, INTENT(IN) :: maxWaitTimeInSeconds
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE WaitForMessageCompletion1: '
  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  INTEGER :: myMPINodeID
  DOUBLE PRECISION :: initialTime, &
                      currentWaitTime

  errorDetected = .FALSE.
  errorInformation = location//'No error detected'

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN
 
    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99

   END IF
 
   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
   IF (.NOT. messageObject%messageDefinitionIsComplete) THEN
 
     charStringObject%charString = 'messageObject definition is not completed.'
     GO TO  99
 
   END IF
  END IF

  IF (messageObject%messageTransferIsComplete) THEN
   RETURN
  END IF

  initialTime = GetMPITimerValue(nodeMPIDataObject)

50 CONTINUE

    currentWaitTime = GetMPITimerValue(nodeMPIDataObject) - initialTime

    IF (currentWaitTime > maxWaitTimeInSeconds) THEN
     myMPINodeID = GetNodeID(object = nodeMPIDataObject)

     WRITE(charStringObject%charString,'(a24,f13.3,a9)') &
      'ERROR: Node has waited: ',           &
      currentWaitTime,                      &
      ' seconds.'

     GO TO 99

    ELSE
     CONTINUE
    END IF

    CALL TestForMessageCompletion(nodeMPIDataObject         = nodeMPIDataObject,                       &
                                  messageObject             = messageObject,                           &
                                  messageTransferIsComplete = messageObject%messageTransferIsComplete, &
                                  enableChecking            = enableChecking,                          &
                                  errorInfoObject           = errorInfoObject)

    IF (CheckForLocalError(errorInfoObject)) THEN
     GO TO 101
    END IF


    IF (.NOT. messageObject%messageTransferIsComplete) THEN
     GO TO 50 ! try again
    ELSE
     CONTINUE
    END IF

!  WRITE(6,*) GetNodeID(object = nodeMPIDataObject), &
!             'current wait time = ',currentWaitTime, &
!             'in WaitForMessageCompletion'

  RETURN 

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

101 CONTINUE
  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE WaitForMessageCompletion1

SUBROUTINE WaitForIncomingMessage1(nodeMPIDataObject,    &
                                   messageObject,        &
                                   maxWaitTimeInSeconds, &
                                   enableChecking,       &
                                   errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  DOUBLE PRECISION, INTENT(IN) :: maxWaitTimeInSeconds
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  INTEGER :: myMPINodeID
  LOGICAL :: incomingMessageDetected
  DOUBLE PRECISION :: initialTime, &
                      currentWaitTime

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE WaitForIncomingMessage1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN
 
    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99

   END IF
 
   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
   IF (.NOT. messageObject%messageDefinitionIsComplete) THEN
 
     charStringObject%charString = 'messageObject definition is not completed.'
     GO TO  99
 
   END IF

   myMPINodeID = GetNodeID(object = nodeMPIDataObject)

   IF (messageObject%recvingNodeID /= myMPINodeID) THEN
     charStringObject%charString = 'inconsistent message definition.'
     GO TO  99
   END IF

   IF (.NOT. messageObject%messageTransferIsComplete) THEN

     charStringObject%charString = 'messageObject is already in use.'
     GO TO  99

   END IF
  END IF

  IF (messageObject%messageTransferIsComplete) THEN
   RETURN
  END IF

  initialTime = GetMPITimerValue(nodeMPIDataObject)

50 CONTINUE

    currentWaitTime = GetMPITimerValue(nodeMPIDataObject) - initialTime

    IF (currentWaitTime > maxWaitTimeInSeconds) THEN
     myMPINodeID = GetNodeID(object = nodeMPIDataObject)

     WRITE(charStringObject%charString,'(a24,f13.3,a9)') &
      'ERROR: Node has waited: ',           &
      currentWaitTime,                      &
      ' seconds.'

     GO TO 99

    ELSE
     CONTINUE
    END IF

    CALL TestForIncomingMessage(nodeMPIDataObject       = nodeMPIDataObject,       &
                                messageObject           = messageObject,           &
                                incomingMessageDetected = incomingMessageDetected, &
                                enableChecking          = enableChecking,          &
                                errorInfoObject         = errorInfoObject)

    IF (CheckForLocalError(errorInfoObject)) THEN
     GO TO 101
    END IF

    IF (.NOT. incomingMessageDetected) THEN
     GO TO 50 ! try again
    ELSE
     CONTINUE
    END IF

!  WRITE(6,*) GetNodeID(object = nodeMPIDataObject), &
!             'current wait time = ',currentWaitTime, &
!             'in WaitForIncomingMessage'

  RETURN

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

101 CONTINUE
  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE WaitForIncomingMessage1

SUBROUTINE TestForIncomingMessage1(nodeMPIDataObject,       &
                                   messageObject,           &
                                   incomingMessageDetected, &
                                   enableChecking,          &
                                   errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  LOGICAL, INTENT(OUT) :: incomingMessageDetected
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  INTEGER :: mpiIerror,     &
             mpiGlobalComm, &
             myMPINodeID

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE TestForIncomingMessage1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
   IF (.NOT. messageObject%messageDefinitionIsComplete) THEN
 
     charStringObject%charString = 'messageObject definition is not completed.'
     GO TO  99
 
   END IF

   myMPINodeID = GetNodeID(object = nodeMPIDataObject)


   IF (messageObject%recvingNodeID /= myMPINodeID) THEN
     charStringObject%charString = 'inconsistent message definition.'
     GO TO  99
   END IF
 
   IF (.NOT. messageObject%messageTransferIsComplete) THEN
 
    charStringObject%charString = 'messageObject is already in use.'
    GO TO  99
 
   END IF
  END IF

  mpiGlobalComm = GetMPIGlobalComm(mpiData = nodeMPIDataObject)

  CALL HTMPI_IPROBE(messageObject%sendingNodeID,      & ! SOURCE/INTEGER
                    messageObject%MessageTag,         & ! TAG/INTEGER
                    mpiGlobalComm,                    & ! COMM/INTEGER
                    incomingMessageDetected,          & ! FLAG/LOGICAL
                    messageObject%messageStatusData,  & ! STATUS/INTEGER ARRAY
                    mpiIerror)                          ! IERROR/INTEGER

  IF (mpiIerror /= 0) THEN
    charStringObject%charString = 'Error in MPI_IPROBE call.'
    GO TO  99
  END IF

  RETURN

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE TestForIncomingMessage1

SUBROUTINE BroadcastMessage1(nodeMPIDataObject,    &
                             messageObject,        &
                             enableChecking,       &
                             errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  INTEGER :: mpiIerror,             &
             mpiGlobalComm

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE BroadcastMessage1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
   IF (.NOT. messageObject%messageDefinitionIsComplete) THEN
 
     charStringObject%charString = 'messageObject definition is not completed.'
     GO TO  99
 
   END IF
 
   IF (.NOT. messageObject%messageTransferIsComplete) THEN
 
    charStringObject%charString = 'messageObject is already in use.'
    GO TO  99
 
   END IF
  END IF

  mpiGlobalComm = GetMPIGlobalComm(mpiData = nodeMPIDataObject)

! for safety's sake, do a global error check so that the code
!  doesn't hang here if a node already has an error.

  IF (CheckForGlobalError(object = errorInfoObject) ) GO TO 101

  CALL HTMPI_BCAST(messageObject%messageTypeID,        & ! BUF/MSGDATATYPE
                   messageObject%messageCount,         & ! COUNT/INTEGER
                   messageObject%mpiStructDataType,    & ! DATATYPE/INTEGER
                   messageObject%sendingNodeID,        & ! ROOT/INTEGER
                   mpiGlobalComm,                      & ! COMM/INTEGER
                   mpiIerror)                            ! IERROR/INTEGER

  IF (mpiIerror /= 0) THEN
    charStringObject%charString = 'Error in MPI_BCAST call (MPI_STRUCT).'
    GO TO  99
  END IF

  messageObject%messageTransferIsComplete = .TRUE.

  RETURN

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE BroadcastMessage1

SUBROUTINE SendMessageToNode1(nodeMPIDataObject,    &
                              messageObject,        &
                              enableChecking,       &
                              errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  INTEGER :: mpiIerror,             &
             mpiGlobalComm,         &
             myMPINodeID

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE SendMessageToNode1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
   IF (.NOT. messageObject%messageDefinitionIsComplete) THEN
 
     charStringObject%charString = 'messageObject definition is not completed.'
     GO TO  99
 
   END IF

   myMPINodeID = GetNodeID(object = nodeMPIDataObject)


   IF (messageObject%sendingNodeID /= myMPINodeID) THEN
     charStringObject%charString = 'inconsistent message definition.'
     GO TO  99
   END IF
 
   IF (.NOT. messageObject%messageTransferIsComplete) THEN
 
    charStringObject%charString = 'messageObject is already in use.'
    GO TO  99
 
   END IF

  END IF

  mpiGlobalComm = GetMPIGlobalComm(mpiData = nodeMPIDataObject)

  CALL HTMPI_ISEND(messageObject%messageTypeID,        & ! BUF/MSGDATATYPE
                   messageObject%messageCount,         & ! COUNT/INTEGER
                   messageObject%mpiStructDataType,    & ! DATATYPE/INTEGER
                   messageObject%recvingNodeID,        & ! DEST/INTEGER
                   messageObject%messageTag,           & ! TAG/INTEGER
                   mpiGlobalComm,                      & ! COMM/INTEGER
                   messageObject%messageRequestHandle, & ! REQUEST/INTEGER
                   mpiIerror)                            ! IERROR/INTEGER

  IF (mpiIerror /= 0) THEN
    charStringObject%charString = 'Error in MPI_ISEND call (MPI_STRUCT).'
    GO TO  99
  END IF

  messageObject%messageTransferIsComplete = .FALSE.

  RETURN

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE SendMessageToNode1

SUBROUTINE RecvMessageFromNode1(nodeMPIDataObject,    &
                                messageObject,        &
                                enableChecking,       &
                                errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  INTEGER :: mpiIerror,             &
             mpiGlobalComm,         &
             myMPINodeID

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE RecvMessageFromNode1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
   IF (.NOT. messageObject%messageDefinitionIsComplete) THEN
 
     charStringObject%charString = 'messageObject definition is not completed.'
     GO TO  99
 
   END IF

   myMPINodeID = GetNodeID(object = nodeMPIDataObject)


   IF (messageObject%recvingNodeID /= myMPINodeID) THEN
     charStringObject%charString = 'inconsistent message definition.'
     GO TO  99
   END IF
 
   IF (.NOT. messageObject%messageTransferIsComplete) THEN
 
    charStringObject%charString = 'messageObject is already in use.'
    GO TO  99
 
   END IF
  END IF

  mpiGlobalComm = GetMPIGlobalComm(mpiData = nodeMPIDataObject)

  CALL HTMPI_IRECV(messageObject%messageTypeID,        & ! BUF/MSGDATATYPE
                   messageObject%messageCount,         & ! COUNT/INTEGER
                   messageObject%mpiStructDataType,    & ! DATATYPE/INTEGER
                   messageObject%sendingNodeID,        & ! SOURCE/INTEGER
                   messageObject%messageTag,           & ! TAG/INTEGER
                   mpiGlobalComm,                      & ! COMM/INTEGER
                   messageObject%messageRequestHandle, & ! REQUEST/INTEGER
                   mpiIerror)                            ! IERROR/INTEGER

  IF (mpiIerror /= 0) THEN
   charStringObject%charString = 'Error in MPI_IRECV call (MPI_STRUCT).'
   GO TO  99
  END IF

  messageObject%messageTransferIsComplete = .FALSE.

  RETURN

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE RecvMessageFromNode1

!-----------messageObject DataStructure definition  routines-----------

include 'AddCharacterDataToStruct.f90'
include 'AddInt4DataToStruct.f90'
include 'AddInt8DataToStruct.f90'
include 'AddLogicalDataToStruct.f90'
include 'AddReal4DataToStruct.f90'
include 'AddReal8DataToStruct.f90'

!----------messageObject DataStructure completion routine-----------

SUBROUTINE MPIStructureDefinitionIsComplete1(nodeMPIDataObject,     &
                                             messageObject,         &
                                             enableChecking,        &
                                             errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  INTEGER :: iError

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'MPIStructureDefinitionIsComplete1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF

   IF (messageObject%messageDefinitionIsComplete) THEN
 
     charStringObject%charString = 'messageObject is already defined.'
     GO TO  99
 
   END IF
  END IF

! at this point, we have a linked list with the data structure
!  inside.

  CALL MPIStructDefinitionIsComplete(object            = messageObject%mpiStructLL,       &
                                     structArrayLength = messageObject%structArrayLength, &
                                     errorInfoObject   = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  ALLOCATE(messageObject%dataLengthArray(messageObject%structArrayLength),       &
           messageObject%dataAddressDeltaArray(messageObject%structArrayLength), &
           messageObject%dataTypeArray(messageObject%structArrayLength),         &
           STAT = iError)

  IF (iError /= 0) THEN
 
    charStringObject%charString = 'ALLOCATION error for structArrays.'
    GO TO  99
 
  END IF

  CALL GetMPIStructArrays(object                = messageObject%mpiStructLL,           &
                          structArrayLength     = messageObject%structArrayLength,     &
                          dataLengthArray       = messageObject%dataLengthArray,       &
                          dataAddressDeltaArray = messageObject%dataAddressDeltaArray, &
                          dataTypeArray         = messageObject%dataTypeArray,         &
                          errorInfoObject       = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

! and get rid of the LL

  CALL DestroyObject(object          = messageObject%mpiStructLL,       &
                     errorInfoObject = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

! and now actually commit the datatype

  CALL HTMPI_TYPE_CREATE_STRUCT(messageObject%structArrayLength,     &
                                messageObject%dataLengthArray,       &
                                messageObject%dataAddressDeltaArray, &
                                messageObject%dataTypeArray,         &
                                messageObject%mpiStructDataType,     &
                                iError)

  IF (iError /= 0) THEN
 
    charStringObject%charString = 'Error in MPI_TYPE_CREATE_STRUCT'
    GO TO  99
 
  END IF

  CALL HTMPI_TYPE_COMMIT(messageObject%mpiStructDataType, &
                         iError)

  IF (iError /= 0) THEN
 
    charStringObject%charString = 'Error in MPI_TYPE_CREATE_STRUCT'
    GO TO  99
 
  END IF

! and we're done.

  messageObject%messageDefinitionIsComplete = .TRUE.
  messageObject%messageTransferIsComplete = .TRUE.

  RETURN

 99 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE MPIStructureDefinitionIsComplete1

!---------------get/change message parameters-----------------------

SUBROUTINE ChangeMessageSendingNodeID1(nodeMPIDataObject,     &
                                       messageObject,         &
                                       sendingNodeID,         &
                                       enableChecking,        &
                                       errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  INTEGER, INTENT(IN) :: sendingNodeID
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'ChangeMessageSendingNodeID1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
  END IF

  messageObject%sendingNodeID = sendingNodeID

  RETURN
  99 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE ChangeMessageSendingNodeID1

SUBROUTINE GetMessageSendingNodeID1(nodeMPIDataObject,     &
                                      messageObject,         &
                                      sendingNodeID,         &
                                      enableChecking,        &
                                      errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  INTEGER, INTENT(OUT) :: sendingNodeID
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'GetMessageSendingNodeID1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
  END IF

  sendingNodeID = messageObject%sendingNodeID

  RETURN
  99 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetMessageSendingNodeID1

SUBROUTINE ChangeMessageRecvingNodeID1(nodeMPIDataObject,     &
                                       messageObject,         &
                                       recvingNodeID,         &
                                       enableChecking,        &
                                       errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  INTEGER, INTENT(IN) :: recvingNodeID
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'ChangeMessageRecvingNodeID1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
  END IF

  messageObject%recvingNodeID = recvingNodeID

  RETURN
  99 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE ChangeMessageRecvingNodeID1

SUBROUTINE GetMessageRecvingNodeID1(nodeMPIDataObject,     &
                                      messageObject,         &
                                      recvingNodeID,         &
                                      enableChecking,        &
                                      errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  INTEGER, INTENT(OUT) :: recvingNodeID
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE GetMessageRecvingNodeID1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
  END IF

  recvingNodeID = messageObject%recvingNodeID

  RETURN
  99 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetMessageRecvingNodeID1

SUBROUTINE ChangeMessageTag1(nodeMPIDataObject,     &
                             messageObject,         &
                             messageTag,            &
                             enableChecking,        &
                             errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  INTEGER, INTENT(IN) :: messageTag
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE ChangeMessageTag1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
  END IF

  messageObject%messageTag = messageTag

  RETURN
  99 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE ChangeMessageTag1

SUBROUTINE GetMessageTag1(nodeMPIDataObject,     &
                          messageObject,         &
                          messageTag,            &
                          enableChecking,        &
                          errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  INTEGER, INTENT(OUT) :: messageTag
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE GetMessageTag1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
  END IF

  messageTag = messageObject%messageTag

  RETURN
  99 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetMessageTag1

SUBROUTINE GetMessageLength1(nodeMPIDataObject,     &
                             messageObject,         &
                             messageLength,         &
                             enableChecking,        &
                             errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  INTEGER, INTENT(OUT) :: messageLength
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE GetMessageLength1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
  END IF

  messageLength = messageObject%messageCount

  RETURN
  99 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetMessageLength1

SUBROUTINE GetMPIStructureSize1(nodeMPIDataObject,     &
                                messageObject,         &
                                structureSize,         &
                                enableChecking,        &
                                errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: messageObject
  INTEGER, INTENT(INOUT) :: structureSize
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  INTEGER :: mpiIerror
  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE GetMPIStructureSize1: '

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    GO TO  99
   END IF

   IF (.NOT. messageObject%initialized) THEN
 
     charStringObject%charString = 'messageObject is not initialized.'
     GO TO  99
 
   END IF
  END IF

  CALL HTMPI_TYPE_SIZE(messageObject%mpiStructDataType, & ! datatype
                       structureSize,                   & ! extent
                       mpiIerror)
     
  IF (mpiIerror /= 0) THEN
   charStringObject%charString = 'Error in MPI_TYPE_EXTENT call (MPI_STRUCT).'
   GO TO  99
  END IF

  RETURN
  99 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetMPIStructureSize1

END MODULE MPI2DataClassNEC
