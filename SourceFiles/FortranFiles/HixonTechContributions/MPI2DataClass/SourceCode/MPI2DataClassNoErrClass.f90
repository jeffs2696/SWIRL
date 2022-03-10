MODULE MPI2DataClassNoErrClass

! this module passes through the 
!  MPIDataClassNEC routines and
!  objects, and adds 'null'
!  MPI_xxxx routines in order
!  to keep any code that USEs
!  this class from bypassing
!  the class calls and using MPI
!  directly

  USE MPI2DataClassNEC

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: mpiDataObjectType,                &
            structMessageObjectType,          &
            CreateObject,                     &
            DestroyObject,                    &
            IsMasterNode,                     &
            GetMPIGlobalComm,                 &
            GetMPINodeComm,                   &
            GetMPIStatusSize,                 &
            GetMPIIntegerDef,                 &
            GetMPIRealDef,                    &
            GetMPICharacterDef,               &
            GetMPILogicalDef,                 &
            GetMPIDatatypeDef,                &
            GetNumberOfNodes,                 &
            GetMPIMaximumTag,                 &
            GetNodeID,                        &
            GetMPIVersion,                    &
            GetMPISubversion,                 &
            GetMPITimerIncrement,             &
            GetMPITimerValue,                 &
            GetMPITotalTimeSinceInit,         &
            CheckMPIInitialization,           &
            SynchronizeMPINodes,              &
            AddDataToMPIStructureDefinition,  &
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
            GetMessageLength
! 'null' MPI routines -- I don't think this actually works...
  PUBLIC :: MPI_ADDRESS_KIND,             &
            MPI_STATUS_SIZE,              &
            MPI_ABORT,                    &
            MPI_BARRIER,                  &
            MPI_BCAST,                    &
            MPI_COMM_GET_ATTR,            &
            MPI_COMM_RANK,                &
            MPI_COMM_SIZE,                &
            MPI_ERROR_STRING,             &
            MPI_FINALIZE,                 &
            MPI_FINALIZED,                &
            MPI_GET_COUNT,                &
            MPI_GET_VERSION,              &
            MPI_INFO_NULL,                &
            MPI_INIT,                     &
            MPI_INITIALIZED,              &
            MPI_IPROBE,                   &
            MPI_IRECV,                    &
            MPI_ISEND,                    &
            MPI_PROBE,                    &
            MPI_RECV,                     &
            MPI_SEND,                     &
            MPI_TEST,                     &
            MPI_WAIT,                     &
            MPI_WTICK,                    &
            MPI_WTIME

INTERFACE MPI_ABORT
  MODULE PROCEDURE MPI_ABORT_NULL
END INTERFACE MPI_ABORT

INTERFACE MPI_BARRIER
  MODULE PROCEDURE MPI_BARRIER_NULL
END INTERFACE MPI_BARRIER

INTERFACE MPI_BCAST
  MODULE PROCEDURE MPI_BCAST_NULL
END INTERFACE MPI_BCAST

INTERFACE MPI_COMM_GET_ATTR
  MODULE PROCEDURE MPI_COMM_GET_ATTR_NULL
END INTERFACE MPI_COMM_GET_ATTR

INTERFACE MPI_COMM_RANK
  MODULE PROCEDURE MPI_COMM_RANK_NULL
END INTERFACE MPI_COMM_RANK

INTERFACE MPI_COMM_SIZE
  MODULE PROCEDURE MPI_COMM_SIZE_NULL
END INTERFACE MPI_COMM_SIZE

INTERFACE MPI_ERROR_STRING
  MODULE PROCEDURE MPI_ERROR_STRING_NULL
END INTERFACE MPI_ERROR_STRING

INTERFACE MPI_FINALIZE
  MODULE PROCEDURE MPI_FINALIZE_NULL
END INTERFACE MPI_FINALIZE

INTERFACE MPI_FINALIZED
  MODULE PROCEDURE MPI_FINALIZED_NULL
END INTERFACE MPI_FINALIZED

INTERFACE MPI_GET_COUNT
  MODULE PROCEDURE MPI_GET_COUNT_NULL
END INTERFACE MPI_GET_COUNT

INTERFACE MPI_GET_VERSION
  MODULE PROCEDURE MPI_GET_VERSION_NULL
END INTERFACE MPI_GET_VERSION

INTERFACE MPI_INIT
  MODULE PROCEDURE MPI_INIT_NULL
END INTERFACE MPI_INIT

INTERFACE MPI_INITIALIZED
  MODULE PROCEDURE MPI_INITIALIZED_NULL
END INTERFACE MPI_INITIALIZED

INTERFACE MPI_IPROBE
  MODULE PROCEDURE MPI_IPROBE_NULL
END INTERFACE MPI_IPROBE

INTERFACE MPI_IRECV
  MODULE PROCEDURE MPI_IRECV_NULL
END INTERFACE MPI_IRECV

INTERFACE MPI_ISEND
  MODULE PROCEDURE MPI_ISEND_NULL
END INTERFACE MPI_ISEND

INTERFACE MPI_PROBE
  MODULE PROCEDURE MPI_PROBE_NULL
END INTERFACE MPI_PROBE

INTERFACE MPI_RECV
  MODULE PROCEDURE MPI_RECV_NULL
END INTERFACE MPI_RECV

INTERFACE MPI_SEND
  MODULE PROCEDURE MPI_SEND_NULL
END INTERFACE MPI_SEND

INTERFACE MPI_TEST
  MODULE PROCEDURE MPI_TEST_NULL
END INTERFACE MPI_TEST

INTERFACE MPI_WAIT
  MODULE PROCEDURE MPI_WAIT_NULL
END INTERFACE MPI_WAIT

INTERFACE MPI_WTICK
  MODULE PROCEDURE MPI_WTICK_NULL
END INTERFACE MPI_WTICK

INTERFACE MPI_WTIME
  MODULE PROCEDURE MPI_WTIME_NULL
END INTERFACE MPI_WTIME

CONTAINS

SUBROUTINE MPI_ABORT_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_ABORT_NULL

SUBROUTINE MPI_BARRIER_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_BARRIER_NULL

SUBROUTINE MPI_BCAST_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_BCAST_NULL

SUBROUTINE MPI_COMM_GET_ATTR_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_COMM_GET_ATTR_NULL

SUBROUTINE MPI_COMM_RANK_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_COMM_RANK_NULL

SUBROUTINE MPI_COMM_SIZE_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_COMM_SIZE_NULL

SUBROUTINE MPI_ERROR_STRING_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_ERROR_STRING_NULL

SUBROUTINE MPI_FINALIZE_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_FINALIZE_NULL

SUBROUTINE MPI_FINALIZED_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_FINALIZED_NULL

SUBROUTINE MPI_GET_COUNT_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_GET_COUNT_NULL

SUBROUTINE MPI_GET_VERSION_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_GET_VERSION_NULL

SUBROUTINE MPI_INIT_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_INIT_NULL

SUBROUTINE MPI_INITIALIZED_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_INITIALIZED_NULL

SUBROUTINE MPI_IPROBE_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_IPROBE_NULL

SUBROUTINE MPI_IRECV_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_IRECV_NULL

SUBROUTINE MPI_ISEND_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_ISEND_NULL

SUBROUTINE MPI_PROBE_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_PROBE_NULL

SUBROUTINE MPI_RECV_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_RECV_NULL

SUBROUTINE MPI_SEND_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_SEND_NULL

SUBROUTINE MPI_TEST_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_TEST_NULL

SUBROUTINE MPI_WAIT_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_WAIT_NULL

SUBROUTINE MPI_WTICK_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_WTICK_NULL

SUBROUTINE MPI_WTIME_NULL(errorDetected)
  LOGICAL, INTENT(OUT) :: errorDetected
   errorDetected = .TRUE.
  RETURN
END SUBROUTINE MPI_WTIME_NULL

END MODULE MPI2DataClassNoErrClass
