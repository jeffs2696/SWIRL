MODULE MPI_ALLREDUCELogicalCall0D

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLREDUCE call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLREDUCELogical0D

CONTAINS

  SUBROUTINE MPIALLREDUCELogical0D(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                   COMM, IERROR)
    LOGICAL, INTENT(IN)  :: SENDBUF
    LOGICAL, INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: COUNT
    INTEGER, INTENT(IN)  :: DATATYPE
    INTEGER, INTENT(IN)  :: OPER
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_ALLREDUCE(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                       COMM, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPIALLREDUCELogical0D

END MODULE MPI_ALLREDUCELogicalCall0D

MODULE MPI_ALLREDUCELogicalModule0D
  USE MPI_ALLREDUCELogicalCall0D

  ! this module is the wrapper to make the new F95 MPI_ALLREDUCE
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ALLREDUCE

  INTERFACE HTMPI_ALLREDUCE
    MODULE PROCEDURE MPI_ALLREDUCELogical0D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLREDUCELogical0D(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                    COMM, IERROR)
    LOGICAL, INTENT(IN)  :: SENDBUF
    LOGICAL, INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: COUNT
    INTEGER, INTENT(IN)  :: DATATYPE
    INTEGER, INTENT(IN)  :: OPER
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLREDUCELogical0D(SENDBUF   = SENDBUF,   &
                               RECVBUF   = RECVBUF,   &
                               COUNT     = COUNT,     &
                               DATATYPE  = DATATYPE,  &
                               OPER      = OPER,      &
                               COMM      = COMM,      &
                               IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLREDUCELogical0D

END MODULE MPI_ALLREDUCELogicalModule0D
