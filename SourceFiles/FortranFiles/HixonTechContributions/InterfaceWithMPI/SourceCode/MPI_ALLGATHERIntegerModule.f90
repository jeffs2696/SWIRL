MODULE MPI_ALLGATHERIntegerCall

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLGATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLGATHERInteger

CONTAINS

  SUBROUTINE MPIALLGATHERInteger(SENDBUF, SENDCOUNT, SENDTYPE, &
                                 RECVBUF, RECVCOUNT, RECVTYPE, &
                                 COMM, IERROR)
    INTEGER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    INTEGER, DIMENSION(:,:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE, &
                       RECVBUF, RECVCOUNT, RECVTYPE, &
                       COMM, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPIALLGATHERInteger

END MODULE MPI_ALLGATHERIntegerCall

MODULE MPI_ALLGATHERIntegerModule
  USE MPI_ALLGATHERIntegerCall

  ! this module is the wrapper to make the new F95 MPI_ALLGATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_ALLGATHER

  INTERFACE MPI_ALLGATHER
    MODULE PROCEDURE MPI_ALLGATHERInteger
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLGATHERInteger(SENDBUF, SENDCOUNT, SENDTYPE, &
                                  RECVBUF, RECVCOUNT, RECVTYPE, &
                                  COMM, IERROR)
    INTEGER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    INTEGER, DIMENSION(:,:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLGATHERInteger(SENDBUF   = SENDBUF,   &
                             SENDCOUNT = SENDCOUNT, &
                             SENDTYPE  = SENDTYPE,  &
                             RECVBUF   = RECVBUF,   &
                             RECVCOUNT = RECVCOUNT, &
                             RECVTYPE  = RECVTYPE,  &
                             COMM      = COMM,      &
                             IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLGATHERInteger

END MODULE MPI_ALLGATHERIntegerModule
