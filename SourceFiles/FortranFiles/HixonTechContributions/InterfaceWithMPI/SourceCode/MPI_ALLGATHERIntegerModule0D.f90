MODULE MPI_ALLGATHERIntegerCall0D

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLGATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLGATHERInteger0D

CONTAINS

  SUBROUTINE MPIALLGATHERInteger0D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                   RECVBUF, RECVCOUNT, RECVTYPE, &
                                   COMM, IERROR)
    INTEGER              , INTENT(IN)  :: SENDBUF
    INTEGER, DIMENSION(:), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIALLGATHERInteger0D

END MODULE MPI_ALLGATHERIntegerCall0D

MODULE MPI_ALLGATHERIntegerModule0D
  USE MPI_ALLGATHERIntegerCall0D

  ! this module is the wrapper to make the new F95 MPI_ALLGATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_ALLGATHER

  INTERFACE MPI_ALLGATHER
    MODULE PROCEDURE MPI_ALLGATHERInteger0D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLGATHERInteger0D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                    RECVBUF, RECVCOUNT, RECVTYPE, &
                                    COMM, IERROR)
    INTEGER              , INTENT(IN)  :: SENDBUF
    INTEGER, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLGATHERInteger0D(SENDBUF   = SENDBUF,   &
                               SENDCOUNT = SENDCOUNT, &
                               SENDTYPE  = SENDTYPE,  &
                               RECVBUF   = RECVBUF,   &
                               RECVCOUNT = RECVCOUNT, &
                               RECVTYPE  = RECVTYPE,  &
                               COMM      = COMM,      &
                               IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLGATHERInteger0D

END MODULE MPI_ALLGATHERIntegerModule0D
