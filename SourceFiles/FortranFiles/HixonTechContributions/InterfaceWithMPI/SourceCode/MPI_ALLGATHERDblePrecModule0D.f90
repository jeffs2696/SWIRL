MODULE MPI_ALLGATHERDblePrecCall0D

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLGATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLGATHERDblePrec0D

CONTAINS

  SUBROUTINE MPIALLGATHERDblePrec0D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                    RECVBUF, RECVCOUNT, RECVTYPE, &
                                    COMM, IERROR)
    DOUBLE PRECISION              , INTENT(IN)  :: SENDBUF
    DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIALLGATHERDblePrec0D

END MODULE MPI_ALLGATHERDblePrecCall0D

MODULE MPI_ALLGATHERDblePrecModule0D
  USE MPI_ALLGATHERDblePrecCall0D

  ! this module is the wrapper to make the new F95 MPI_ALLGATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ALLGATHER

  INTERFACE HTMPI_ALLGATHER
    MODULE PROCEDURE MPI_ALLGATHERDblePrec0D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLGATHERDblePrec0D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                     RECVBUF, RECVCOUNT, RECVTYPE, &
                                     COMM, IERROR)
    DOUBLE PRECISION              , INTENT(IN)  :: SENDBUF
    DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLGATHERDblePrec0D(SENDBUF   = SENDBUF,   &
                                SENDCOUNT = SENDCOUNT, &
                                SENDTYPE  = SENDTYPE,  &
                                RECVBUF   = RECVBUF,   &
                                RECVCOUNT = RECVCOUNT, &
                                RECVTYPE  = RECVTYPE,  &
                                COMM      = COMM,      &
                                IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLGATHERDblePrec0D

END MODULE MPI_ALLGATHERDblePrecModule0D
