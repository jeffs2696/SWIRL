MODULE MPI_ALLGATHERReal4Call0D

  USE IncludeMPIImplementation
  USE MPIKindDefs

  ! this module actually makes the F77 MPI_ALLGATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLGATHERReal40D

CONTAINS

  SUBROUTINE MPIALLGATHERReal40D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                 RECVBUF, RECVCOUNT, RECVTYPE, &
                                 COMM, IERROR)
    REAL(KIND=real4Kind)              , INTENT(IN)  :: SENDBUF
    REAL(KIND=real4Kind), DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_ALLGATHER(SENDBUF,    SENDCOUNT, SENDTYPE, &
                       RECVBUF(1), RECVCOUNT, RECVTYPE, &
                       COMM, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPIALLGATHERReal40D

END MODULE MPI_ALLGATHERReal4Call0D

MODULE MPI_ALLGATHERReal4Module0D
  USE MPI_ALLGATHERReal4Call0D
  USE MPIKindDefs

  ! this module is the wrapper to make the new F95 MPI_ALLGATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ALLGATHER

  INTERFACE HTMPI_ALLGATHER
    MODULE PROCEDURE MPI_ALLGATHERReal40D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLGATHERReal40D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                 RECVBUF, RECVCOUNT, RECVTYPE, &
                                 COMM, IERROR)
    REAL(KIND=real4Kind)              , INTENT(IN)  :: SENDBUF
    REAL(KIND=real4Kind), DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLGATHERReal40D(SENDBUF   = SENDBUF,   &
                             SENDCOUNT = SENDCOUNT, &
                             SENDTYPE  = SENDTYPE,  &
                             RECVBUF   = RECVBUF,   &
                             RECVCOUNT = RECVCOUNT, &
                             RECVTYPE  = RECVTYPE,  &
                             COMM      = COMM,      &
                             IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLGATHERReal40D

END MODULE MPI_ALLGATHERReal4Module0D
