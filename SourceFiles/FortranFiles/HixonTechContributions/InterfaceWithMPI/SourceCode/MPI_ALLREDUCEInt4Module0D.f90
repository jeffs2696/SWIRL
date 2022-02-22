MODULE MPI_ALLREDUCEInt4Call0D

  USE IncludeMPIImplementation
  USE MPIKindDefs

  ! this module actually makes the F77 MPI_ALLREDUCE call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLREDUCEInt40D

CONTAINS

  SUBROUTINE MPIALLREDUCEInt40D(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                   COMM, IERROR)
    INTEGER(KIND=int4Kind), INTENT(IN)  :: SENDBUF
    INTEGER(KIND=int4Kind), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIALLREDUCEInt40D

END MODULE MPI_ALLREDUCEInt4Call0D

MODULE MPI_ALLREDUCEInt4Module0D
  USE MPI_ALLREDUCEInt4Call0D
  USE MPIKindDefs

  ! this module is the wrapper to make the new F95 MPI_ALLREDUCE
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ALLREDUCE

  INTERFACE HTMPI_ALLREDUCE
    MODULE PROCEDURE MPI_ALLREDUCEInt40D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLREDUCEInt40D(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                    COMM, IERROR)
    INTEGER(KIND=int4Kind), INTENT(IN)  :: SENDBUF
    INTEGER(KIND=int4Kind), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: COUNT
    INTEGER, INTENT(IN)  :: DATATYPE
    INTEGER, INTENT(IN)  :: OPER
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLREDUCEInt40D(SENDBUF   = SENDBUF,   &
                               RECVBUF   = RECVBUF,   &
                               COUNT     = COUNT,     &
                               DATATYPE  = DATATYPE,  &
                               OPER      = OPER,      &
                               COMM      = COMM,      &
                               IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLREDUCEInt40D

END MODULE MPI_ALLREDUCEInt4Module0D
