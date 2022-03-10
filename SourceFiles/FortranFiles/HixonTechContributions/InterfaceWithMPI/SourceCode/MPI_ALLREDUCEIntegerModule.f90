MODULE MPI_ALLREDUCEIntegerCall

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLREDUCE call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLREDUCEInteger

CONTAINS

  SUBROUTINE MPIALLREDUCEInteger(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                 COMM, IERROR)
    INTEGER, DIMENSION(:), INTENT(IN)  :: SENDBUF
    INTEGER, DIMENSION(:), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIALLREDUCEInteger

END MODULE MPI_ALLREDUCEIntegerCall

MODULE MPI_ALLREDUCEIntegerModule
  USE MPI_ALLREDUCEIntegerCall

  ! this module is the wrapper to make the new F95 MPI_ALLREDUCE
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_ALLREDUCE

  INTERFACE MPI_ALLREDUCE
    MODULE PROCEDURE MPI_ALLREDUCEInteger
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLREDUCEInteger(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                  COMM, IERROR)
    INTEGER, DIMENSION(:), INTENT(IN)  :: SENDBUF
    INTEGER, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: COUNT
    INTEGER, INTENT(IN)  :: DATATYPE
    INTEGER, INTENT(IN)  :: OPER
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLREDUCEInteger(SENDBUF   = SENDBUF,   &
                             RECVBUF   = RECVBUF,   &
                             COUNT     = COUNT,     &
                             DATATYPE  = DATATYPE,  &
                             OPER      = OPER,      &
                             COMM      = COMM,      &
                             IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLREDUCEInteger

END MODULE MPI_ALLREDUCEIntegerModule
