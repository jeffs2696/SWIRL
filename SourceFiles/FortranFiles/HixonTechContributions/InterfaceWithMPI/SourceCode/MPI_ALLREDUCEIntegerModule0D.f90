MODULE MPI_ALLREDUCEIntegerCall0D

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLREDUCE call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLREDUCEInteger0D

CONTAINS

  SUBROUTINE MPIALLREDUCEInteger0D(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                   COMM, IERROR)
    INTEGER, INTENT(IN)  :: SENDBUF
    INTEGER, INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIALLREDUCEInteger0D

END MODULE MPI_ALLREDUCEIntegerCall0D

MODULE MPI_ALLREDUCEIntegerModule0D
  USE MPI_ALLREDUCEIntegerCall0D

  ! this module is the wrapper to make the new F95 MPI_ALLREDUCE
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_ALLREDUCE

  INTERFACE MPI_ALLREDUCE
    MODULE PROCEDURE MPI_ALLREDUCEInteger0D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLREDUCEInteger0D(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                    COMM, IERROR)
    INTEGER, INTENT(IN)  :: SENDBUF
    INTEGER, INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: COUNT
    INTEGER, INTENT(IN)  :: DATATYPE
    INTEGER, INTENT(IN)  :: OPER
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLREDUCEInteger0D(SENDBUF   = SENDBUF,   &
                               RECVBUF   = RECVBUF,   &
                               COUNT     = COUNT,     &
                               DATATYPE  = DATATYPE,  &
                               OPER      = OPER,      &
                               COMM      = COMM,      &
                               IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLREDUCEInteger0D

END MODULE MPI_ALLREDUCEIntegerModule0D
