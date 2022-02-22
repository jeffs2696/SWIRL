MODULE MPI_ALLREDUCEDblePrecCall0D

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLREDUCE call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLREDUCEDblePrec0D

CONTAINS

  SUBROUTINE MPIALLREDUCEDblePrec0D(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                    COMM, IERROR)
    DOUBLE PRECISION, INTENT(IN)  :: SENDBUF
    DOUBLE PRECISION, INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIALLREDUCEDblePrec0D

END MODULE MPI_ALLREDUCEDblePrecCall0D

MODULE MPI_ALLREDUCEDblePrecModule0D
  USE MPI_ALLREDUCEDblePrecCall0D

  ! this module is the wrapper to make the new F95 MPI_ALLREDUCE
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_ALLREDUCE

  INTERFACE MPI_ALLREDUCE
    MODULE PROCEDURE MPI_ALLREDUCEDblePrec0D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLREDUCEDblePrec0D(SENDBUF, RECVBUF, COUNT, DATATYPE, OPER, &
                                     COMM, IERROR)
    DOUBLE PRECISION, INTENT(IN)  :: SENDBUF
    DOUBLE PRECISION, INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: COUNT
    INTEGER, INTENT(IN)  :: DATATYPE
    INTEGER, INTENT(IN)  :: OPER
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLREDUCEDblePrec0D(SENDBUF   = SENDBUF,   &
                                RECVBUF   = RECVBUF,   &
                                COUNT     = COUNT,     &
                                DATATYPE  = DATATYPE,  &
                                OPER      = OPER,      &
                                COMM      = COMM,      &
                                IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLREDUCEDblePrec0D

END MODULE MPI_ALLREDUCEDblePrecModule0D
