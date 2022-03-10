MODULE MPI_ALLGATHERRealCall0D

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLGATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLGATHERReal0D

CONTAINS

  SUBROUTINE MPIALLGATHERReal0D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                RECVBUF, RECVCOUNT, RECVTYPE, &
                                COMM, IERROR)
    REAL              , INTENT(IN)  :: SENDBUF
    REAL, DIMENSION(:), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIALLGATHERReal0D

END MODULE MPI_ALLGATHERRealCall0D

MODULE MPI_ALLGATHERRealModule0D
  USE MPI_ALLGATHERRealCall0D

  ! this module is the wrapper to make the new F95 MPI_ALLGATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_ALLGATHER

  INTERFACE MPI_ALLGATHER
    MODULE PROCEDURE MPI_ALLGATHERReal0D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLGATHERReal0D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                 RECVBUF, RECVCOUNT, RECVTYPE, &
                                 COMM, IERROR)
    REAL              , INTENT(IN)  :: SENDBUF
    REAL, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLGATHERReal0D(SENDBUF   = SENDBUF,   &
                            SENDCOUNT = SENDCOUNT, &
                            SENDTYPE  = SENDTYPE,  &
                            RECVBUF   = RECVBUF,   &
                            RECVCOUNT = RECVCOUNT, &
                            RECVTYPE  = RECVTYPE,  &
                            COMM      = COMM,      &
                            IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLGATHERReal0D

END MODULE MPI_ALLGATHERRealModule0D
