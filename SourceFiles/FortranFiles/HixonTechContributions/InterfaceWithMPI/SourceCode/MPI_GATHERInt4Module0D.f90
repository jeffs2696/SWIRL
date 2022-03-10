MODULE MPI_GATHERInt4Call0D

  USE IncludeMPIImplementation
  USE MPIKindDefs

  ! this module actually makes the F77 MPI_GATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIGATHERInt40D

CONTAINS

  SUBROUTINE MPIGATHERInt40D(SENDBUF, SENDCOUNT, SENDTYPE, &
                           RECVBUF, RECVCOUNT, RECVTYPE, &
                           ROOT, COMM, IERROR)
    INTEGER(KIND=int4Kind), INTENT(IN)  :: SENDBUF
    INTEGER(KIND=int4Kind), DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_GATHER(SENDBUF,    SENDCOUNT, SENDTYPE, &
                    RECVBUF(1), RECVCOUNT, RECVTYPE, &
                    ROOT, COMM, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPIGATHERInt40D

END MODULE MPI_GATHERInt4Call0D

MODULE MPI_GATHERInt4Module0D
  USE MPI_GATHERInt4Call0D
  USE MPIKindDefs

  ! this module is the wrapper to make the new F95 MPI_GATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GATHER

  INTERFACE HTMPI_GATHER
    MODULE PROCEDURE MPI_GATHERInt40D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_GATHERInt40D(SENDBUF, SENDCOUNT, SENDTYPE, &
                              RECVBUF, RECVCOUNT, RECVTYPE, &
                              ROOT, COMM, IERROR)
    INTEGER(KIND=int4Kind), INTENT(IN)  :: SENDBUF
    INTEGER(KIND=int4Kind), DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIGATHERInt40D(SENDBUF   = SENDBUF,   &
                            SENDCOUNT = SENDCOUNT, &
                            SENDTYPE  = SENDTYPE,  &
                            RECVBUF   = RECVBUF,   &
                            RECVCOUNT = RECVCOUNT, &
                            RECVTYPE  = RECVTYPE,  &
                            ROOT      = ROOT,      &
                            COMM      = COMM,      &
                            IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_GATHERInt40D

END MODULE MPI_GATHERInt4Module0D
