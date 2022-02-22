MODULE MPI_GATHERReal8Call0D

  USE IncludeMPIImplementation
  USE MPIKindDefs

  ! this module actually makes the F77 MPI_GATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIGATHERReal80D

CONTAINS

  SUBROUTINE MPIGATHERReal80D(SENDBUF, SENDCOUNT, SENDTYPE, &
                           RECVBUF, RECVCOUNT, RECVTYPE, &
                           ROOT, COMM, IERROR)
    REAL(KIND=real8Kind), INTENT(IN)  :: SENDBUF
    REAL(KIND=real8Kind), DIMENSION(:), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIGATHERReal80D

END MODULE MPI_GATHERReal8Call0D

MODULE MPI_GATHERReal8Module0D
  USE MPI_GATHERReal8Call0D
  USE MPIKindDefs

  ! this module is the wrapper to make the new F95 MPI_GATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GATHER

  INTERFACE HTMPI_GATHER
    MODULE PROCEDURE MPI_GATHERReal80D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_GATHERReal80D(SENDBUF, SENDCOUNT, SENDTYPE, &
                              RECVBUF, RECVCOUNT, RECVTYPE, &
                              ROOT, COMM, IERROR)
    REAL(KIND=real8Kind), INTENT(IN)  :: SENDBUF
    REAL(KIND=real8Kind), DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIGATHERReal80D(SENDBUF   = SENDBUF,   &
                         SENDCOUNT = SENDCOUNT, &
                         SENDTYPE  = SENDTYPE,  &
                         RECVBUF   = RECVBUF,   &
                         RECVCOUNT = RECVCOUNT, &
                         RECVTYPE  = RECVTYPE,  &
                         ROOT      = ROOT,      &
                         COMM      = COMM,      &
                         IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_GATHERReal80D

END MODULE MPI_GATHERReal8Module0D
