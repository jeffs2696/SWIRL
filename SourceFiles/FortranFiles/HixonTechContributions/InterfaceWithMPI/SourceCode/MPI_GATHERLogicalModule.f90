MODULE MPI_GATHERLogicalCall

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_GATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIGATHERLogical

INTERFACE MPIGATHERLogical
  MODULE PROCEDURE MPIGATHERLogical1
  MODULE PROCEDURE MPIGATHERLogical2
END INTERFACE 

CONTAINS

  SUBROUTINE MPIGATHERLogical1(SENDBUF, SENDCOUNT, SENDTYPE, &
                               RECVBUF, RECVCOUNT, RECVTYPE, &
                               ROOT, COMM, IERROR)
    LOGICAL, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    LOGICAL, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_GATHER(SENDBUF(1), SENDCOUNT, SENDTYPE, &
                    RECVBUF(1), RECVCOUNT, RECVTYPE, &
                    ROOT, COMM, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPIGATHERLogical1

  SUBROUTINE MPIGATHERLogical2(SENDBUF, SENDCOUNT, SENDTYPE, &
                               RECVBUF, RECVCOUNT, RECVTYPE, &
                               ROOT, COMM, IERROR)
    LOGICAL, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    LOGICAL, DIMENSION(:,:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_GATHER(SENDBUF(1),   SENDCOUNT, SENDTYPE, &
                    RECVBUF(1,1), RECVCOUNT, RECVTYPE, &
                    ROOT, COMM, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPIGATHERLogical2

END MODULE MPI_GATHERLogicalCall

MODULE MPI_GATHERLogicalModule
  USE MPI_GATHERLogicalCall

  ! this module is the wrapper to make the new F95 MPI_GATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GATHER

  INTERFACE HTMPI_GATHER
    MODULE PROCEDURE MPI_GATHERLogical1
    MODULE PROCEDURE MPI_GATHERLogical2
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_GATHERLogical1(SENDBUF, SENDCOUNT, SENDTYPE, &
                                RECVBUF, RECVCOUNT, RECVTYPE, &
                                ROOT, COMM, IERROR)
    LOGICAL, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    LOGICAL, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIGATHERLogical(SENDBUF   = SENDBUF,   &
                          SENDCOUNT = SENDCOUNT, &
                          SENDTYPE  = SENDTYPE,  &
                          RECVBUF   = RECVBUF,   &
                          RECVCOUNT = RECVCOUNT, &
                          RECVTYPE  = RECVTYPE,  &
                          ROOT      = ROOT,      &
                          COMM      = COMM,      &
                          IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_GATHERLogical1

  SUBROUTINE MPI_GATHERLogical2(SENDBUF, SENDCOUNT, SENDTYPE, &
                                RECVBUF, RECVCOUNT, RECVTYPE, &
                                ROOT, COMM, IERROR)
    LOGICAL, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    LOGICAL, DIMENSION(:,:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIGATHERLogical(SENDBUF   = SENDBUF,   &
                          SENDCOUNT = SENDCOUNT, &
                          SENDTYPE  = SENDTYPE,  &
                          RECVBUF   = RECVBUF,   &
                          RECVCOUNT = RECVCOUNT, &
                          RECVTYPE  = RECVTYPE,  &
                          ROOT      = ROOT,      &
                          COMM      = COMM,      &
                          IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_GATHERLogical2

END MODULE MPI_GATHERLogicalModule
