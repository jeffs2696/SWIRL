MODULE MPI_GATHERCharacterCall0D

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_GATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIGATHERCharacter0D

CONTAINS

  SUBROUTINE MPIGATHERCharacter0D(SENDBUF, SENDCOUNT, SENDTYPE, &
                           RECVBUF, RECVCOUNT, RECVTYPE, &
                           ROOT, COMM, IERROR)
    CHARACTER, INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIGATHERCharacter0D

END MODULE MPI_GATHERCharacterCall0D

MODULE MPI_GATHERCharacterModule0D
  USE MPI_GATHERCharacterCall0D

  ! this module is the wrapper to make the new F95 MPI_GATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GATHER

  INTERFACE HTMPI_GATHER
    MODULE PROCEDURE MPI_GATHERCharacter0D
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_GATHERCharacter0D(SENDBUF, SENDCOUNT, SENDTYPE, &
                                   RECVBUF, RECVCOUNT, RECVTYPE, &
                                   ROOT, COMM, IERROR)
    CHARACTER, INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIGATHERCharacter0D(SENDBUF   = SENDBUF,   &
                              SENDCOUNT = SENDCOUNT, &
                              SENDTYPE  = SENDTYPE,  &
                              RECVBUF   = RECVBUF,   &
                              RECVCOUNT = RECVCOUNT, &
                              RECVTYPE  = RECVTYPE,  &
                              ROOT      = ROOT,      &
                              COMM      = COMM,      &
                              IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_GATHERCharacter0D

END MODULE MPI_GATHERCharacterModule0D
