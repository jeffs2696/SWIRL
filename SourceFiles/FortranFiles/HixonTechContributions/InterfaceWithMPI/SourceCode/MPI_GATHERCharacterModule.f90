MODULE MPI_GATHERCharacterCall

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_GATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIGATHERCharacter

INTERFACE MPIGATHERCharacter
  MODULE PROCEDURE MPIGATHERCharacter1
  MODULE PROCEDURE MPIGATHERCharacter2
END INTERFACE 

CONTAINS

  SUBROUTINE MPIGATHERCharacter1(SENDBUF, SENDCOUNT, SENDTYPE, &
                                 RECVBUF, RECVCOUNT, RECVTYPE, &
                                 ROOT, COMM, IERROR)
    CHARACTER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIGATHERCharacter1

  SUBROUTINE MPIGATHERCharacter2(SENDBUF, SENDCOUNT, SENDTYPE, &
                                 RECVBUF, RECVCOUNT, RECVTYPE, &
                                 ROOT, COMM, IERROR)
    CHARACTER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:,:), INTENT(OUT) :: RECVBUF
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
  END SUBROUTINE MPIGATHERCharacter2

END MODULE MPI_GATHERCharacterCall

MODULE MPI_GATHERCharacterModule
  USE MPI_GATHERCharacterCall

  ! this module is the wrapper to make the new F95 MPI_GATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GATHER

  INTERFACE HTMPI_GATHER
    MODULE PROCEDURE MPI_GATHERCharacter1
    MODULE PROCEDURE MPI_GATHERCharacter2
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_GATHERCharacter1(SENDBUF, SENDCOUNT, SENDTYPE, &
                                  RECVBUF, RECVCOUNT, RECVTYPE, &
                                  ROOT, COMM, IERROR)
    CHARACTER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIGATHERCharacter(SENDBUF   = SENDBUF,   &
                            SENDCOUNT = SENDCOUNT, &
                            SENDTYPE  = SENDTYPE,  &
                            RECVBUF   = RECVBUF,   &
                            RECVCOUNT = RECVCOUNT, &
                            RECVTYPE  = RECVTYPE,  &
                            ROOT      = ROOT,      &
                            COMM      = COMM,      &
                            IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_GATHERCharacter1

  SUBROUTINE MPI_GATHERCharacter2(SENDBUF, SENDCOUNT, SENDTYPE, &
                                  RECVBUF, RECVCOUNT, RECVTYPE, &
                                  ROOT, COMM, IERROR)
    CHARACTER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:,:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: ROOT
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIGATHERCharacter(SENDBUF   = SENDBUF,   &
                            SENDCOUNT = SENDCOUNT, &
                            SENDTYPE  = SENDTYPE,  &
                            RECVBUF   = RECVBUF,   &
                            RECVCOUNT = RECVCOUNT, &
                            RECVTYPE  = RECVTYPE,  &
                            ROOT      = ROOT,      &
                            COMM      = COMM,      &
                            IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_GATHERCharacter2

END MODULE MPI_GATHERCharacterModule
