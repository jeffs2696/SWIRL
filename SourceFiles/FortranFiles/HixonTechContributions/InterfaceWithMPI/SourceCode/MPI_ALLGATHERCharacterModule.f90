MODULE MPI_ALLGATHERCharacterCall

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_ALLGATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIALLGATHERCharacter

  INTERFACE MPIALLGATHERCharacter
    MODULE PROCEDURE MPIALLGATHERCharacter1
    MODULE PROCEDURE MPIALLGATHERCharacter2
  END INTERFACE

CONTAINS

  SUBROUTINE MPIALLGATHERCharacter1(SENDBUF, SENDCOUNT, SENDTYPE, &
                                    RECVBUF, RECVCOUNT, RECVTYPE, &
                                    COMM, IERROR)
    CHARACTER, DIMENSION(:), INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_ALLGATHER(SENDBUF(1), SENDCOUNT, SENDTYPE, &
                       RECVBUF(1), RECVCOUNT, RECVTYPE, &
                       COMM, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPIALLGATHERCharacter1

  SUBROUTINE MPIALLGATHERCharacter2(SENDBUF, SENDCOUNT, SENDTYPE, &
                                    RECVBUF, RECVCOUNT, RECVTYPE, &
                                    COMM, IERROR)
    CHARACTER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:,:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_ALLGATHER(SENDBUF(1),   SENDCOUNT, SENDTYPE, &
                       RECVBUF(1,1), RECVCOUNT, RECVTYPE, &
                       COMM, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPIALLGATHERCharacter2

END MODULE MPI_ALLGATHERCharacterCall

MODULE MPI_ALLGATHERCharacterModule
  USE MPI_ALLGATHERCharacterCall

  ! this module is the wrapper to make the new F95 MPI_ALLGATHER
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ALLGATHER

  INTERFACE HTMPI_ALLGATHER
    MODULE PROCEDURE MPI_ALLGATHERCharacter1
    MODULE PROCEDURE MPI_ALLGATHERCharacter2
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_ALLGATHERCharacter1(SENDBUF, SENDCOUNT, SENDTYPE, &
                                     RECVBUF, RECVCOUNT, RECVTYPE, &
                                     COMM, IERROR)
    CHARACTER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLGATHERCharacter(SENDBUF   = SENDBUF,   &
                               SENDCOUNT = SENDCOUNT, &
                               SENDTYPE  = SENDTYPE,  &
                               RECVBUF   = RECVBUF,   &
                               RECVCOUNT = RECVCOUNT, &
                               RECVTYPE  = RECVTYPE,  &
                               COMM      = COMM,      &
                               IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLGATHERCharacter1

  SUBROUTINE MPI_ALLGATHERCharacter2(SENDBUF, SENDCOUNT, SENDTYPE, &
                                     RECVBUF, RECVCOUNT, RECVTYPE, &
                                     COMM, IERROR)
    CHARACTER, DIMENSION(:)  , INTENT(IN)  :: SENDBUF
    CHARACTER, DIMENSION(:,:), INTENT(OUT) :: RECVBUF
    INTEGER, INTENT(IN)  :: SENDCOUNT
    INTEGER, INTENT(IN)  :: RECVCOUNT
    INTEGER, INTENT(IN)  :: SENDTYPE
    INTEGER, INTENT(IN)  :: RECVTYPE
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPIALLGATHERCharacter(SENDBUF   = SENDBUF,   &
                               SENDCOUNT = SENDCOUNT, &
                               SENDTYPE  = SENDTYPE,  &
                               RECVBUF   = RECVBUF,   &
                               RECVCOUNT = RECVCOUNT, &
                               RECVTYPE  = RECVTYPE,  &
                               COMM      = COMM,      &
                               IERROR    = IERROR)

    RETURN
  END SUBROUTINE MPI_ALLGATHERCharacter2

END MODULE MPI_ALLGATHERCharacterModule
