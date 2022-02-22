!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                    !
!  This code copyright 2007 by Hixon Technologies, LLC.              !
!                                                                    !
!  Used by permission in the NASA BASS CAA code, and                 !
!   cannot be modified or used in other applications without the     !
!   express permission of Hixon Technologies, LLC.                   !
!                                                                    !
!  Contact:  Ray Hixon, Hixon Technologies, LLC.                     !
!            (440) 979-1783                                          !
!            email:  rhixon@wideopenwest.com                         !
!                                                                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE MPI_SENDRECVLogicalCall0D

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_SENDRECV call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPISendRecvLogical0D

CONTAINS

SUBROUTINE MPISendRecvLogical0D(SENDBUF,SENDCOUNT,SENDTYPE,DEST,SENDTAG, &
           RECVBUF,RECVCOUNT,RECVTYPE,SOURCE,RECVTAG,COMM,STATUS,IERROR)
  LOGICAL, INTENT(IN)  :: SENDBUF
  LOGICAL, INTENT(OUT) :: RECVBUF
  INTEGER, INTENT(IN)  :: SENDCOUNT, RECVCOUNT
  INTEGER, INTENT(IN)  :: SENDTYPE, RECVTYPE
  INTEGER, INTENT(IN)  :: DEST, SOURCE
  INTEGER, INTENT(IN)  :: SENDTAG, RECVTAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_SENDRECV(SENDBUF,SENDCOUNT,SENDTYPE,DEST,SENDTAG,   &
                    RECVBUF,RECVCOUNT,RECVTYPE,SOURCE,RECVTAG, &
                    COMM,STATUS,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPISendRecvLogical0D

END MODULE MPI_SENDRECVLogicalCall0D

MODULE MPI_SENDRECVLogicalModule0D
  USE MPI_SENDRECVLogicalCall0D
  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

! this module is the wrapper to make the new F95 MPI_SENDRECV
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_SENDRECV

INTERFACE HTMPI_SENDRECV
  MODULE PROCEDURE MPI_SENDRECVLogical0D
END INTERFACE HTMPI_SENDRECV

CONTAINS

SUBROUTINE MPI_SENDRECVLogical0D(SENDBUF,SENDCOUNT,SENDTYPE,DEST,SENDTAG, &
           RECVBUF,RECVCOUNT,RECVTYPE,SOURCE,RECVTAG,COMM,STATUS,IERROR)
  LOGICAL, INTENT(IN)  :: SENDBUF
  LOGICAL, INTENT(OUT) :: RECVBUF
  INTEGER, INTENT(IN)  :: SENDCOUNT, RECVCOUNT
  INTEGER, INTENT(IN)  :: SENDTYPE, RECVTYPE
  INTEGER, INTENT(IN)  :: DEST, SOURCE
  INTEGER, INTENT(IN)  :: SENDTAG, RECVTAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPISendRecvLogical0D(SENDBUF   = SENDBUF,   &
                            SENDCOUNT = SENDCOUNT, &
                            SENDTYPE  = SENDTYPE,  &
                            DEST      = DEST,      &
                            SENDTAG   = SENDTAG,   &
                            RECVBUF   = RECVBUF,   &
                            RECVCOUNT = RECVCOUNT, &
                            RECVTYPE  = RECVTYPE,  &
                            SOURCE    = SOURCE,    &
                            RECVTAG   = RECVTAG,   &
                            COMM      = COMM,      &
                            STATUS    = STATUS,    &
                            IERROR    = IERROR)

  RETURN
END SUBROUTINE MPI_SENDRECVLogical0D

END MODULE MPI_SENDRECVLogicalModule0D
