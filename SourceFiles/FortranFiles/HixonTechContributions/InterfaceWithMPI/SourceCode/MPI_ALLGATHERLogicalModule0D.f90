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

MODULE MPI_ALLGATHERLogicalCall0D

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_ALLGATHER call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIAllGatherLogical0D

CONTAINS

SUBROUTINE MPIAllGatherLogical0D(SENDBUF,SENDCOUNT,SENDTYPE, &
           RECVBUF,RECVCOUNT,RECVTYPE,COMM,IERROR)
  LOGICAL              , INTENT(IN)  :: SENDBUF
  LOGICAL, DIMENSION(:), INTENT(OUT) :: RECVBUF
  INTEGER, INTENT(IN)  :: SENDCOUNT, RECVCOUNT
  INTEGER, INTENT(IN)  :: SENDTYPE, RECVTYPE
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_ALLGATHER(SENDBUF,   SENDCOUNT,SENDTYPE, &
                     RECVBUF(1),RECVCOUNT,RECVTYPE, &
                     COMM,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIAllGatherLogical0D

END MODULE MPI_ALLGATHERLogicalCall0D

MODULE MPI_ALLGATHERLogicalModule0D
  USE MPI_ALLGATHERLogicalCall0D

! this module is the wrapper to make the new F95 MPI_ALLGATHER
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ALLGATHER

INTERFACE HTMPI_ALLGATHER
  MODULE PROCEDURE MPI_ALLGATHERLogical0D
END INTERFACE HTMPI_ALLGATHER

CONTAINS

SUBROUTINE MPI_ALLGATHERLogical0D(SENDBUF,SENDCOUNT,SENDTYPE, &
           RECVBUF,RECVCOUNT,RECVTYPE,COMM,IERROR)
  LOGICAL              , INTENT(IN)  :: SENDBUF
  LOGICAL, DIMENSION(:), INTENT(OUT) :: RECVBUF
  INTEGER, INTENT(IN)  :: SENDCOUNT, RECVCOUNT
  INTEGER, INTENT(IN)  :: SENDTYPE, RECVTYPE
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIAllGatherLogical0D(SENDBUF   = SENDBUF,   &
                             SENDCOUNT = SENDCOUNT, &
                             SENDTYPE  = SENDTYPE,  &
                             RECVBUF   = RECVBUF,   &
                             RECVCOUNT = RECVCOUNT, &
                             RECVTYPE  = RECVTYPE,  &
                             COMM      = COMM,      &
                             IERROR    = IERROR)

  RETURN
END SUBROUTINE MPI_ALLGATHERLogical0D

END MODULE MPI_ALLGATHERLogicalModule0D
