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

MODULE MPI_RECVInt4Call0D

  USE IncludeMPIImplementation
  USE MPIKindDefs

! this module actually makes the F77 MPI_RECV call for integer data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIRecvInt40D

CONTAINS

SUBROUTINE MPIRecvInt40D(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  INTEGER(KIND=int4Kind), INTENT(OUT) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_RECV(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIRecvInt40D

END MODULE MPI_RECVInt4Call0D

MODULE MPI_RECVInt4Module0D
  USE MPI_RECVInt4Call0D

! this module is the wrapper to make the new F95 MPI_RECV
!  call _look like_ the standard F77 call.

  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE
  USE MPIKindDefs

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_RECV

INTERFACE HTMPI_RECV
  MODULE PROCEDURE MPI_RECVInt40D
END INTERFACE HTMPI_RECV

CONTAINS

SUBROUTINE MPI_RECVInt40D(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  INTEGER(KIND=int4Kind), INTENT(OUT) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIRecvInt40D(BUF      = BUF,      &
                         COUNT    = COUNT,    &
                         DATATYPE = DATATYPE, &
                         SOURCE   = SOURCE,   &
                         TAG      = TAG,      &
                         COMM     = COMM,     &
                         STATUS   = STATUS,   &
                         IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_RECVInt40D

END MODULE MPI_RECVInt4Module0D
