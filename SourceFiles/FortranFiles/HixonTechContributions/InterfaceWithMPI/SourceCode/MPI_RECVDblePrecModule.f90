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

MODULE MPI_RECVDblePrecCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_RECV call for real8 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIRecvDblePrec

CONTAINS

SUBROUTINE MPIRecvDblePrec(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: BUF
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
END SUBROUTINE MPIRecvDblePrec

END MODULE MPI_RECVDblePrecCall

MODULE MPI_RECVDblePrecModule
  USE MPI_RECVDblePrecCall

! this module is the wrapper to make the new F95 MPI_RECV
!  call _look like_ the standard F77 call.

  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_RECV

INTERFACE MPI_RECV
  MODULE PROCEDURE MPI_RECVDblePrec
END INTERFACE MPI_RECV

CONTAINS

SUBROUTINE MPI_RECVDblePrec(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIRecvDblePrec(BUF      = BUF,      &
                    COUNT    = COUNT,    &
                    DATATYPE = DATATYPE, &
                    SOURCE   = SOURCE,   &
                    TAG      = TAG,      &
                    COMM     = COMM,     &
                    STATUS   = STATUS,   &
                    IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_RECVDblePrec

END MODULE MPI_RECVDblePrecModule
