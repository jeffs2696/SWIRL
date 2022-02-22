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

MODULE MPI_RECVRealCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_RECV call for real data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIRecvReal

CONTAINS

SUBROUTINE MPIRecvReal(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  REAL, INTENT(OUT), DIMENSION(:) :: BUF
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
END SUBROUTINE MPIRecvReal

END MODULE MPI_RECVRealCall

MODULE MPI_RECVRealModule
  USE MPI_RECVRealCall

! this module is the wrapper to make the new F95 MPI_RECV
!  call _look like_ the standard F77 call.

  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_RECV

INTERFACE MPI_RECV
  MODULE PROCEDURE MPI_RECVReal
END INTERFACE MPI_RECV

CONTAINS

SUBROUTINE MPI_RECVReal(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  REAL, INTENT(OUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIRecvReal(BUF      = BUF,      &
                    COUNT    = COUNT,    &
                    DATATYPE = DATATYPE, &
                    SOURCE   = SOURCE,   &
                    TAG      = TAG,      &
                    COMM     = COMM,     &
                    STATUS   = STATUS,   &
                    IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_RECVReal

END MODULE MPI_RECVRealModule
