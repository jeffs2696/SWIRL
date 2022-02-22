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

MODULE MPI_RECVReal8Call

  USE IncludeMPIImplementation
  USE MPIKindDefs

! this module actually makes the F77 MPI_RECV call for real data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIRecvReal8

CONTAINS

SUBROUTINE MPIRecvReal8(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  REAL(KIND=real8Kind), INTENT(OUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_RECV(BUF(1),COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIRecvReal8

END MODULE MPI_RECVReal8Call

MODULE MPI_RECVReal8Module
  USE MPI_RECVReal8Call
  USE MPIKindDefs

! this module is the wrapper to make the new F95 MPI_RECV
!  call _look like_ the standard F77 call.

  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_RECV

INTERFACE HTMPI_RECV
  MODULE PROCEDURE MPI_RECVReal8
END INTERFACE HTMPI_RECV

CONTAINS

SUBROUTINE MPI_RECVReal8(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  REAL(KIND=real8Kind), INTENT(OUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIRecvReal8(BUF      = BUF,      &
                    COUNT    = COUNT,    &
                    DATATYPE = DATATYPE, &
                    SOURCE   = SOURCE,   &
                    TAG      = TAG,      &
                    COMM     = COMM,     &
                    STATUS   = STATUS,   &
                    IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_RECVReal8

END MODULE MPI_RECVReal8Module
