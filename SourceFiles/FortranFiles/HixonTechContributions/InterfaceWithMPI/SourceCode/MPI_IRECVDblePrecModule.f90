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

MODULE MPI_IRECVDblePrecCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_IRECV call for real8 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIIrecvDblePrec

CONTAINS

SUBROUTINE MPIIrecvDblePrec(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)
  DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_IRECV(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIIrecvDblePrec

END MODULE MPI_IRECVDblePrecCall

MODULE MPI_IRECVDblePrecModule
  USE MPI_IRECVDblePrecCall

! this module is the wrapper to make the new F95 MPI_IRECV
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_IRECV

INTERFACE MPI_IRECV
  MODULE PROCEDURE MPI_IRECVDblePrec
END INTERFACE MPI_IRECV

CONTAINS

SUBROUTINE MPI_IRECVDblePrec(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)
  DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIIrecvDblePrec(BUF      = BUF,      &
                     COUNT    = COUNT,    &
                     DATATYPE = DATATYPE, &
                     SOURCE   = SOURCE,   &
                     TAG      = TAG,      &
                     COMM     = COMM,     &
                     REQUEST  = REQUEST,  &
                     IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_IRECVDblePrec

END MODULE MPI_IRECVDblePrecModule
