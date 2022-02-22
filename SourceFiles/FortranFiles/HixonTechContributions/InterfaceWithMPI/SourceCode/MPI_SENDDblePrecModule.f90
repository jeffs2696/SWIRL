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

MODULE MPI_SENDDblePrecCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_SEND call for real8 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPISendDblePrec

CONTAINS

SUBROUTINE MPISendDblePrec(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
  DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_SEND(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPISendDblePrec

END MODULE MPI_SENDDblePrecCall

MODULE MPI_SENDDblePrecModule
  USE MPI_SENDDblePrecCall

! this module is the wrapper to make the new F95 MPI_SEND
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_SEND

INTERFACE MPI_SEND
  MODULE PROCEDURE MPI_SENDDblePrec
END INTERFACE MPI_SEND

CONTAINS

SUBROUTINE MPI_SENDDblePrec(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
  DOUBLE PRECISION, INTENT(IN), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPISendDblePrec(BUF      = BUF,      &
                    COUNT    = COUNT,    &
                    DATATYPE = DATATYPE, &
                    DEST     = DEST,     &
                    TAG      = TAG,      &
                    COMM     = COMM,     &
                    IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_SENDDblePrec

END MODULE MPI_SENDDblePrecModule
