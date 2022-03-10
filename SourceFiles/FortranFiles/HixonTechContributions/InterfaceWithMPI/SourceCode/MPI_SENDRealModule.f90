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

MODULE MPI_SENDRealCall

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_SEND call for real data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPISendReal

CONTAINS

  SUBROUTINE MPISendReal(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
    REAL, INTENT(IN), DIMENSION(:) :: BUF
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
  END SUBROUTINE MPISendReal

END MODULE MPI_SENDRealCall

MODULE MPI_SENDRealModule
  USE MPI_SENDRealCall

  ! this module is the wrapper to make the new F95 MPI_SEND
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_SEND

  INTERFACE MPI_SEND
    MODULE PROCEDURE MPI_SENDReal
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_SENDReal(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
    REAL, INTENT(IN), DIMENSION(:) :: BUF
    INTEGER, INTENT(IN)  :: COUNT
    INTEGER, INTENT(IN)  :: DATATYPE
    INTEGER, INTENT(IN)  :: DEST
    INTEGER, INTENT(IN)  :: TAG
    INTEGER, INTENT(IN)  :: COMM
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPISendReal(BUF      = BUF,      &
                     COUNT    = COUNT,    &
                     DATATYPE = DATATYPE, &
                     DEST     = DEST,     &
                     TAG      = TAG,      &
                     COMM     = COMM,     &
                     IERROR   = IERROR)

    RETURN
  END SUBROUTINE MPI_SENDReal

END MODULE MPI_SENDRealModule
