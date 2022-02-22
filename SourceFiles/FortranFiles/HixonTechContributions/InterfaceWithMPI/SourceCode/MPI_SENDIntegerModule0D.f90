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

MODULE MPI_SENDIntegerCall0D

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_SEND call for real4 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPISendInteger0D

CONTAINS

SUBROUTINE MPISendInteger0D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
  INTEGER, INTENT(IN) :: BUF
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
END SUBROUTINE MPISendInteger0D

END MODULE MPI_SENDIntegerCall0D

MODULE MPI_SENDIntegerModule0D
  USE MPI_SENDIntegerCall0D

! this module is the wrapper to make the new F95 MPI_SEND
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_SEND

INTERFACE MPI_SEND
  MODULE PROCEDURE MPI_SENDInteger0D
END INTERFACE MPI_SEND

CONTAINS

SUBROUTINE MPI_SENDInteger0D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
  INTEGER, INTENT(IN) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPISendInteger0D(BUF      = BUF,      &
                         COUNT    = COUNT,    &
                         DATATYPE = DATATYPE, &
                         DEST     = DEST,     &
                         TAG      = TAG,      &
                         COMM     = COMM,     &
                         IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_SENDInteger0D

END MODULE MPI_SENDIntegerModule0D
