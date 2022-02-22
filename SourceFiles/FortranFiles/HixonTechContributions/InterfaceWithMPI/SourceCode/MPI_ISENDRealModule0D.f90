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

MODULE MPI_ISENDRealCall0D

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_ISEND call for real4 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIIsendReal0D

CONTAINS

SUBROUTINE MPIIsendReal0D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,REQUEST,IERROR)
  REAL, INTENT(IN) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_ISEND(BUF,COUNT,DATATYPE,DEST,TAG,COMM,REQUEST,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIIsendReal0D

END MODULE MPI_ISENDRealCall0D

MODULE MPI_ISENDRealModule0D
  USE MPI_ISENDRealCall0D

! this module is the wrapper to make the new F95 MPI_ISEND
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_ISEND

INTERFACE MPI_ISEND
  MODULE PROCEDURE MPI_ISENDReal0D
END INTERFACE MPI_ISEND

CONTAINS

SUBROUTINE MPI_ISENDReal0D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,REQUEST,IERROR)
  REAL, INTENT(IN) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIIsendReal0D(BUF      = BUF,      &
                       COUNT    = COUNT,    &
                       DATATYPE = DATATYPE, &
                       DEST     = DEST,     &
                       TAG      = TAG,      &
                       COMM     = COMM,     &
                       REQUEST  = REQUEST,  &
                       IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_ISENDReal0D

END MODULE MPI_ISENDRealModule0D
