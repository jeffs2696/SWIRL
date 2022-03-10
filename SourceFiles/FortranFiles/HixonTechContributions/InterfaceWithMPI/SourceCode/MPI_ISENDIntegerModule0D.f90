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

MODULE MPI_ISENDIntegerCall0D

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_ISEND call for real4 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIIsendInteger0D

CONTAINS

SUBROUTINE MPIIsendInteger0D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,REQUEST,IERROR)
  INTEGER, INTENT(IN) :: BUF
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
END SUBROUTINE MPIIsendInteger0D

END MODULE MPI_ISENDIntegerCall0D

MODULE MPI_ISENDIntegerModule0D
  USE MPI_ISENDIntegerCall0D

! this module is the wrapper to make the new F95 MPI_ISEND
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_ISEND

INTERFACE MPI_ISEND
  MODULE PROCEDURE MPI_ISENDInteger0D
END INTERFACE MPI_ISEND

CONTAINS

SUBROUTINE MPI_ISENDInteger0D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,REQUEST,IERROR)
  INTEGER, INTENT(IN) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIIsendInteger0D(BUF      = BUF,      &
                          COUNT    = COUNT,    &
                          DATATYPE = DATATYPE, &
                          DEST     = DEST,     &
                          TAG      = TAG,      &
                          COMM     = COMM,     &
                          REQUEST  = REQUEST,  &
                          IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_ISENDInteger0D

END MODULE MPI_ISENDIntegerModule0D
