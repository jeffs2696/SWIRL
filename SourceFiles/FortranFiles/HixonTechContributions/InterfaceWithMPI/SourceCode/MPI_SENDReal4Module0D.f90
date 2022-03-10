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

MODULE MPI_SENDReal4Call0D

  USE IncludeMPIImplementation
  USE MPIKindDefs

! this module actually makes the F77 MPI_SEND call for real data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPISendReal40D

CONTAINS

SUBROUTINE MPISendReal40D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
  REAL(KIND=real4Kind), INTENT(IN) :: BUF
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
END SUBROUTINE MPISendReal40D

END MODULE MPI_SENDReal4Call0D

MODULE MPI_SENDReal4Module0D
  USE MPI_SENDReal4Call0D
  USE MPIKindDefs

! this module is the wrapper to make the new F95 MPI_SEND
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_SEND

INTERFACE HTMPI_SEND
  MODULE PROCEDURE MPI_SENDReal40D
END INTERFACE HTMPI_SEND

CONTAINS

SUBROUTINE MPI_SENDReal40D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
  REAL(KIND=real4Kind), INTENT(IN) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPISendReal40D(BUF      = BUF,      &
                      COUNT    = COUNT,    &
                      DATATYPE = DATATYPE, &
                      DEST     = DEST,     &
                      TAG      = TAG,      &
                      COMM     = COMM,     &
                      IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_SENDReal40D

END MODULE MPI_SENDReal4Module0D
