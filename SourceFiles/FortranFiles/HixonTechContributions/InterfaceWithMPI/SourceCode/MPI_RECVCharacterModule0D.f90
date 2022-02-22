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

MODULE MPI_RECVCharacterCall0D

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_RECV call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIRecvCharacter0D

CONTAINS

SUBROUTINE MPIRecvCharacter0D(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  CHARACTER, INTENT(OUT) :: BUF
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
END SUBROUTINE MPIRecvCharacter0D

END MODULE MPI_RECVCharacterCall0D

MODULE MPI_RECVCharacterModule0D
  USE MPI_RECVCharacterCall0D

! this module is the wrapper to make the new F95 MPI_RECV
!  call _look like_ the standard F77 call.

  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_RECV

INTERFACE HTMPI_RECV
  MODULE PROCEDURE MPI_RECVCharacter0D
END INTERFACE HTMPI_RECV

CONTAINS

SUBROUTINE MPI_RECVCharacter0D(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,STATUS,IERROR)
  CHARACTER, INTENT(OUT) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIRecvCharacter0D(BUF      = BUF,      &
                          COUNT    = COUNT,    &
                          DATATYPE = DATATYPE, &
                          SOURCE   = SOURCE,   &
                          TAG      = TAG,      &
                          COMM     = COMM,     &
                          STATUS   = STATUS,   &
                          IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_RECVCharacter0D

END MODULE MPI_RECVCharacterModule0D
