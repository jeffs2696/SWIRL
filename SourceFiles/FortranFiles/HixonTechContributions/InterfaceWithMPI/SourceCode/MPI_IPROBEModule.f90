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

MODULE MPI_IPROBECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_IPROBE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: ProbeForMPIMessage

CONTAINS

SUBROUTINE ProbeForMPIMessage(SOURCE, TAG, COMM, FLAG, STATUS, IERROR)
  INTEGER, INTENT(IN) :: SOURCE
  INTEGER, INTENT(IN) :: TAG
  INTEGER, INTENT(IN) :: COMM ! handle
  LOGICAL, INTENT(OUT) :: FLAG
! INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS
  INTEGER, DIMENSION(:), INTENT(OUT) :: STATUS
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_IPROBE(SOURCE,TAG,COMM,FLAG,STATUS(1),IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE ProbeForMPIMessage

END MODULE MPI_IPROBECall

MODULE MPI_IPROBEModule
  USE MPI_IPROBECall
  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

! this module is the wrapper to make the new F95 MPI_IPROBE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_IPROBE

CONTAINS

SUBROUTINE HTMPI_IPROBE(SOURCE, TAG, COMM, FLAG, STATUS, IERROR)
  INTEGER, INTENT(IN) :: SOURCE
  INTEGER, INTENT(IN) :: TAG
  INTEGER, INTENT(IN) :: COMM ! handle
  LOGICAL, INTENT(OUT) :: FLAG
  INTEGER, DIMENSION(:), INTENT(OUT) :: STATUS
  INTEGER, INTENT(OUT) :: IERROR

  IF (SIZE(STATUS) /= MPI_STATUS_SIZE) THEN
   IERROR = 121
   RETURN
  END IF

  CALL ProbeForMPIMessage(SOURCE  = SOURCE,  &
                          TAG     = TAG,     &
                          COMM    = COMM,    &
                          FLAG    = FLAG,    &
                          STATUS  = STATUS,  &
                          IERROR  = IERROR)

  RETURN
END SUBROUTINE HTMPI_IPROBE

END MODULE MPI_IPROBEModule
