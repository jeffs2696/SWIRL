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

MODULE MPI_TESTCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_TEST call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: TestForMPIMessage

CONTAINS

SUBROUTINE TestForMPIMessage(REQUEST, FLAG, STATUS, IERROR)
  INTEGER, INTENT(INOUT) :: REQUEST ! handle
  LOGICAL, INTENT(OUT) :: FLAG
! INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS
  INTEGER, DIMENSION(:), INTENT(OUT) :: STATUS
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_TEST(REQUEST,FLAG,STATUS(1),IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE TestForMPIMessage

END MODULE MPI_TESTCall

MODULE MPI_TESTModule
  USE MPI_TESTCall
  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

! this module is the wrapper to make the new F95 MPI_TEST
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_TEST

CONTAINS

SUBROUTINE HTMPI_TEST(REQUEST, FLAG, STATUS, IERROR)
  INTEGER, INTENT(INOUT) :: REQUEST ! handle
  LOGICAL, INTENT(OUT) :: FLAG
  INTEGER, DIMENSION(:), INTENT(OUT) :: STATUS
  INTEGER, INTENT(OUT) :: IERROR

  IF (SIZE(STATUS) /= MPI_STATUS_SIZE) THEN
   IERROR = 121
   RETURN
  END IF

  CALL TestForMPIMessage(REQUEST = REQUEST, &
                         FLAG    = FLAG,    &
                         STATUS  = STATUS,  &
                         IERROR  = IERROR)

  RETURN
END SUBROUTINE HTMPI_TEST

END MODULE MPI_TESTModule
