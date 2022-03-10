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

MODULE MPI_FINALIZECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_FINALIZE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: StopMPIOnNodes

CONTAINS

SUBROUTINE StopMPIOnNodes(IERROR)
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_FINALIZE(IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE StopMPIOnNodes

END MODULE MPI_FINALIZECall

MODULE MPI_FINALIZEModule
  USE MPI_FINALIZECall

! this module is the wrapper to make the new F95 MPI_FINALIZE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_FINALIZE

CONTAINS

SUBROUTINE HTMPI_FINALIZE(IERROR)
  INTEGER, INTENT(OUT) :: IERROR

  CALL StopMPIOnNodes(IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_FINALIZE

END MODULE MPI_FINALIZEModule
