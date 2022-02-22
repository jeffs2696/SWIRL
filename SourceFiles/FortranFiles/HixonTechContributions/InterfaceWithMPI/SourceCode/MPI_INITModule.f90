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

MODULE MPI_INITCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_INIT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: StartMPIOnNodes

CONTAINS

SUBROUTINE StartMPIOnNodes(IERROR)
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_INIT(IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE StartMPIOnNodes

END MODULE MPI_INITCall

MODULE MPI_INITModule
  USE MPI_INITCall

! this module is the wrapper to make the new F95 MPI_INIT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_INIT

CONTAINS

SUBROUTINE HTMPI_INIT(IERROR)
  INTEGER, INTENT(OUT) :: IERROR

  CALL StartMPIOnNodes(IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_INIT

END MODULE MPI_INITModule
