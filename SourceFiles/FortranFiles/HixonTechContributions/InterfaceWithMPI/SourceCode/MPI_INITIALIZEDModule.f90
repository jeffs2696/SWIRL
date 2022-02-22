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

MODULE MPI_INITIALIZEDCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_INITIALIZED call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: CheckForMPIInitialization

CONTAINS

SUBROUTINE CheckForMPIInitialization(FLAG,IERROR)
  LOGICAL, INTENT(OUT) :: FLAG
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_INITIALIZED(FLAG,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE CheckForMPIInitialization

END MODULE MPI_INITIALIZEDCall

MODULE MPI_INITIALIZEDModule
  USE MPI_INITIALIZEDCall

! this module is the wrapper to make the new F95 MPI_INITIALIZED
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_INITIALIZED

CONTAINS

SUBROUTINE HTMPI_INITIALIZED(FLAG,IERROR)
  LOGICAL, INTENT(OUT) :: FLAG
  INTEGER, INTENT(OUT) :: IERROR

  CALL CheckForMPIInitialization(FLAG   = FLAG,  &
                                 IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_INITIALIZED

END MODULE MPI_INITIALIZEDModule
