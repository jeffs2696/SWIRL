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

MODULE MPI_FINALIZEDCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_FINALIZED call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: CheckForMPIFinalization

CONTAINS

SUBROUTINE CheckForMPIFinalization(FLAG,IERROR)
  LOGICAL, INTENT(OUT) :: FLAG
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_FINALIZED(FLAG,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE CheckForMPIFinalization

END MODULE MPI_FINALIZEDCall

MODULE MPI_FINALIZEDModule
  USE MPI_FINALIZEDCall

! this module is the wrapper to make the new F95 MPI_FINALIZED
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_FINALIZED

CONTAINS

SUBROUTINE HTMPI_FINALIZED(FLAG,IERROR)
  LOGICAL, INTENT(OUT) :: FLAG
  INTEGER, INTENT(OUT) :: IERROR

  CALL CheckForMPIFinalization(FLAG   = FLAG,  &
                               IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_FINALIZED

END MODULE MPI_FINALIZEDModule
