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

MODULE MPI_COMM_CREATECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_COMM_CREATE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: CreateMPIComm

CONTAINS

SUBROUTINE CreateMPIComm(COMM,GROUP,NEWCOMM,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(IN)  :: GROUP   ! handle
  INTEGER, INTENT(OUT) :: NEWCOMM ! handle  
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_COMM_CREATE(COMM,GROUP,NEWCOMM,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE CreateMPIComm

END MODULE MPI_COMM_CREATECall

MODULE MPI_COMM_CREATEModule
  USE MPI_COMM_CREATECall

! this module is the wrapper to make the new F95 MPI_COMM_CREATE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_COMM_CREATE

CONTAINS

SUBROUTINE HTMPI_COMM_CREATE(COMM,GROUP,NEWCOMM,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(IN)  :: GROUP ! handle
  INTEGER, INTENT(OUT) :: NEWCOMM ! handle  
  INTEGER, INTENT(OUT) :: IERROR

  CALL CreateMPIComm(COMM    = COMM,    &
                     GROUP   = GROUP,   &
                     NEWCOMM = NEWCOMM, &
                     IERROR  = IERROR)

  RETURN
END SUBROUTINE HTMPI_COMM_CREATE

END MODULE MPI_COMM_CREATEModule
