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

MODULE MPI_COMM_GROUPCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_COMM_GROUP call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPICommGroup

CONTAINS

SUBROUTINE GetMPICommGroup(COMM,GROUP,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(OUT) :: GROUP   ! handle 
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_COMM_GROUP(COMM,GROUP,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPICommGroup

END MODULE MPI_COMM_GROUPCall

MODULE MPI_COMM_GROUPModule
  USE MPI_COMM_GROUPCall

! this module is the wrapper to make the new F95 MPI_COMM_GROUP
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_COMM_GROUP

CONTAINS

SUBROUTINE HTMPI_COMM_GROUP(COMM,GROUP,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(OUT) :: GROUP ! handle 
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPICommGroup(COMM   = COMM,  &
                       GROUP  = GROUP, &
                       IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_COMM_GROUP

END MODULE MPI_COMM_GROUPModule
