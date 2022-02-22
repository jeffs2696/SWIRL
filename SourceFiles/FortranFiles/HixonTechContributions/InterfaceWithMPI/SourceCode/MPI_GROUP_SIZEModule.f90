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

MODULE MPI_GROUP_SIZECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_GROUP_SIZE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPIGroupSize

CONTAINS

SUBROUTINE GetMPIGroupSize(GROUP,SIZE,IERROR)
  INTEGER, INTENT(IN)  :: GROUP    ! handle
  INTEGER, INTENT(OUT) :: SIZE    
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_GROUP_SIZE(GROUP,SIZE,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPIGroupSize

END MODULE MPI_GROUP_SIZECall

MODULE MPI_GROUP_SIZEModule
  USE MPI_GROUP_SIZECall

! this module is the wrapper to make the new F95 MPI_GROUP_SIZE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GROUP_SIZE

CONTAINS

SUBROUTINE HTMPI_GROUP_SIZE(GROUP,SIZE,IERROR)
  INTEGER, INTENT(IN)  :: GROUP  ! handle
  INTEGER, INTENT(OUT) :: SIZE  
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPIGroupSize(GROUP   = GROUP,  &
                       SIZE   = SIZE,  &
                       IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_GROUP_SIZE

END MODULE MPI_GROUP_SIZEModule
