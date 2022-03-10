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

MODULE MPI_GROUP_RANKCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_GROUP_RANK call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPIGroupRank

CONTAINS

SUBROUTINE GetMPIGroupRank(GROUP,RANK,IERROR)
  INTEGER, INTENT(IN)  :: GROUP    ! handle
  INTEGER, INTENT(OUT) :: RANK    
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_GROUP_RANK(GROUP,RANK,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPIGroupRank

END MODULE MPI_GROUP_RANKCall

MODULE MPI_GROUP_RANKModule
  USE MPI_GROUP_RANKCall

! this module is the wrapper to make the new F95 MPI_GROUP_RANK
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GROUP_RANK

CONTAINS

SUBROUTINE HTMPI_GROUP_RANK(GROUP,RANK,IERROR)
  INTEGER, INTENT(IN)  :: GROUP  ! handle
  INTEGER, INTENT(OUT) :: RANK  
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPIGroupRank(GROUP  = GROUP,  &
                       RANK   = RANK,  &
                       IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_GROUP_RANK

END MODULE MPI_GROUP_RANKModule
