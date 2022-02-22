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

MODULE MPI_COMM_RANKCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_COMM_RANK call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPICommRank

CONTAINS

SUBROUTINE GetMPICommRank(COMM,RANK,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(OUT) :: RANK    
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_COMM_RANK(COMM,RANK,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPICommRank

END MODULE MPI_COMM_RANKCall

MODULE MPI_COMM_RANKModule
  USE MPI_COMM_RANKCall

! this module is the wrapper to make the new F95 MPI_COMM_RANK
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_COMM_RANK

CONTAINS

SUBROUTINE HTMPI_COMM_RANK(COMM,RANK,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(OUT) :: RANK  
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPICommRank(COMM   = COMM,  &
                      RANK   = RANK,  &
                      IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_COMM_RANK

END MODULE MPI_COMM_RANKModule
