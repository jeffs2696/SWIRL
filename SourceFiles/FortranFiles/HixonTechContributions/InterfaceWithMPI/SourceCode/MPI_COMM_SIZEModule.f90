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

MODULE MPI_COMM_SIZECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_COMM_SIZE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPICommSize

CONTAINS

SUBROUTINE GetMPICommSize(COMM,SIZE,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(OUT) :: SIZE    
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_COMM_SIZE(COMM,SIZE,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPICommSize

END MODULE MPI_COMM_SIZECall

MODULE MPI_COMM_SIZEModule
  USE MPI_COMM_SIZECall

! this module is the wrapper to make the new F95 MPI_COMM_SIZE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_COMM_SIZE

CONTAINS

SUBROUTINE HTMPI_COMM_SIZE(COMM,SIZE,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(OUT) :: SIZE  
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPICommSize(COMM   = COMM,  &
                      SIZE   = SIZE,  &
                      IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_COMM_SIZE

END MODULE MPI_COMM_SIZEModule
