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

MODULE MPI_CART_SHIFTCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_CART_SHIFT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: ShiftMPICart

CONTAINS

SUBROUTINE ShiftMPICart(COMM,DIRECTION,DISP,RANK_SOURCE,RANK_DEST,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(IN)  :: DIRECTION   
  INTEGER, INTENT(IN)  :: DISP   
  INTEGER, INTENT(OUT) :: RANK_SOURCE 
  INTEGER, INTENT(OUT) :: RANK_DEST 
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_CART_SHIFT(COMM,DIRECTION,DISP,RANK_SOURCE,RANK_DEST,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE ShiftMPICart

END MODULE MPI_CART_SHIFTCall

MODULE MPI_CART_SHIFTModule
  USE MPI_CART_SHIFTCall

! this module is the wrapper to make the new F95 MPI_CART_SHIFT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_CART_SHIFT

CONTAINS

SUBROUTINE HTMPI_CART_SHIFT(COMM,DIRECTION,DISP,RANK_SOURCE,RANK_DEST,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(IN)  :: DIRECTION   
  INTEGER, INTENT(IN)  :: DISP   
  INTEGER, INTENT(OUT) :: RANK_SOURCE 
  INTEGER, INTENT(OUT) :: RANK_DEST 
  INTEGER, INTENT(OUT) :: IERROR

  CALL ShiftMPICart(COMM        = COMM,        &
                    DIRECTION   = DIRECTION,   &
                    DISP        = DISP,        &
                    RANK_SOURCE = RANK_SOURCE, &
                    RANK_DEST   = RANK_DEST,   &
                    IERROR      = IERROR)

  RETURN
END SUBROUTINE HTMPI_CART_SHIFT

END MODULE MPI_CART_SHIFTModule
