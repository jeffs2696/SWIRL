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

MODULE MPI_CART_CREATECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_CART_CREATE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: CreateMPICart

CONTAINS

SUBROUTINE CreateMPICart(COMM_OLD,NDIMS,DIMS,PERIODS,REORDER,COMM_CART,IERROR)
  INTEGER, INTENT(IN)  :: COMM_OLD    ! handle
  INTEGER, INTENT(IN)  :: NDIMS   
  INTEGER, DIMENSION(:), INTENT(IN)  :: DIMS   
  LOGICAL, DIMENSION(:), INTENT(IN)  :: PERIODS   
  LOGICAL, INTENT(IN)  :: REORDER   
  INTEGER, INTENT(OUT) :: COMM_CART ! handle  
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_CART_CREATE(COMM_OLD,NDIMS,DIMS(1),PERIODS(1),REORDER,COMM_CART,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE CreateMPICart

END MODULE MPI_CART_CREATECall

MODULE MPI_CART_CREATEModule
  USE MPI_CART_CREATECall

! this module is the wrapper to make the new F95 MPI_CART_CREATE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_CART_CREATE

CONTAINS

SUBROUTINE HTMPI_CART_CREATE(COMM_OLD,NDIMS,DIMS,PERIODS,REORDER,COMM_CART,IERROR)
  INTEGER, INTENT(IN)  :: COMM_OLD    ! handle
  INTEGER, INTENT(IN)  :: NDIMS   
  INTEGER, DIMENSION(:), INTENT(IN)  :: DIMS   
  LOGICAL, DIMENSION(:), INTENT(IN)  :: PERIODS   
  LOGICAL, INTENT(IN)  :: REORDER   
  INTEGER, INTENT(OUT) :: COMM_CART ! handle  
  INTEGER, INTENT(OUT) :: IERROR

  CALL CreateMPICart(COMM_OLD  = COMM_OLD,  &
                     NDIMS     = NDIMS,     &
                     DIMS      = DIMS,      &
                     PERIODS   = PERIODS,   &
                     REORDER   = REORDER,   &
                     COMM_CART = COMM_CART, &
                     IERROR    = IERROR)

  RETURN
END SUBROUTINE HTMPI_CART_CREATE

END MODULE MPI_CART_CREATEModule
