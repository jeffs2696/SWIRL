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

MODULE MPI_COMM_GET_ATTRCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_COMM_GET_ATTR call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPICommAttr, &
            MPI_ADDRESS_KIND

CONTAINS

SUBROUTINE GetMPICommAttr(COMM,COMM_KEYVAL,ATTRIBUTE_VAL,FLAG,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(IN)  :: COMM_KEYVAL
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: ATTRIBUTE_VAL
  LOGICAL, INTENT(OUT) :: FLAG  
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_COMM_GET_ATTR(COMM,COMM_KEYVAL,ATTRIBUTE_VAL,FLAG,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPICommAttr

END MODULE MPI_COMM_GET_ATTRCall

MODULE MPI_COMM_GET_ATTRModule
  USE MPI_COMM_GET_ATTRCall

! this module is the wrapper to make the new F95 MPI_COMM_GET_ATTR
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_COMM_GET_ATTR

CONTAINS

SUBROUTINE HTMPI_COMM_GET_ATTR(COMM,COMM_KEYVAL,ATTRIBUTE_VAL,FLAG,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(IN)  :: COMM_KEYVAL
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: ATTRIBUTE_VAL
  LOGICAL, INTENT(OUT) :: FLAG  
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPICommAttr(COMM          = COMM,          &
                      COMM_KEYVAL   = COMM_KEYVAL,   &
                      ATTRIBUTE_VAL = ATTRIBUTE_VAL, &
                      FLAG          = FLAG,          &
                      IERROR        = IERROR)

  RETURN
END SUBROUTINE HTMPI_COMM_GET_ATTR

END MODULE MPI_COMM_GET_ATTRModule
