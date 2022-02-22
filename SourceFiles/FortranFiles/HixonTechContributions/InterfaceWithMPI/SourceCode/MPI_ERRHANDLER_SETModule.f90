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

MODULE MPI_ERRHANDLER_SETCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_ERRHANDLER_SET call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: SetMPIErrHandler

CONTAINS

SUBROUTINE SetMPIErrHandler( COMM, OnError, IERROR )
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(IN)  :: OnError ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_ERRHANDLER_SET( COMM, OnError, IERROR )

  IF ( IERROR == MPI_SUCCESS ) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE SetMPIErrHandler

END MODULE MPI_ERRHANDLER_SETCall

MODULE MPI_ERRHANDLER_SETModule
  USE MPI_ERRHANDLER_SETCall

! this module is the wrapper to make the new F95 MPI_ERRHANDLER_SET
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ERRHANDLER_SET

CONTAINS

SUBROUTINE HTMPI_ERRHANDLER_SET( COMM, ERRHANDLER, IERROR )
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(IN)  :: ERRHANDLER  
  INTEGER, INTENT(OUT) :: IERROR

  CALL SetMPIErrHandler( COMM    = COMM,       &
                         OnError = ERRHANDLER, &
                         IERROR  = IERROR )

  RETURN
END SUBROUTINE HTMPI_ERRHANDLER_SET

END MODULE MPI_ERRHANDLER_SETModule
