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

MODULE MPI_COMM_SPLITCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_COMM_SPLIT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: CallMPICommSplit

CONTAINS

SUBROUTINE CallMPICommSplit(COMM,COLOR,KEY,NEWCOMM,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(IN)  :: COLOR 
  INTEGER, INTENT(IN)  :: KEY  
  INTEGER, INTENT(OUT) :: NEWCOMM ! handle  
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_COMM_SPLIT(COMM,COLOR,KEY,NEWCOMM,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE CallMPICommSplit

END MODULE MPI_COMM_SPLITCall

MODULE MPI_COMM_SPLITModule
  USE MPI_COMM_SPLITCall

! this module is the wrapper to make the new F95 MPI_COMM_SPLIT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_COMM_SPLIT

CONTAINS

SUBROUTINE HTMPI_COMM_SPLIT(COMM,COLOR,KEY,NEWCOMM,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(IN)  :: COLOR 
  INTEGER, INTENT(IN)  :: KEY  
  INTEGER, INTENT(OUT) :: NEWCOMM ! handle  
  INTEGER, INTENT(OUT) :: IERROR

  CALL CallMPICommSplit(COMM    = COMM,    &
                        COLOR   = COLOR,   &
                        KEY     = KEY,     &
                        NEWCOMM = NEWCOMM, &
                        IERROR  = IERROR)

  RETURN
END SUBROUTINE HTMPI_COMM_SPLIT

END MODULE MPI_COMM_SPLITModule
