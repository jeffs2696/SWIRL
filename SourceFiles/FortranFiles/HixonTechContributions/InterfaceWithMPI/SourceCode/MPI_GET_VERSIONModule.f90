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

MODULE MPI_GET_VERSIONCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_GET_VERSION call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPIVersion

CONTAINS

SUBROUTINE GetMPIVersion(VERSION,SUBVERSION,IERROR)
  INTEGER, INTENT(OUT) :: VERSION   
  INTEGER, INTENT(OUT) :: SUBVERSION    
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_GET_VERSION(VERSION,SUBVERSION,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPIVersion

END MODULE MPI_GET_VERSIONCall

MODULE MPI_GET_VERSIONModule
  USE MPI_GET_VERSIONCall

! this module is the wrapper to make the new F95 MPI_GET_VERSION
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GET_VERSION

CONTAINS

SUBROUTINE HTMPI_GET_VERSION(VERSION,SUBVERSION,IERROR)
  INTEGER, INTENT(OUT) :: VERSION
  INTEGER, INTENT(OUT) :: SUBVERSION  
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPIVersion(VERSION    = VERSION,  &
                     SUBVERSION = SUBVERSION,  &
                     IERROR     = IERROR)

  RETURN
END SUBROUTINE HTMPI_GET_VERSION

END MODULE MPI_GET_VERSIONModule
