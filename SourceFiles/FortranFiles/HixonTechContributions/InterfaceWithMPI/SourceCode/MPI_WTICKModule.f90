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

MODULE MPI_WTICKCall

  USE IncludeMPIImplementation, ONLY: MPI_WTICK

! this module actually makes the F77 MPI_WTICK call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIWTick

CONTAINS

DOUBLE PRECISION FUNCTION MPIWTick()

  MPIWTick = MPI_WTICK()

  RETURN
END FUNCTION MPIWTick

END MODULE MPI_WTICKCall

MODULE MPI_WTICKModule
  USE MPI_WTICKCall

! this module is the wrapper to make the new F95 MPI_WTICK
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_WTICK

CONTAINS

DOUBLE PRECISION FUNCTION HTMPI_WTICK()

  HTMPI_WTICK = MPIWTick()

  RETURN
END FUNCTION HTMPI_WTICK

END MODULE MPI_WTICKModule
