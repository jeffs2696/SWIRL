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

MODULE MPI_WTIMECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_WTIME call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIWTime

CONTAINS

DOUBLE PRECISION FUNCTION MPIWTime()

  MPIWTime = MPI_WTIME()

  RETURN
END FUNCTION MPIWTime

END MODULE MPI_WTIMECall

MODULE MPI_WTIMEModule
  USE MPI_WTIMECall

! this module is the wrapper to make the new F95 MPI_WTIME
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_WTIME

CONTAINS

DOUBLE PRECISION FUNCTION HTMPI_WTIME()

  HTMPI_WTIME = MPIWTime()

  RETURN
END FUNCTION HTMPI_WTIME

END MODULE MPI_WTIMEModule
