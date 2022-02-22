!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                    !
!  This code copyright 2020 by Hixon Technologies, LLC.              !
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

MODULE MPI_SIZEOFInt8Call

  USE IncludeMPIImplementation
  USE MPIKindDefs

! this module actually makes the F77 MPI_SIZEOF call for int8 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPISizeofInt8

CONTAINS

SUBROUTINE MPISizeofInt8(X,SIZE,IERROR)
  INTEGER(KIND=int8Kind), INTENT(IN) :: X
  INTEGER, INTENT(INOUT)  :: SIZE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_SIZEOF(X,SIZE,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPISizeofInt8

END MODULE MPI_SIZEOFInt8Call

MODULE MPI_SIZEOFInt8Module
  USE MPI_SIZEOFInt8Call
  USE MPIKindDefs

! this module is the wrapper to make the new F95 MPI_SIZEOF
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_SIZEOF

INTERFACE HTMPI_SIZEOF
  MODULE PROCEDURE MPI_SIZEOFInt8
END INTERFACE HTMPI_SIZEOF

CONTAINS

SUBROUTINE MPI_SIZEOFInt8(X,SIZE,IERROR)
  INTEGER(KIND=int8Kind), INTENT(IN) :: X
  INTEGER, INTENT(INOUT)  :: SIZE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPISizeofInt8(X      = X,      &
                     SIZE   = SIZE,   &
                     IERROR = IERROR)

  RETURN
END SUBROUTINE MPI_SIZEOFInt8

END MODULE MPI_SIZEOFInt8Module
