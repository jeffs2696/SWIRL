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

MODULE MPI_TYPE_SIZECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_TYPE_SIZE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPITypeSize

CONTAINS

SUBROUTINE MPITypeSize(DATATYPE,SIZE,IERROR)
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(OUT) :: SIZE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_TYPE_SIZE(DATATYPE,SIZE,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPITypeSize

END MODULE MPI_TYPE_SIZECall

MODULE MPI_TYPE_SIZEModule
  USE MPI_TYPE_SIZECall

! this module is the wrapper to make the new F95 MPI_TYPE_SIZE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_TYPE_SIZE

CONTAINS

SUBROUTINE HTMPI_TYPE_SIZE(DATATYPE,SIZE,IERROR)
  INTEGER, INTENT(IN) :: DATATYPE
  INTEGER, INTENT(OUT)  :: SIZE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPITypeSize(DATATYPE = DATATYPE, &
                   SIZE     = SIZE,     &
                   IERROR   = IERROR)

  RETURN
END SUBROUTINE HTMPI_TYPE_SIZE

END MODULE MPI_TYPE_SIZEModule
