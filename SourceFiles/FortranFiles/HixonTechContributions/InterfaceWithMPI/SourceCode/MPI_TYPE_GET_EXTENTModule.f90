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

MODULE MPI_TYPE_GET_EXTENTCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_TYPE_GET_EXTENT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPITypeGetExtent, &
            MPI_ADDRESS_KIND

CONTAINS

SUBROUTINE MPITypeGetExtent(DATATYPE,LB,EXTENT,IERROR)
  INTEGER, INTENT(IN) :: DATATYPE
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT)  :: LB, EXTENT
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_TYPE_GET_EXTENT(DATATYPE,LB,EXTENT,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPITypeGetExtent

END MODULE MPI_TYPE_GET_EXTENTCall

MODULE MPI_TYPE_GET_EXTENTModule
  USE MPI_TYPE_GET_EXTENTCall

! this module is the wrapper to make the new F95 MPI_TYPE_GET_EXTENT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_TYPE_GET_EXTENT

CONTAINS

SUBROUTINE HTMPI_TYPE_GET_EXTENT(DATATYPE,LB,EXTENT,IERROR)
  INTEGER, INTENT(IN) :: DATATYPE
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT)  :: LB, EXTENT
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPITypeGetExtent(DATATYPE = DATATYPE,  &
                        LB       = LB,        &
                        EXTENT   = EXTENT,    &
                        IERROR   = IERROR)

  RETURN
END SUBROUTINE HTMPI_TYPE_GET_EXTENT

END MODULE MPI_TYPE_GET_EXTENTModule
