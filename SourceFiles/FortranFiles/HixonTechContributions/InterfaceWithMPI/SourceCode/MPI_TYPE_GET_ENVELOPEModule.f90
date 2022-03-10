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

MODULE MPI_TYPE_GET_ENVELOPECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_TYPE_GET_ENVELOPE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPITypeGetEnvelope

CONTAINS

SUBROUTINE MPITypeGetEnvelope(DATATYPE,      &
                              NUM_INTEGERS,  &
                              NUM_ADDRESSES, &
                              NUM_DATATYPES, &
                              COMBINER,      &
                              IERROR)

  INTEGER, INTENT(IN) :: DATATYPE
  INTEGER, INTENT(OUT) :: NUM_INTEGERS, NUM_ADDRESSES, NUM_DATATYPES, COMBINER
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_TYPE_GET_ENVELOPE(DATATYPE,      &
                             NUM_INTEGERS,  &
                             NUM_ADDRESSES, &
                             NUM_DATATYPES, &
                             COMBINER,      &
                             IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPITypeGetEnvelope

END MODULE MPI_TYPE_GET_ENVELOPECall

MODULE MPI_TYPE_GET_ENVELOPEModule
  USE MPI_TYPE_GET_ENVELOPECall

! this module is the wrapper to make the new F95 MPI_TYPE_GET_ENVELOPE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_TYPE_GET_ENVELOPE

CONTAINS

SUBROUTINE HTMPI_TYPE_GET_ENVELOPE(DATATYPE,      &
                                 NUM_INTEGERS,  &
                                 NUM_ADDRESSES, &
                                 NUM_DATATYPES, &
                                 COMBINER,      &
                                 IERROR)
  INTEGER, INTENT(IN) :: DATATYPE
  INTEGER, INTENT(OUT) :: NUM_INTEGERS, NUM_ADDRESSES, NUM_DATATYPES, COMBINER
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPITypeGetEnvelope(DATATYPE      = DATATYPE,      &
                          NUM_INTEGERS  = NUM_INTEGERS,  &
                          NUM_ADDRESSES = NUM_ADDRESSES, &
                          NUM_DATATYPES = NUM_DATATYPES, &
                          COMBINER      = COMBINER,      &
                          IERROR        = IERROR)

  RETURN
END SUBROUTINE HTMPI_TYPE_GET_ENVELOPE

END MODULE MPI_TYPE_GET_ENVELOPEModule
