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

MODULE MPI_IRECVInt8Call

  USE IncludeMPIImplementation
  USE MPIKindDefs

! this module actually makes the F77 MPI_IRECV call for real4 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIIrecvInt8

CONTAINS

SUBROUTINE MPIIrecvInt8(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)
  INTEGER(KIND=int8Kind), INTENT(OUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_IRECV(BUF(1),COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIIrecvInt8

END MODULE MPI_IRECVInt8Call

MODULE MPI_IRECVInt8Module
  USE MPI_IRECVInt8Call
  USE MPIKindDefs

! this module is the wrapper to make the new F95 MPI_IRECV
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_IRECV

INTERFACE HTMPI_IRECV
  MODULE PROCEDURE MPI_IRECVInt8
END INTERFACE HTMPI_IRECV

CONTAINS

SUBROUTINE MPI_IRECVInt8(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)
  INTEGER(KIND=int8Kind), INTENT(OUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIIrecvInt8(BUF      = BUF,      &
                        COUNT    = COUNT,    &
                        DATATYPE = DATATYPE, &
                        SOURCE   = SOURCE,   &
                        TAG      = TAG,      &
                        COMM     = COMM,     &
                        REQUEST  = REQUEST,  &
                        IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_IRECVInt8

END MODULE MPI_IRECVInt8Module
