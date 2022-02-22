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

MODULE MPI_IRECVReal8Call0D

  USE IncludeMPIImplementation
  USE MPIKindDefs

! this module actually makes the F77 MPI_IRECV call for real8 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIIrecvReal80D

CONTAINS

SUBROUTINE MPIIrecvReal80D(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)
  REAL(KIND=real8Kind), INTENT(OUT) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_IRECV(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIIrecvReal80D

END MODULE MPI_IRECVReal8Call0D

MODULE MPI_IRECVReal8Module0D
  USE MPI_IRECVReal8Call0D
  USE MPIKindDefs

! this module is the wrapper to make the new F95 MPI_IRECV
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_IRECV

INTERFACE HTMPI_IRECV
  MODULE PROCEDURE MPI_IRECVReal80D
END INTERFACE HTMPI_IRECV

CONTAINS

SUBROUTINE MPI_IRECVReal80D(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,REQUEST,IERROR)
  REAL(KIND=real8Kind), INTENT(OUT) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: SOURCE
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIIrecvReal80D(BUF      = BUF,      &
                       COUNT    = COUNT,    &
                       DATATYPE = DATATYPE, &
                       SOURCE   = SOURCE,   &
                       TAG      = TAG,      &
                       COMM     = COMM,     &
                       REQUEST  = REQUEST,  &
                       IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_IRECVReal80D

END MODULE MPI_IRECVReal8Module0D
