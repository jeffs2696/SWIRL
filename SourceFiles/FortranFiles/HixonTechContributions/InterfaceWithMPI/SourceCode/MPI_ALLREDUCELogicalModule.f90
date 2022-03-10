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

MODULE MPI_ALLREDUCELogicalCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_ALLREDUCE call for Logical data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIAllReduceLogical

CONTAINS

SUBROUTINE MPIAllReduceLogical(SENDBUF,RECVBUF,COUNT,DATATYPE,OP,COMM,IERROR)
  LOGICAL, DIMENSION(:), INTENT(IN)  :: SENDBUF
  LOGICAL, INTENT(OUT) :: RECVBUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: OP
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_ALLREDUCE(SENDBUF(1),RECVBUF,COUNT,DATATYPE,OP,COMM,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIAllReduceLogical

END MODULE MPI_ALLREDUCELogicalCall

MODULE MPI_ALLREDUCELogicalModule
  USE MPI_ALLREDUCELogicalCall

! this module is the wrapper to make the new F95 MPI_ALLREDUCE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ALLREDUCE

INTERFACE HTMPI_ALLREDUCE
  MODULE PROCEDURE MPI_ALLREDUCELogical0D
  MODULE PROCEDURE MPI_ALLREDUCELogical1D
END INTERFACE HTMPI_ALLREDUCE

CONTAINS

SUBROUTINE MPI_ALLREDUCELogical0D(SENDBUF,RECVBUF,COUNT,DATATYPE,OP,COMM,IERROR)
  LOGICAL, DIMENSION(:), INTENT(IN)  :: SENDBUF
  LOGICAL, INTENT(OUT) :: RECVBUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: OP
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIAllReduceLogical(SENDBUF   = SENDBUF,   &
                           RECVBUF   = RECVBUF,   &
                           COUNT     = COUNT,     &
                           DATATYPE  = DATATYPE,  &
                           OP        = OP,        &
                           COMM      = COMM,      &
                           IERROR    = IERROR)

  RETURN
END SUBROUTINE MPI_ALLREDUCELogical0D

SUBROUTINE MPI_ALLREDUCELogical1D(SENDBUF,RECVBUF,COUNT,DATATYPE,OP,COMM,IERROR)
  LOGICAL, DIMENSION(:), INTENT(IN)  :: SENDBUF
  LOGICAL, DIMENSION(:), INTENT(OUT) :: RECVBUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: OP
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

! local variable

  LOGICAL :: RECVBUF0D

  CALL MPIAllReduceLogical(SENDBUF   = SENDBUF,   &
                           RECVBUF   = RECVBUF0D, &
                           COUNT     = COUNT,     &
                           DATATYPE  = DATATYPE,  &
                           OP        = OP,        &
                           COMM      = COMM,      &
                           IERROR    = IERROR)

  RECVBUF(1) = RECVBUF0D                      

  RETURN
END SUBROUTINE MPI_ALLREDUCELogical1D

END MODULE MPI_ALLREDUCELogicalModule
