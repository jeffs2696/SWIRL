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

MODULE MPI_TYPE_GET_CONTENTSCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_TYPE_GET_CONTENTS call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPITypeGetContents, &
            MPI_ADDRESS_KIND

CONTAINS

SUBROUTINE MPITypeGetContents(DATATYPE,           &
                              MAX_INTEGERS,       &
                              MAX_ADDRESSES,      &
                              MAX_DATATYPES,      &
                              ARRAY_OF_INTEGERS,  &
                              ARRAY_OF_ADDRESSES, &
                              ARRAY_OF_DATATYPES, &
                              IERROR)

  INTEGER, INTENT(IN) :: DATATYPE,      &
                         MAX_INTEGERS,  &
                         MAX_ADDRESSES, &
                         MAX_DATATYPES
  INTEGER, DIMENSION(:) , INTENT(OUT) :: ARRAY_OF_INTEGERS,  &
                                         ARRAY_OF_DATATYPES
  INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(:) , INTENT(OUT) :: ARRAY_OF_ADDRESSES
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_TYPE_GET_CONTENTS(DATATYPE,              &
                             MAX_INTEGERS,          &
                             MAX_ADDRESSES,         &
                             MAX_DATATYPES,         &
                             ARRAY_OF_INTEGERS(1),  &
                             ARRAY_OF_ADDRESSES(1), &
                             ARRAY_OF_DATATYPES(1), &
                             IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPITypeGetContents

END MODULE MPI_TYPE_GET_CONTENTSCall

MODULE MPI_TYPE_GET_CONTENTSModule
  USE MPI_TYPE_GET_CONTENTSCall

! this module is the wrapper to make the new F95 MPI_TYPE_GET_CONTENTS
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_TYPE_GET_CONTENTS

CONTAINS

SUBROUTINE HTMPI_TYPE_GET_CONTENTS(DATATYPE,           &
                                 MAX_INTEGERS,       &
                                 MAX_ADDRESSES,      &
                                 MAX_DATATYPES,      &
                                 ARRAY_OF_INTEGERS,  &
                                 ARRAY_OF_ADDRESSES, &
                                 ARRAY_OF_DATATYPES, &
                                 IERROR)

  INTEGER, INTENT(IN) :: DATATYPE,      &
                         MAX_INTEGERS,  &
                         MAX_ADDRESSES, &
                         MAX_DATATYPES
  INTEGER, DIMENSION(:) , INTENT(OUT) :: ARRAY_OF_INTEGERS,  &
                                         ARRAY_OF_DATATYPES

  INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(:) , INTENT(OUT) :: ARRAY_OF_ADDRESSES

  INTEGER, INTENT(OUT) :: IERROR

  CALL MPITypeGetContents(DATATYPE           = DATATYPE,           &
                          MAX_INTEGERS       = MAX_INTEGERS,       &
                          MAX_ADDRESSES      = MAX_ADDRESSES,      &
                          MAX_DATATYPES      = MAX_DATATYPES,      &
                          ARRAY_OF_INTEGERS  = ARRAY_OF_INTEGERS,  &
                          ARRAY_OF_ADDRESSES = ARRAY_OF_ADDRESSES, &
                          ARRAY_OF_DATATYPES = ARRAY_OF_DATATYPES, &
                          IERROR             = IERROR)

  RETURN
END SUBROUTINE HTMPI_TYPE_GET_CONTENTS

END MODULE MPI_TYPE_GET_CONTENTSModule
