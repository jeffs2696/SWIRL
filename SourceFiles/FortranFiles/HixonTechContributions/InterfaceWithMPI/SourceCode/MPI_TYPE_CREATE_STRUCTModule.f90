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

MODULE MPI_TYPE_CREATE_STRUCTCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_TYPE_CREATE_STRUCT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPITypeCreateStruct, &
            MPI_ADDRESS_KIND

CONTAINS

SUBROUTINE MPITypeCreateStruct(COUNT,                  &
                               ARRAY_OF_BLOCKLENGTHS,  &
                               ARRAY_OF_DISPLACEMENTS, &
                               ARRAY_OF_TYPES,         &
                               NEWTYPE,                &
                               IERROR)

  INTEGER, INTENT(IN) :: COUNT
  INTEGER, DIMENSION(:), INTENT(IN) :: ARRAY_OF_BLOCKLENGTHS,  &
                                       ARRAY_OF_TYPES
  INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(:), INTENT(IN)  :: &
                                       ARRAY_OF_DISPLACEMENTS
  INTEGER, INTENT(OUT) :: NEWTYPE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_TYPE_CREATE_STRUCT(COUNT,                     &
                              ARRAY_OF_BLOCKLENGTHS(1),  &
                              ARRAY_OF_DISPLACEMENTS(1), &
                              ARRAY_OF_TYPES(1),         &
                              NEWTYPE,                   &
                              IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPITypeCreateStruct

END MODULE MPI_TYPE_CREATE_STRUCTCall

MODULE MPI_TYPE_CREATE_STRUCTModule
  USE MPI_TYPE_CREATE_STRUCTCall

! this module is the wrapper to make the new F95 MPI_TYPE_CREATE_STRUCT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_TYPE_CREATE_STRUCT

CONTAINS

SUBROUTINE HTMPI_TYPE_CREATE_STRUCT(COUNT,                  &
                                  ARRAY_OF_BLOCKLENGTHS,  &
                                  ARRAY_OF_DISPLACEMENTS, &
                                  ARRAY_OF_TYPES,         &
                                  NEWTYPE,                &
                                  IERROR)

  INTEGER, INTENT(IN) :: COUNT
  INTEGER, DIMENSION(:), INTENT(IN) :: ARRAY_OF_BLOCKLENGTHS,  &
                                       ARRAY_OF_TYPES
  INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(:), INTENT(IN)  :: &
                                       ARRAY_OF_DISPLACEMENTS
  INTEGER, INTENT(OUT) :: NEWTYPE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPITypeCreateStruct(                              &
        COUNT                  = COUNT,                  &
        ARRAY_OF_BLOCKLENGTHS  = ARRAY_OF_BLOCKLENGTHS,  &
        ARRAY_OF_DISPLACEMENTS = ARRAY_OF_DISPLACEMENTS, &
        ARRAY_OF_TYPES         = ARRAY_OF_TYPES,         &
        NEWTYPE                = NEWTYPE,                &
        IERROR                 = IERROR)

  RETURN
END SUBROUTINE HTMPI_TYPE_CREATE_STRUCT

END MODULE MPI_TYPE_CREATE_STRUCTModule
