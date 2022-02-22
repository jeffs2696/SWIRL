MODULE MPI_GET_ADDRESSCharacterCall

  USE IncludeMPIImplementation

  ! this module actually makes the F77 MPI_GET_ADDRESS call for character data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_GET_ADDRESSCharacter

INTERFACE MPI_GET_ADDRESSCharacter
  MODULE PROCEDURE MPI_GET_ADDRESSCharacter1
END INTERFACE 

CONTAINS

  SUBROUTINE MPI_GET_ADDRESSCharacter1(LOCATION, ADDRESS, IERROR)
    CHARACTER, DIMENSION(:), INTENT(IN)  :: LOCATION
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT)  :: ADDRESS
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_GET_ADDRESS(LOCATION(1), ADDRESS, IERROR)

    IF (IERROR == MPI_SUCCESS) THEN
      IERROR = 0
    END IF

    RETURN
  END SUBROUTINE MPI_GET_ADDRESSCharacter1

END MODULE MPI_GET_ADDRESSCharacterCall

MODULE MPI_GET_ADDRESSCharacterModule
  USE IncludeMPIImplementation
  USE MPI_GET_ADDRESSCharacterCall

  ! this module is the wrapper to make the new F95 MPI_GET_ADDRESS
  !  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GET_ADDRESS

  INTERFACE HTMPI_GET_ADDRESS
    MODULE PROCEDURE MPI_GET_ADDRESSCharacter1
  END INTERFACE

CONTAINS

  SUBROUTINE MPI_GET_ADDRESSCharacter1(LOCATION, ADDRESS, IERROR)
    CHARACTER, DIMENSION(:)  , INTENT(IN)  :: LOCATION
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT)  :: ADDRESS
    INTEGER, INTENT(OUT) :: IERROR

    CALL MPI_GET_ADDRESSCharacter(LOCATION = LOCATION, &
                                  ADDRESS  = ADDRESS,  &
                                  IERROR   = IERROR)

    RETURN
  END SUBROUTINE MPI_GET_ADDRESSCharacter1

END MODULE MPI_GET_ADDRESSCharacterModule
