MODULE MPIKindDefs

  USE IncludeMPIImplementation, ONLY: MPI_LOGICAL,   &
                                      MPI_CHARACTER, &
                                      MPI_INTEGER4,  & 
                                      MPI_INTEGER8,  &
                                      MPI_REAL4,     &
                                      MPI_REAL8,     &
                                      MPI_UNDEFINED

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: int4Range,      &
            int8Range,      &
            int4Kind,       &
            int8Kind,       &
            real4Precision, &
            real8Precision, &
            real4Kind,      &
            real8Kind,      &
            GetMPITypeDef

  INTEGER, PARAMETER :: int4Kind = SELECTED_INT_KIND(8), &  
                        int8Kind = SELECTED_INT_KIND(10)

  INTEGER, PARAMETER :: real4Kind = SELECTED_REAL_KIND(6,37), &
                        real8Kind = SELECTED_REAL_KIND(15,307)

  INTEGER(KIND=int4Kind) :: int4Variable = 0_int4Kind
  INTEGER(KIND=int8Kind) :: int8Variable = 0_int8Kind

  REAL(KIND=real4Kind) :: real4Variable = 0.0_real4Kind
  REAL(KIND=real8Kind) :: real8Variable = 0.0_real8Kind

  INTEGER, PARAMETER :: int4Range = RANGE(int4Variable), &
                        int8Range = RANGE(int8Variable)

  INTEGER, PARAMETER :: real4Range = RANGE(real4Variable), &
                        real8Range = RANGE(real8Variable)

  INTEGER, PARAMETER :: real4Precision = PRECISION(real4Variable), &
                        real8Precision = PRECISION(real8Variable)

INTERFACE GetMPITypeDef
  MODULE PROCEDURE GetMPICharacterTypeDef
  MODULE PROCEDURE GetMPIInt4TypeDef
  MODULE PROCEDURE GetMPIInt8TypeDef
  MODULE PROCEDURE GetMPILogicalTypeDef
  MODULE PROCEDURE GetMPIReal4TypeDef
  MODULE PROCEDURE GetMPIReal8TypeDef
END INTERFACE GetMPITypeDef

CONTAINS

INTEGER FUNCTION GetMPICharacterTypeDef(variable)
  CHARACTER, INTENT(IN) :: variable
  
  GetMPICharacterTypeDef = MPI_CHARACTER

  RETURN
  WRITE(6,*) variable
END FUNCTION GetMPICharacterTypeDef 

INTEGER FUNCTION GetMPILogicalTypeDef(variable)
  LOGICAL, INTENT(IN) :: variable
  
  GetMPILogicalTypeDef = MPI_LOGICAL

  RETURN
  WRITE(6,*) variable
END FUNCTION GetMPILogicalTypeDef 

INTEGER FUNCTION GetMPIInt4TypeDef(variable)
  INTEGER(KIND=int4Kind), INTENT(IN) :: variable
  
  INTEGER :: errorTest

  GetMPIInt4TypeDef = MPI_INTEGER4
  errorTest         = 0

! CALL MPI_TYPE_CREATE_F90_INTEGER(int4Range,         &
!                                  GetMPIInt4TypeDef, &
!                                  errorTest)

  IF (errorTest /= 0) THEN
   WRITE(6,*) 'Error found in MPI_TYPE_CREATE_F90_INTEGER call(4): ',errorTest
   STOP
  END IF

  RETURN
  WRITE(6,*) variable
END FUNCTION GetMPIInt4TypeDef 

INTEGER FUNCTION GetMPIInt8TypeDef(variable)
  INTEGER(KIND=int8Kind), INTENT(IN) :: variable
  
  INTEGER :: errorTest

  GetMPIInt8TypeDef = MPI_INTEGER8

! GetMPIInt8TypeDef = 0
  errorTest         = 0

! CALL MPI_TYPE_CREATE_F90_INTEGER(int8Range,         &
!                                  GetMPIInt8TypeDef, &
!                                  errorTest)

  IF (errorTest /= 0) THEN
   WRITE(6,*) 'Error found in MPI_TYPE_CREATE_F90_INTEGER call(8): ',errorTest
   STOP
  END IF

  RETURN
  WRITE(6,*) variable
END FUNCTION GetMPIInt8TypeDef 

INTEGER FUNCTION GetMPIReal4TypeDef(variable)
  REAL(KIND=real4Kind), INTENT(IN) :: variable
  
  INTEGER :: errorTest

  GetMPIReal4TypeDef = MPI_REAL4
  errorTest          = 0

! CALL MPI_TYPE_CREATE_F90_REAL(real4Precision,     &
!                               MPI_UNDEFINED,      &
!                               GetMPIReal4TypeDef, &
!                               errorTest)

  IF (errorTest /= 0) THEN
   WRITE(6,*) 'Error found in MPI_TYPE_CREATE_F90_REAL call(4): ',errorTest
   STOP
  END IF

  RETURN
  WRITE(6,*) variable
END FUNCTION GetMPIReal4TypeDef 

INTEGER FUNCTION GetMPIReal8TypeDef(variable)
  REAL(KIND=real8Kind), INTENT(IN) :: variable
  
  INTEGER :: errorTest

! INTERFACE 
!   SUBROUTINE MPI_TYPE_CREATE_F90_REAL(p,r,newtype,ierror)
!    INTEGER, INTENT(IN) :: p, r
!    INTEGER, INTENT(OUT) :: newtype, &
!                            ierror
!   END SUBROUTINE
! END INTERFACE 

  GetMPIReal8TypeDef = MPI_REAL8
! GetMPIReal8TypeDef = 0
  errorTest          = 0

! CALL MPI_TYPE_CREATE_F90_REAL(p       = real8Precision,     &
!                               r       = MPI_UNDEFINED,      &
!                               newtype = GetMPIReal8TypeDef, &
!                               ierror  = errorTest)

  IF (errorTest /= 0) THEN
   WRITE(6,*) 'Error found in MPI_TYPE_CREATE_F90_REAL call(8): ',errorTest
   STOP
  END IF

  RETURN
  WRITE(6,*) variable
END FUNCTION GetMPIReal8TypeDef 


END MODULE MPIKindDefs
