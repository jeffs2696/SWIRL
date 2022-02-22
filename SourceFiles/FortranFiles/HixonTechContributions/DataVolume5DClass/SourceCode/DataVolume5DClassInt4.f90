MODULE DataVolume5DClassInt4

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: DataVolume5DInt4Type,        &
            CreateObject,                &
            DestroyObject,               &
            ReadObjectDataFromFile,      &
            WriteObjectDataToFile,       &
            GetPointerToVolumeData,      &
            RemovePointerToVolumeData,   &
            CheckForPointerToVolumeData, &
            AddVolumeDataToMPIStructureDefinition

  INTEGER, PARAMETER :: int4Def = INT32
  INTEGER(KIND=int4Def), PARAMETER :: tolerance = 0_int4Def, &
                                      undefined = -1001_int4Def

  TYPE(CharacterStringType) :: charStringObject

  CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE DataVolume5DClassInt4: '


TYPE DataVolume5DInt4Type
  PRIVATE
  LOGICAL :: isInitialized          = .FALSE., &
             isAllocated            = .FALSE., &
             hasPointerToVolumeData = .FALSE.
  INTEGER(KIND=int4Def), DIMENSION(:,:,:,:,:), ALLOCATABLE :: volumeData 
END TYPE DataVolume5DInt4Type

include 'DataVolume5DClassInt4Routines.f90'

END MODULE DataVolume5DClassInt4
