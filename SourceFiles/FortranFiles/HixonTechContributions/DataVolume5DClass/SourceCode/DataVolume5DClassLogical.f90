MODULE DataVolume5DClassLogical

  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: DataVolume5DLogicalType,        &
            CreateObject,                &
            DestroyObject,               &
            ReadObjectDataFromFile,      &
            WriteObjectDataToFile,       &
            GetPointerToVolumeData,      &
            RemovePointerToVolumeData,   &
            CheckForPointerToVolumeData, &
            AddVolumeDataToMPIStructureDefinition

  INTEGER, PARAMETER :: undefined = -1001

  TYPE(CharacterStringType) :: charStringObject

  CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE DataVolume5DClassLogical: '


TYPE DataVolume5DLogicalType
  PRIVATE
  LOGICAL :: isInitialized          = .FALSE., &
             isAllocated            = .FALSE., &
             hasPointerToVolumeData = .FALSE.
  LOGICAL, DIMENSION(:,:,:,:,:), ALLOCATABLE :: volumeData 
END TYPE DataVolume5DLogicalType

include 'DataVolume5DClassLogicalRoutines.f90'

END MODULE DataVolume5DClassLogical
