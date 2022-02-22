MODULE DataVolume5DClassInt8

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: DataVolume5DInt8Type,        &
            CreateObject,                &
            DestroyObject,               &
            ReadObjectDataFromFile,      &
            WriteObjectDataToFile,       &
            GetPointerToVolumeData,      &
            RemovePointerToVolumeData,   &
            CheckForPointerToVolumeData, &
            AddVolumeDataToMPIStructureDefinition

INTEGER, PARAMETER :: int8Def = INT64
INTEGER(KIND=int8Def), PARAMETER :: tolerance = 0_int8Def, &
                                    undefined = -1001_int8Def


  TYPE(CharacterStringType) :: charStringObject

  CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE DataVolume5DClassInt8: '

TYPE DataVolume5DInt8Type
  PRIVATE
  LOGICAL :: isInitialized          = .FALSE., &
             isAllocated            = .FALSE., &
             hasPointerToVolumeData = .FALSE.
  INTEGER(KIND=int8Def), DIMENSION(:,:,:,:,:), ALLOCATABLE :: volumeData 
END TYPE DataVolume5DInt8Type

include 'DataVolume5DClassInt8Routines.f90'

END MODULE DataVolume5DClassInt8
