MODULE DataVolume5DClassRealSP

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: DataVolume5DRealSPType,      &
            CreateObject,                &
            DestroyObject,               &
            ReadObjectDataFromFile,      &
            WriteObjectDataToFile,       &
            GetPointerToVolumeData,      &
            RemovePointerToVolumeData,   &
            CheckForPointerToVolumeData, &
            AddVolumeDataToMPIStructureDefinition

INTEGER, PARAMETER :: realSPDef = REAL32
REAL(KIND=realSPDef), PARAMETER :: tolerance = 1.0E-6_realSPDef
INTEGER, PARAMETER :: undefined = -1001

  TYPE(CharacterStringType) :: charStringObject

  CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE DataVolume5DClassRealSP: '

TYPE DataVolume5DRealSPType
  PRIVATE
  LOGICAL :: isInitialized          = .FALSE., &
             isAllocated            = .FALSE., &
             hasPointerToVolumeData = .FALSE.
  REAL(KIND=realSPDef), DIMENSION(:,:,:,:,:), ALLOCATABLE :: volumeData
END TYPE DataVolume5DRealSPType

include 'DataVolume5DClassRealSPRoutines.f90'

END MODULE DataVolume5DClassRealSP
