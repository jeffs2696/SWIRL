MODULE DataVolume5DClassRealDP

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: DataVolume5DRealDPType,      &
            CreateObject,                &
            DestroyObject,               &
            ReadObjectDataFromFile,      &
            WriteObjectDataToFile,       &
            GetPointerToVolumeData,      &
            RemovePointerToVolumeData,   &
            CheckForPointerToVolumeData, &
            AddVolumeDataToMPIStructureDefinition

INTEGER, PARAMETER :: realDPDef = REAL64
REAL(KIND=realDPDef), PARAMETER :: tolerance = 1.0E-12_realDPDef
INTEGER, PARAMETER :: undefined = -1001

  TYPE(CharacterStringType) :: charStringObject

  CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE DataVolume5DClassRealDP: '

TYPE DataVolume5DRealDPType
  PRIVATE
  LOGICAL :: isInitialized          = .FALSE., &
             isAllocated            = .FALSE., &
             hasPointerToVolumeData = .FALSE.
  REAL(KIND=realDPDef), DIMENSION(:,:,:,:,:), ALLOCATABLE :: volumeData
END TYPE DataVolume5DRealDPType

include 'DataVolume5DClassRealDPRoutines.f90'

END MODULE DataVolume5DClassRealDP
