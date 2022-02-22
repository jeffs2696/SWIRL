MODULE DataVolume5DClass

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE ErrorInformationClass
  USE MPI2DataClassNoErrClass
  USE DataVolume5DClassLogical
  USE DataVolume5DClassInt4
  USE DataVolume5DClassInt8
  USE DataVolume5DClassRealSP
  USE DataVolume5DClassRealDP

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: DataVolume5DType,                  &
            CreateObject,                      &
            DestroyObject,                     &
            GetPointerToVolumeData,            &
            RemovePointerToVolumeData,         &
            CheckForPointerToVolumeData,       &
            GetVolumeDataBounds,               &
            AddDataToMPIStructureDefinition,   &
            ReadObjectFromFile,                &
            WriteObjectToFile,                 &
            IsObjectKeyword,                   &
            ObjectIsInitialized

CHARACTER(LEN=80), PARAMETER :: dataVolume5DTypeKeywordCharString =  &
   'DataVolume5DType                                                                '

INTEGER, PARAMETER :: int4Def   = INT32,  & ! for the moment
                      int8Def   = INT64,  & ! for the moment
                      realSPDef = REAL32, & ! for the moment
                      realDPDef = REAL64    ! for the moment

INTEGER, PARAMETER :: int4DataTypeID    =  1, &
                      int8DataTypeID    =  2, &
                      realSPDataTypeID  =  3, & 
                      realDPDataTypeID  =  4, &
                      logicalDataTypeID =  5, &
                      unknownDataTypeID = -1

  TYPE(CharacterStringType) :: charStringObject

  CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE DataVolume5DClass: '

  

TYPE DataVolume5DType
  PRIVATE
  LOGICAL :: isInitialized = .FALSE., &
             int4Data      = .FALSE., &
             int8Data      = .FALSE., &
             realSPData    = .FALSE., &
             realDPData    = .FALSE., &
             logicalData   = .FALSE.
  INTEGER :: dataTypeID             = -1, &
             numberOfDataDimensions = -1                            ! between 1 and 5
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: volumeDataTotalBounds,  & ! (nDim,2) is correct dimension
                                          volumeDataUpdateBounds, &
                                          pointerDataTotalBounds, & ! this is necessary because pointer arrays
                                          pointerDataUpdateBounds   !  always start at 1
!
! trying to limit memory while keeping speed, so using 1D allocatable array
!   (which will be allocated to a length of one) rather than a pointer.
!
  TYPE(DataVolume5DInt4Type), DIMENSION(:), ALLOCATABLE :: int4DataVolume5D
  TYPE(DataVolume5DInt8Type), DIMENSION(:), ALLOCATABLE :: int8DataVolume5D
  TYPE(DataVolume5DRealSPType), DIMENSION(:), ALLOCATABLE :: realSPDataVolume5D
  TYPE(DataVolume5DRealDPType), DIMENSION(:), ALLOCATABLE :: realDPDataVolume5D
  TYPE(DataVolume5DLogicalType), DIMENSION(:), ALLOCATABLE :: logicalDataVolume5D
END TYPE DataVolume5DType

INTERFACE CreateObject
  MODULE PROCEDURE CreateDataVolume5DInt4Object5D
  MODULE PROCEDURE CreateDataVolume5DInt8Object5D
  MODULE PROCEDURE CreateDataVolume5DRealSPObject5D
  MODULE PROCEDURE CreateDataVolume5DRealDPObject5D
  MODULE PROCEDURE CreateDataVolume5DLogicalObject5D
  MODULE PROCEDURE CreateDataVolume5DInt4Object4D
  MODULE PROCEDURE CreateDataVolume5DInt8Object4D
  MODULE PROCEDURE CreateDataVolume5DRealSPObject4D
  MODULE PROCEDURE CreateDataVolume5DRealDPObject4D
  MODULE PROCEDURE CreateDataVolume5DLogicalObject4D
  MODULE PROCEDURE CreateDataVolume5DInt4Object3D
  MODULE PROCEDURE CreateDataVolume5DInt8Object3D
  MODULE PROCEDURE CreateDataVolume5DRealSPObject3D
  MODULE PROCEDURE CreateDataVolume5DRealDPObject3D
  MODULE PROCEDURE CreateDataVolume5DLogicalObject3D
  MODULE PROCEDURE CreateDataVolume5DInt4Object2D
  MODULE PROCEDURE CreateDataVolume5DInt8Object2D
  MODULE PROCEDURE CreateDataVolume5DRealSPObject2D
  MODULE PROCEDURE CreateDataVolume5DRealDPObject2D
  MODULE PROCEDURE CreateDataVolume5DLogicalObject2D
  MODULE PROCEDURE CreateDataVolume5DInt4Object1D
  MODULE PROCEDURE CreateDataVolume5DInt8Object1D
  MODULE PROCEDURE CreateDataVolume5DRealSPObject1D
  MODULE PROCEDURE CreateDataVolume5DRealDPObject1D
  MODULE PROCEDURE CreateDataVolume5DLogicalObject1D
END INTERFACE CreateObject

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyDataVolume5DObject
END INTERFACE DestroyObject

INTERFACE GetPointerToVolumeData
  MODULE PROCEDURE GetPointerToInt4VolumeData5D
  MODULE PROCEDURE GetPointerToInt8VolumeData5D
  MODULE PROCEDURE GetPointerToRealSPVolumeData5D
  MODULE PROCEDURE GetPointerToRealDPVolumeData5D
  MODULE PROCEDURE GetPointerToLogicalVolumeData5D
  MODULE PROCEDURE GetPointerToInt4VolumeData4D
  MODULE PROCEDURE GetPointerToInt8VolumeData4D
  MODULE PROCEDURE GetPointerToRealSPVolumeData4D
  MODULE PROCEDURE GetPointerToRealDPVolumeData4D
  MODULE PROCEDURE GetPointerToLogicalVolumeData4D
  MODULE PROCEDURE GetPointerToInt4VolumeData3D
  MODULE PROCEDURE GetPointerToInt8VolumeData3D
  MODULE PROCEDURE GetPointerToRealSPVolumeData3D
  MODULE PROCEDURE GetPointerToRealDPVolumeData3D
  MODULE PROCEDURE GetPointerToLogicalVolumeData3D
  MODULE PROCEDURE GetPointerToInt4VolumeData2D
  MODULE PROCEDURE GetPointerToInt8VolumeData2D
  MODULE PROCEDURE GetPointerToRealSPVolumeData2D
  MODULE PROCEDURE GetPointerToRealDPVolumeData2D
  MODULE PROCEDURE GetPointerToLogicalVolumeData2D
  MODULE PROCEDURE GetPointerToInt4VolumeData1D
  MODULE PROCEDURE GetPointerToInt8VolumeData1D
  MODULE PROCEDURE GetPointerToRealSPVolumeData1D
  MODULE PROCEDURE GetPointerToRealDPVolumeData1D
  MODULE PROCEDURE GetPointerToLogicalVolumeData1D
END INTERFACE GetPointerToVolumeData

INTERFACE RemovePointerToVolumeData
  MODULE PROCEDURE RemovePointerToInt4VolumeData5D
  MODULE PROCEDURE RemovePointerToInt8VolumeData5D
  MODULE PROCEDURE RemovePointerToRealSPVolumeData5D
  MODULE PROCEDURE RemovePointerToRealDPVolumeData5D
  MODULE PROCEDURE RemovePointerToLogicalVolumeData5D
  MODULE PROCEDURE RemovePointerToInt4VolumeData4D
  MODULE PROCEDURE RemovePointerToInt8VolumeData4D
  MODULE PROCEDURE RemovePointerToRealSPVolumeData4D
  MODULE PROCEDURE RemovePointerToRealDPVolumeData4D
  MODULE PROCEDURE RemovePointerToLogicalVolumeData4D
  MODULE PROCEDURE RemovePointerToInt4VolumeData3D
  MODULE PROCEDURE RemovePointerToInt8VolumeData3D
  MODULE PROCEDURE RemovePointerToRealSPVolumeData3D
  MODULE PROCEDURE RemovePointerToRealDPVolumeData3D
  MODULE PROCEDURE RemovePointerToLogicalVolumeData3D
  MODULE PROCEDURE RemovePointerToInt4VolumeData2D
  MODULE PROCEDURE RemovePointerToInt8VolumeData2D
  MODULE PROCEDURE RemovePointerToRealSPVolumeData2D
  MODULE PROCEDURE RemovePointerToRealDPVolumeData2D
  MODULE PROCEDURE RemovePointerToLogicalVolumeData2D
  MODULE PROCEDURE RemovePointerToInt4VolumeData1D
  MODULE PROCEDURE RemovePointerToInt8VolumeData1D
  MODULE PROCEDURE RemovePointerToRealSPVolumeData1D
  MODULE PROCEDURE RemovePointerToRealDPVolumeData1D
  MODULE PROCEDURE RemovePointerToLogicalVolumeData1D
END INTERFACE RemovePointerToVolumeData

INTERFACE AddDataToMPIStructureDefinition
  MODULE PROCEDURE AddVolumeDataToMPIStructureDefinition1
END INTERFACE AddDataToMPIStructureDefinition

INTERFACE CheckForPointerToVolumeData
  MODULE PROCEDURE CheckForPointerToVolumeData1
END INTERFACE CheckForPointerToVolumeData

INTERFACE GetVolumeDataBounds
  MODULE PROCEDURE GetVolumeDataBounds1
END INTERFACE GetVolumeDataBounds

INTERFACE ReadObjectFromFile
  MODULE PROCEDURE ReadDataVolume5DObjectFromFile
END INTERFACE ReadObjectFromFile

INTERFACE WriteObjectToFile
  MODULE PROCEDURE WriteDataVolume5DObjectToFileV1SV1
END INTERFACE WriteObjectToFile

INTERFACE IsObjectKeyword
 MODULE PROCEDURE IsDataVolume5DObjectTypeKeyword
END INTERFACE 

INTERFACE ObjectIsInitialized
  MODULE PROCEDURE DataVolume5DObjectIsInitialized
END INTERFACE ObjectIsInitialized

CONTAINS

include 'DataVolume5DClassTotInt4Routines.f90'
include 'DataVolume5DClassTotInt8Routines.f90'
include 'DataVolume5DClassTotRealSPRoutines.f90'
include 'DataVolume5DClassTotRealDPRoutines.f90'
include 'DataVolume5DClassTotLogicalRoutines.f90'

SUBROUTINE DestroyDataVolume5DObject(object,            &
                                     nodeMPIDataObject, &
                                     enableChecking,    &
                                     errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyDataVolume5DObject5D'

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101

   END IF

   IF (object%isInitialized) THEN
    CONTINUE ! good
   ELSE
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

! destroy appropriate object

  IF (object%int4Data) THEN
   CALL DestroyObject(object            = object%int4DataVolume5D(1), &
                      nodeMPIDataObject = nodeMPIDataObject,        &
                      enableChecking    = enableChecking,           &
                      errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   END IF

   DEALLOCATE(object%int4DataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for int4DataVolume5D.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE IF (object%int8Data) THEN
   CALL DestroyObject(object            = object%int8DataVolume5D(1), &
                      nodeMPIDataObject = nodeMPIDataObject,        &
                      enableChecking    = enableChecking,           &
                      errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   END IF

   DEALLOCATE(object%int8DataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for int8DataVolume5D.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    GO TO 101
   END IF


  ELSE IF (object%realSPData) THEN
   CALL DestroyObject(object            = object%realSPDataVolume5D(1), &
                      nodeMPIDataObject = nodeMPIDataObject,        &
                      enableChecking    = enableChecking,           &
                      errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   END IF

   DEALLOCATE(object%realSPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realSPDataVolume5D.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE IF (object%realDPData) THEN
   CALL DestroyObject(object            = object%realDPDataVolume5D(1), &
                      nodeMPIDataObject = nodeMPIDataObject,        &
                      enableChecking    = enableChecking,           &
                      errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   END IF

   DEALLOCATE(object%realDPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realDPDataVolume5D.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE IF (object%logicalData) THEN
   CALL DestroyObject(object            = object%logicalDataVolume5D(1), &
                      nodeMPIDataObject = nodeMPIDataObject,             &
                      enableChecking    = enableChecking,                &
                      errorInfoObject   = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   END IF

   DEALLOCATE(object%logicalDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for logicalDataVolume5D.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
! error condition; should never hit this one
   charStringObject%charString = 'could not determine dataVolumeObject data type.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  DEALLOCATE(object%volumeDataTotalBounds,   &
             object%volumeDataUpdateBounds,  &
             object%pointerDataTotalBounds,  &
             object%pointerDataUpdateBounds, &
             STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'DEALLOCATION error for volumeDataBounds.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   GO TO 101
  END IF

  object%numberOfDataDimensions = -1

  object%isInitialized = .FALSE.

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = ' in '//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE DestroyDataVolume5DObject

SUBROUTINE AddVolumeDataToMPIStructureDefinition1(nodeMPIDataObject,   &
                                                  structMessageObject, &
                                                  messageData,         &
                                                  iOrder,              & 
                                                  iStart,              & 
                                                  iEnd,                & 
                                                  deltaI,              & 
                                                  enableChecking,      &
                                                  errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  TYPE(DataVolume5DType), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder,iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddVolumeDataToMPIStructureDefinition'

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101

   END IF

   IF (messageData%isInitialized) THEN
    CONTINUE ! good
   ELSE
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (messageData%dataTypeID == int4DataTypeID) THEN
   CALL AddVolumeDataToMPIStructureDefinition(structMessageObject    = structMessageObject,                &
                                              nodeMPIDataObject      = nodeMPIDataObject,                  &
                                              messageData            = messageData%int4DataVolume5D(1),    &
                                              iOrder                 = iOrder,                             &
                                              iStart                 = iStart,                             &
                                              iEnd                   = iEnd,                               &
                                              deltaI                 = deltaI,                             &
                                              numberOfDataDimensions = messageData%numberOfDataDimensions, &
                                              enableChecking         = enableChecking,                     &
                                              errorInfoObject        = errorInfoObject)

  ELSE IF (messageData%dataTypeID == int8DataTypeID) THEN

   CALL AddVolumeDataToMPIStructureDefinition(structMessageObject    = structMessageObject,                &
                                              nodeMPIDataObject      = nodeMPIDataObject,                  &
                                              messageData            = messageData%int8DataVolume5D(1),    &
                                              iOrder                 = iOrder,                             &
                                              iStart                 = iStart,                             &
                                              iEnd                   = iEnd,                               &
                                              deltaI                 = deltaI,                             &
                                              numberOfDataDimensions = messageData%numberOfDataDimensions, &
                                              enableChecking         = enableChecking,                     &
                                              errorInfoObject        = errorInfoObject)

  ELSE IF (messageData%dataTypeID == realSPDataTypeID) THEN

   CALL AddVolumeDataToMPIStructureDefinition(structMessageObject    = structMessageObject,                &
                                              nodeMPIDataObject      = nodeMPIDataObject,                  &
                                              messageData            = messageData%realSPDataVolume5D(1),  &
                                              iOrder                 = iOrder,                             &
                                              iStart                 = iStart,                             &
                                              iEnd                   = iEnd,                               &
                                              deltaI                 = deltaI,                             &
                                              numberOfDataDimensions = messageData%numberOfDataDimensions, &
                                              enableChecking         = enableChecking,                     &
                                              errorInfoObject        = errorInfoObject)

  ELSE IF (messageData%dataTypeID == realDPDataTypeID) THEN

   CALL AddVolumeDataToMPIStructureDefinition(structMessageObject    = structMessageObject,                &
                                              nodeMPIDataObject      = nodeMPIDataObject,                  &
                                              messageData            = messageData%realDPDataVolume5D(1),  &
                                              iOrder                 = iOrder,                             &
                                              iStart                 = iStart,                             &
                                              iEnd                   = iEnd,                               &
                                              deltaI                 = deltaI,                             &
                                              numberOfDataDimensions = messageData%numberOfDataDimensions, &
                                              enableChecking         = enableChecking,                     &
                                              errorInfoObject        = errorInfoObject)

  ELSE IF (messageData%dataTypeID == logicalDataTypeID) THEN

   CALL AddVolumeDataToMPIStructureDefinition(structMessageObject    = structMessageObject,                &
                                              nodeMPIDataObject      = nodeMPIDataObject,                  &
                                              messageData            = messageData%logicalDataVolume5D(1), &
                                              iOrder                 = iOrder,                             &
                                              iStart                 = iStart,                             &
                                              iEnd                   = iEnd,                               &
                                              deltaI                 = deltaI,                             &
                                              numberOfDataDimensions = messageData%numberOfDataDimensions, &
                                              enableChecking         = enableChecking,                     &
                                              errorInfoObject        = errorInfoObject)

  ELSE
   charStringObject%charString = 'dataVolumeObject data type is unknown.'

   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101

  END IF

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = ' in '//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

  RETURN
END SUBROUTINE AddVolumeDataToMPIStructureDefinition1

SUBROUTINE CheckForPointerToVolumeData1(dataVolumeObject,     &
                                        volumeDataHasPointer, &
                                        enableChecking,       &
                                        errorInfoObject)

  TYPE(DataVolume5DType), INTENT(IN) :: dataVolumeObject
  LOGICAL, INTENT(OUT) :: volumeDataHasPointer
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  CHARACTER(LEN=*), PARAMETER :: location = &
    'LOGICAL FUNCTION CheckForPointerToVolumeData1'

  IF (enableChecking) THEN

   IF (dataVolumeObject%isInitialized) THEN
    CONTINUE ! good
   ELSE
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (dataVolumeObject%dataTypeID == int4DataTypeID) THEN

   CALL CheckForPointerToVolumeData(dataVolumeObject     = dataVolumeObject%int4DataVolume5D(1), &
                                    volumeDataHasPointer = volumeDataHasPointer,                 &
                                    enableChecking       = enableChecking,                       &
                                    errorInfoObject      = errorInfoObject)

  ELSE IF (dataVolumeObject%dataTypeID == int8DataTypeID) THEN

   CALL CheckForPointerToVolumeData(dataVolumeObject     = dataVolumeObject%int8DataVolume5D(1), &
                                    volumeDataHasPointer = volumeDataHasPointer,                 &
                                    enableChecking       = enableChecking,                       &
                                    errorInfoObject      = errorInfoObject)

  ELSE IF (dataVolumeObject%dataTypeID == realSPDataTypeID) THEN

   CALL CheckForPointerToVolumeData(dataVolumeObject     = dataVolumeObject%realSPDataVolume5D(1), &
                                    volumeDataHasPointer = volumeDataHasPointer,                   &
                                    enableChecking       = enableChecking,                         &
                                    errorInfoObject      = errorInfoObject)

  ELSE IF (dataVolumeObject%dataTypeID == realDPDataTypeID) THEN

   CALL CheckForPointerToVolumeData(dataVolumeObject     = dataVolumeObject%realDPDataVolume5D(1), &
                                    volumeDataHasPointer = volumeDataHasPointer,                   &
                                    enableChecking       = enableChecking,                         &
                                    errorInfoObject      = errorInfoObject)

  ELSE IF (dataVolumeObject%dataTypeID == logicalDataTypeID) THEN

   CALL CheckForPointerToVolumeData(dataVolumeObject     = dataVolumeObject%logicalDataVolume5D(1), &
                                    volumeDataHasPointer = volumeDataHasPointer,                    &
                                    enableChecking       = enableChecking,                          &
                                    errorInfoObject      = errorInfoObject)

  ELSE
   charStringObject%charString = 'dataVolumeObject data type is unknown.'

   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101

  END IF

  IF (CheckForLocalError(errorInfoObject)) GO TO 101

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = ' in '//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

  RETURN
END SUBROUTINE CheckForPointerToVolumeData1

SUBROUTINE GetVolumeDataBounds1(dataVolumeObject,       &
                                volumeDataTotalBounds,  &
                                volumeDataUpdateBounds, &
                                enableChecking,         &
                                errorInfoObject)

  TYPE(DataVolume5DType), INTENT(IN) :: dataVolumeObject
  INTEGER, DIMENSION(:,:), INTENT(OUT) :: volumeDataTotalBounds, & ! (nDim,2) is correct dimension
                                          volumeDataUpdateBounds
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: nDim,nBound

  CHARACTER(LEN=*), PARAMETER :: location = &
    'LOGICAL FUNCTION GetVolumeDataBounds1'

  IF (enableChecking) THEN

   IF (dataVolumeObject%isInitialized) THEN
    CONTINUE ! good
   ELSE
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (UBOUND(volumeDataTotalBounds,1) /= dataVolumeObject%numberOfDataDimensions) THEN
    charStringObject%charString = 'UBOUND(volumeDataTotalBounds,1) /= numberOfDataDimensions'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  DO nDim = 1,dataVolumeObject%numberOfDataDimensions
   DO nBound = 1,2
    volumeDataTotalBounds(nDim,nBound)  = dataVolumeObject%volumeDataTotalBounds(nDim,nBound)
    volumeDataUpdateBounds(nDim,nBound) = dataVolumeObject%volumeDataUpdateBounds(nDim,nBound)
   END DO
  END DO

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = ' in '//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

  RETURN
END SUBROUTINE GetVolumeDataBounds1

SUBROUTINE ReadDataVolume5DObjectFromFile(object,            &
                                          fileUnit,          &
                                          fileIsFormatted,   &
                                          nodeMPIDataObject, &
                                          enableChecking,    &
                                          errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: fileUnit
  LOGICAL, INTENT(IN) :: fileIsFormatted
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected, &
             fileIsOpen 
  CHARACTER(LEN=80) :: errorInformation     
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE ReadDataVolume5DObjectFromFile'

  INTEGER :: version, subVersion

  IF (enableChecking) THEN

   INQUIRE(UNIT=fileUnit,OPENED = fileIsOpen)

   WRITE(0,*) 'FileUnit = ',fileUnit
   WRITE(0,*) 'FileIsOpen(x) = ',fileIsOpen

   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101

   END IF

   IF (object%isInitialized) THEN
    charStringObject%charString = 'dataVolumeObject is already initialized.'

    GO TO 100
   ELSE
    CONTINUE
   END IF

   INQUIRE(UNIT=fileUnit,OPENED = fileIsOpen)

   WRITE(0,*) 'FileUnit = ',fileUnit
   WRITE(0,*) 'FileIsOpen = ',fileIsOpen

   IF (.NOT. fileIsOpen) THEN

    charStringObject%charString = 'output file is not open'

    GO TO 100
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

! get the version and subversion data

  IF (fileIsFormatted) THEN
! file format data
   READ(UNIT=fileUnit, &
        FMT = *) version, &
                 subVersion

  ELSE
! file format data
   READ(UNIT=fileUnit) version, &
                       subVersion

  END IF

  IF (version == 1) THEN
   IF (subVersion == 1) THEN
    CALL ReadDataVolume5DObjectFromFileV1SV1(object            = object,            &
                                             fileUnit          = fileUnit,          &
                                             fileIsFormatted   = fileIsFormatted,   &
                                             nodeMPIDataObject = nodeMPIDataObject, &
                                             enableChecking    = enableChecking,    &
                                             errorInfoObject   = errorInfoObject)


   ELSE
    WRITE(charStringObject%charString,'(a,i8)') &
      'Unknown file subVersion: ',subVersion
    GO TO 100
   END IF
  ELSE
   WRITE(charStringObject%charString,'(a,i8)') &
     'Unknown file version: ',version
   GO TO 100
  END IF


  RETURN

 100 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = ' in '//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE ReadDataVolume5DObjectFromFile

SUBROUTINE ReadDataVolume5DObjectFromFileV1SV1(object,            &
                                               fileUnit,          &
                                               fileIsFormatted,   &
                                               nodeMPIDataObject, &
                                               enableChecking,    &
                                               errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: fileUnit
  LOGICAL, INTENT(IN) :: fileIsFormatted
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected, &
             fileIsOpen 
  CHARACTER(LEN=80) :: errorInformation     
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE ReadDataVolume5DObjectFromFileV1SV1'

  INTEGER :: iError
  INTEGER :: i,j

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101

   END IF

   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolumeObject is not initialized.'

    GO TO 100
   END IF

   INQUIRE(UNIT=fileUnit,OPENED = fileIsOpen)

   IF (.NOT. fileIsOpen) THEN

    charStringObject%charString = 'output file is not open'

    GO TO 100
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

! read in the object data

  IF (fileIsFormatted) THEN

! logicals
   READ(UNIT=fileUnit, &
        FMT = *) object%int4Data,   &
                 object%int8Data,   &
                 object%realSPData, &
                 object%realDPData, &
                 object%logicalData
! integers
   READ(UNIT=fileUnit, &
        FMT = *) object%dataTypeID, &
                 object%numberOfDataDimensions
  ELSE
! logicals
   READ(UNIT=fileUnit) object%int4Data,   &
                       object%int8Data,   &
                       object%realSPData, &
                       object%realDPData, &
                       object%logicalData
! integers
   READ(UNIT=fileUnit) object%dataTypeID, &
                       object%numberOfDataDimensions
  END IF

  ALLOCATE(object%volumeDataTotalBounds(4,2),  &
           object%volumeDataUpdateBounds(4,2), &
           STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeDataBounds.'
   GO TO 100
  END IF

! initialize, in case there are fewer than 4 dimensions

  DO j=1,2
   DO i=1,4
    object%volumeDataTotalBounds(i,j) = 1
    object%volumeDataUpdateBounds(i,j) = 1
   END DO
  END DO

! integers
  IF (fileIsFormatted) THEN
   READ(UNIT=fileUnit, &
        FMT = *) object%volumeDataTotalBounds(1:object%numberOfDataDimensions,1:2)
   READ(UNIT=fileUnit, &
        FMT = *) object%volumeDataUpdateBounds(1:object%numberOfDataDimensions,1:2)
  ELSE
   READ(UNIT=fileUnit) object%volumeDataTotalBounds(1:object%numberOfDataDimensions,1:2)
   READ(UNIT=fileUnit) object%volumeDataUpdateBounds(1:object%numberOfDataDimensions,1:2)
  END IF
  
  IF (object%int4Data) THEN
   ALLOCATE(object%int4DataVolume5D(1), &
            STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'ALLOCATION error for volumeData.'
    GO TO 100
   END IF

   CALL ReadObjectDataFromFile(object                  = object%int4DataVolume5D(1),    &
                               bufferVolumeDataBounds  = object%volumeDataTotalBounds,  &
                               fileUnit                = fileUnit,                      &
                               fileIsFormatted         = fileIsFormatted,               &
                               nodeMPIDataObject       = nodeMPIDataObject,             &
                               enableChecking          = enableChecking,                &
                               errorInfoObject         = errorInfoObject)
  ELSE IF (object%int8Data) THEN
   ALLOCATE(object%int8DataVolume5D(1), &
            STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'ALLOCATION error for volumeData.'
    GO TO 100
   END IF

   CALL ReadObjectDataFromFile(object                  = object%int8DataVolume5D(1),    &
                               bufferVolumeDataBounds  = object%volumeDataTotalBounds,  &
                               fileUnit                = fileUnit,                      &
                               fileIsFormatted         = fileIsFormatted,               &
                               nodeMPIDataObject       = nodeMPIDataObject,             &
                               enableChecking          = enableChecking,                &
                               errorInfoObject         = errorInfoObject)
  ELSE IF (object%realSPData) THEN
   ALLOCATE(object%realSPDataVolume5D(1), &
            STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'ALLOCATION error for volumeData.'
    GO TO 100
   END IF

   CALL ReadObjectDataFromFile(object                  = object%realSPDataVolume5D(1),  &
                               bufferVolumeDataBounds  = object%volumeDataTotalBounds,  &
                               fileUnit                = fileUnit,                      &
                               fileIsFormatted         = fileIsFormatted,               &
                               nodeMPIDataObject       = nodeMPIDataObject,             &
                               enableChecking          = enableChecking,                &
                               errorInfoObject         = errorInfoObject)
  ELSE IF (object%realDPData) THEN
   ALLOCATE(object%realDPDataVolume5D(1), &
            STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'ALLOCATION error for volumeData.'
    GO TO 100
   END IF

   CALL ReadObjectDataFromFile(object                  = object%realDPDataVolume5D(1),  &
                               bufferVolumeDataBounds  = object%volumeDataTotalBounds,  &
                               fileUnit                = fileUnit,                      &
                               fileIsFormatted         = fileIsFormatted,               &
                               nodeMPIDataObject       = nodeMPIDataObject,             &
                               enableChecking          = enableChecking,                &
                               errorInfoObject         = errorInfoObject)
  ELSE IF (object%logicalData) THEN
   ALLOCATE(object%logicalDataVolume5D(1), &
            STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'ALLOCATION error for volumeData.'
    GO TO 100
   END IF

   CALL ReadObjectDataFromFile(object                  = object%logicalDataVolume5D(1), &
                               bufferVolumeDataBounds  = object%volumeDataTotalBounds,  &
                               fileUnit                = fileUnit,                      &
                               fileIsFormatted         = fileIsFormatted,               &
                               nodeMPIDataObject       = nodeMPIDataObject,             &
                               enableChecking          = enableChecking,                &
                               errorInfoObject         = errorInfoObject)
  ELSE
   charStringObject%charString = 'No dataVolumes found in the object!'

   GO TO 100
  END IF

  IF (CheckForLocalError(errorInfoObject)) RETURN

  RETURN

 100 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = ' in '//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE ReadDataVolume5DObjectFromFileV1SV1

SUBROUTINE WriteDataVolume5DObjectToFileV1SV1(object,            &
                                              fileUnit,          &
                                              fileIsFormatted,   &
                                              nodeMPIDataObject, &
                                              enableChecking,    &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, INTENT(IN) :: fileUnit
  LOGICAL, INTENT(IN) :: fileIsFormatted
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected, &
             fileIsOpen 
  CHARACTER(LEN=80) :: errorInformation     
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE WriteDataVolume5DObjectToFileV1SV1'

  TYPE(CharacterStringType) :: keywordString
  INTEGER :: version, subVersion

  IF (enableChecking) THEN
   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    WRITE(charStringObject%charString,'(a80)') errorInformation

    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101

   END IF

   IF (object%isInitialized) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolumeObject is not initialized.'

    GO TO 100
   END IF

   INQUIRE(UNIT=fileUnit,OPENED = fileIsOpen)

   IF (.NOT. fileIsOpen) THEN

    charStringObject%charString = 'output file is not open'

    GO TO 100
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  version = 1
  subVersion = 1

! write out the keyword data

  keywordString%charString = dataVolume5DTypeKeywordCharString

  CALL WriteObjectToFile(object          = keywordString, &
                         unitNumber      = fileUnit,      &
                         fileIsFormatted = fileIsFormatted)

! now the object data

  IF (fileIsFormatted) THEN
! file format data
   WRITE(UNIT=fileUnit, &
         FMT = *) version, &
                  subVersion

! logicals
   WRITE(UNIT=fileUnit, &
         FMT = *) object%int4Data,   &
                  object%int8Data,   &
                  object%realSPData, &
                  object%realDPData, &
                  object%logicalData
! integers
   WRITE(UNIT=fileUnit, &
         FMT = *) object%dataTypeID, &
                  object%numberOfDataDimensions
! integers
   WRITE(UNIT=fileUnit, &
         FMT = *) object%volumeDataTotalBounds(1:object%numberOfDataDimensions,1:2)
   WRITE(UNIT=fileUnit, &
         FMT = *) object%volumeDataUpdateBounds(1:object%numberOfDataDimensions,1:2)
  ELSE
! file format data
   WRITE(UNIT=fileUnit) version, &
                        subVersion

! logicals
   WRITE(UNIT=fileUnit) object%int4Data,   &
                        object%int8Data,   &
                        object%realSPData, &
                        object%realDPData, &
                        object%logicalData
! integers
   WRITE(UNIT=fileUnit) object%dataTypeID, &
                        object%numberOfDataDimensions
! integers
   WRITE(UNIT=fileUnit) object%volumeDataTotalBounds(1:object%numberOfDataDimensions,1:2)
   WRITE(UNIT=fileUnit) object%volumeDataUpdateBounds(1:object%numberOfDataDimensions,1:2)

  END IF

  IF (object%int4Data) THEN
   CALL WriteObjectDataToFile(object                 = object%int4DataVolume5D(1),   &
                              bufferVolumeDataBounds = object%volumeDataTotalBounds, &
                              fileUnit               = fileUnit,                     &
                              fileIsFormatted        = fileIsFormatted,              &
                              nodeMPIDataObject      = nodeMPIDataObject,            &
                              enableChecking         = enableChecking,               &
                              errorInfoObject        = errorInfoObject)
  ELSE IF (object%int8Data) THEN
   CALL WriteObjectDataToFile(object                 = object%int8DataVolume5D(1),   &
                              bufferVolumeDataBounds = object%volumeDataTotalBounds, &
                              fileUnit               = fileUnit,                     &
                              fileIsFormatted        = fileIsFormatted,              &
                              nodeMPIDataObject      = nodeMPIDataObject,            &
                              enableChecking         = enableChecking,               &
                              errorInfoObject        = errorInfoObject)
  ELSE IF (object%realSPData) THEN
   CALL WriteObjectDataToFile(object                 = object%realSPDataVolume5D(1), &
                              bufferVolumeDataBounds = object%volumeDataTotalBounds, &
                              fileUnit               = fileUnit,                     &
                              fileIsFormatted        = fileIsFormatted,              &
                              nodeMPIDataObject      = nodeMPIDataObject,            &
                              enableChecking         = enableChecking,               &
                              errorInfoObject        = errorInfoObject)
  ELSE IF (object%realDPData) THEN
   CALL WriteObjectDataToFile(object                 = object%realDPDataVolume5D(1), &
                              bufferVolumeDataBounds = object%volumeDataTotalBounds, &
                              fileUnit               = fileUnit,                     &
                              fileIsFormatted        = fileIsFormatted,              &
                              nodeMPIDataObject      = nodeMPIDataObject,            &
                              enableChecking         = enableChecking,               &
                              errorInfoObject        = errorInfoObject)
  ELSE IF (object%logicalData) THEN
   CALL WriteObjectDataToFile(object                 = object%logicalDataVolume5D(1), &
                              bufferVolumeDataBounds = object%volumeDataTotalBounds,  &
                              fileUnit               = fileUnit,                      &
                              fileIsFormatted        = fileIsFormatted,               &
                              nodeMPIDataObject      = nodeMPIDataObject,             &
                              enableChecking         = enableChecking,                &
                              errorInfoObject        = errorInfoObject)
  ELSE
   charStringObject%charString = 'No dataVolumes found in the object!'

   GO TO 100
  END IF

  IF (CheckForLocalError(errorInfoObject)) RETURN

  RETURN

 100 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = ' in '//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE WriteDataVolume5DObjectToFileV1SV1

FUNCTION IsDataVolume5DObjectTypeKeyword(object,charString)
   LOGICAL :: IsDataVolume5DObjectTypeKeyword
   TYPE(DataVolume5DType), INTENT(IN) :: object
   TYPE(CharacterStringType), INTENT(IN) :: charString

! local -- it pains me that I have to generate this every time!

   TYPE(CharacterStringType) :: objectKeyword

   WRITE(objectKeyword%charString,'(a)') dataVolume5DTypeKeywordCharString

   IsDataVolume5DObjectTypeKeyword = IsKeyword(charString = charString, &
                                               keyword    = objectKeyword)

   RETURN
   IF (object%isInitialized) CONTINUE ! dummy call; object is only here to steer to the correct function
END FUNCTION IsDataVolume5DObjectTypeKeyword

FUNCTION DataVolume5DObjectIsInitialized(object)
   LOGICAL :: DataVolume5DObjectIsInitialized
   TYPE(DataVolume5DType), INTENT(IN) :: object

   DataVolume5DObjectIsInitialized = object%isInitialized

   RETURN
END FUNCTION DataVolume5DObjectIsInitialized


END MODULE DataVolume5DClass
