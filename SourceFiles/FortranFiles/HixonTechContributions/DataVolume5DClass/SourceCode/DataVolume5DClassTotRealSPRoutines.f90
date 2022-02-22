SUBROUTINE CreateDataVolume5DRealSPObject5D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(5,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realSPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):, &
                       bufferVolumeDataTotalBounds(2,1):, &
                       bufferVolumeDataTotalBounds(3,1):, &
                       bufferVolumeDataTotalBounds(4,1):, &
                       bufferVolumeDataTotalBounds(5,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j,iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume5DRealSPObject5D (1)'

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
    charStringObject%charString = 'dataVolumeObject is already initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  object%numberOfDataDimensions = 5

  ALLOCATE(object%volumeDataTotalBounds(5,2),                               &
           object%volumeDataUpdateBounds(5,2),                              &
           object%pointerDataTotalBounds(object%numberOfDataDimensions,2),  &
           object%pointerDataUpdateBounds(object%numberOfDataDimensions,2), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeDataBounds.'
   GO TO 100
  END IF

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%volumeDataTotalBounds(i,j)  = bufferVolumeDataTotalBounds(i,j)
    object%volumeDataUpdateBounds(i,j) = bufferVolumeDataUpdateBounds(i,j)
   END DO
  END DO

  DO j=1,2
   DO i=object%numberOfDataDimensions+1,5
    object%volumeDataTotalBounds(i,j) = 1
    object%volumeDataUpdateBounds(i,j) = 1
   END DO
  END DO

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%pointerDataTotalBounds(i,j) = object%volumeDataTotalBounds(i,j) + 1   &
                                       - object%volumeDataTotalBounds(i,1)
    object%pointerDataUpdateBounds(i,j) = object%volumeDataUpdateBounds(i,j) + 1 &
                                        - object%volumeDataTotalBounds(i,1)
   END DO
  END DO

  ALLOCATE(object%realSPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realSPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realSPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realSPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realSPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%dataTypeID             = realSPDataTypeID
  object%realSPData            = .TRUE.
  object%isInitialized          = .TRUE.

  RETURN

 100 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CreateDataVolume5DRealSPObject5D

SUBROUTINE CreateDataVolume5DRealSPObject4D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(4,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realSPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):, &
                       bufferVolumeDataTotalBounds(2,1):, &
                       bufferVolumeDataTotalBounds(3,1):, &
                       bufferVolumeDataTotalBounds(4,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j,iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume5DRealSPObject4D (1)'

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
    charStringObject%charString = 'dataVolumeObject is already initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  object%numberOfDataDimensions = 4

  ALLOCATE(object%volumeDataTotalBounds(5,2),                               &
           object%volumeDataUpdateBounds(5,2),                              &
           object%pointerDataTotalBounds(object%numberOfDataDimensions,2),  &
           object%pointerDataUpdateBounds(object%numberOfDataDimensions,2), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeDataBounds.'
   GO TO 100
  END IF

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%volumeDataTotalBounds(i,j)  = bufferVolumeDataTotalBounds(i,j)
    object%volumeDataUpdateBounds(i,j) = bufferVolumeDataUpdateBounds(i,j)
   END DO
  END DO

  DO j=1,2
   DO i=object%numberOfDataDimensions+1,5
    object%volumeDataTotalBounds(i,j) = 1
    object%volumeDataUpdateBounds(i,j) = 1
   END DO
  END DO

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%pointerDataTotalBounds(i,j) = object%volumeDataTotalBounds(i,j) + 1   &
                                       - object%volumeDataTotalBounds(i,1)
    object%pointerDataUpdateBounds(i,j) = object%volumeDataUpdateBounds(i,j) + 1 &
                                        - object%volumeDataTotalBounds(i,1)
   END DO
  END DO

  ALLOCATE(object%realSPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realSPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realSPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realSPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realSPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%dataTypeID             = realSPDataTypeID
  object%realSPData            = .TRUE.
  object%isInitialized          = .TRUE.

  RETURN

 100 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CreateDataVolume5DRealSPObject4D

SUBROUTINE CreateDataVolume5DRealSPObject3D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(3,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realSPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):, &
                                        bufferVolumeDataTotalBounds(2,1):, &
                                        bufferVolumeDataTotalBounds(3,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j,iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume5DRealSPObject3D (1)'

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
    charStringObject%charString = 'dataVolumeObject is already initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  object%numberOfDataDimensions = 3

  ALLOCATE(object%volumeDataTotalBounds(5,2),                               &
           object%volumeDataUpdateBounds(5,2),                              &
           object%pointerDataTotalBounds(object%numberOfDataDimensions,2),  &
           object%pointerDataUpdateBounds(object%numberOfDataDimensions,2), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeDataBounds.'
   GO TO 100
  END IF

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%volumeDataTotalBounds(i,j) = bufferVolumeDataTotalBounds(i,j)
    object%volumeDataUpdateBounds(i,j) = bufferVolumeDataUpdateBounds(i,j)
   END DO
  END DO

  DO j=1,2
   DO i=object%numberOfDataDimensions+1,5
    object%volumeDataTotalBounds(i,j)  = 1
    object%volumeDataUpdateBounds(i,j) = 1
   END DO
  END DO

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%pointerDataTotalBounds(i,j) = object%volumeDataTotalBounds(i,j) + 1   &
                                       - object%volumeDataTotalBounds(i,1)
    object%pointerDataUpdateBounds(i,j) = object%volumeDataUpdateBounds(i,j) + 1 &
                                        - object%volumeDataTotalBounds(i,1)
   END DO
  END DO

  ALLOCATE(object%realSPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realSPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realSPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realSPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realSPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%numberOfDataDimensions = 3
  object%dataTypeID             = realSPDataTypeID
  object%realSPData            = .TRUE.
  object%isInitialized          = .TRUE.

  RETURN

 100 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CreateDataVolume5DRealSPObject3D

SUBROUTINE CreateDataVolume5DRealSPObject2D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(2,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realSPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):, &
                                        bufferVolumeDataTotalBounds(2,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j,iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume5DRealSPObject2D (1)'

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
    charStringObject%charString = 'dataVolumeObject is already initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  object%numberOfDataDimensions = 2

  ALLOCATE(object%volumeDataTotalBounds(5,2),                               &
           object%volumeDataUpdateBounds(5,2),                              &
           object%pointerDataTotalBounds(object%numberOfDataDimensions,2),  &
           object%pointerDataUpdateBounds(object%numberOfDataDimensions,2), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeDataBounds.'
   GO TO 100
  END IF

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%volumeDataTotalBounds(i,j)  = bufferVolumeDataTotalBounds(i,j)
    object%volumeDataUpdateBounds(i,j) = bufferVolumeDataUpdateBounds(i,j)
   END DO
  END DO

  DO j=1,2
   DO i=object%numberOfDataDimensions+1,5
    object%volumeDataTotalBounds(i,j)  = 1
    object%volumeDataUpdateBounds(i,j) = 1
   END DO
  END DO

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%pointerDataTotalBounds(i,j) = object%volumeDataTotalBounds(i,j) + 1   &
                                       - object%volumeDataTotalBounds(i,1)
    object%pointerDataUpdateBounds(i,j) = object%volumeDataUpdateBounds(i,j) + 1 &
                                        - object%volumeDataTotalBounds(i,1)
   END DO
  END DO

  ALLOCATE(object%realSPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realSPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realSPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realSPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realSPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%dataTypeID             = realSPDataTypeID
  object%realSPData            = .TRUE.
  object%isInitialized          = .TRUE.

  RETURN

 100 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CreateDataVolume5DRealSPObject2D

SUBROUTINE CreateDataVolume5DRealSPObject1D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(1,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realSPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j,iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume5DRealSPObject1D (1)'

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
    charStringObject%charString = 'dataVolumeObject is already initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  object%numberOfDataDimensions = 1

  ALLOCATE(object%volumeDataTotalBounds(5,2),                               &
           object%volumeDataUpdateBounds(5,2),                              &
           object%pointerDataTotalBounds(object%numberOfDataDimensions,2),  &
           object%pointerDataUpdateBounds(object%numberOfDataDimensions,2), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeDataBounds.'
   GO TO 100
  END IF

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%volumeDataTotalBounds(i,j)  = bufferVolumeDataTotalBounds(i,j)
    object%volumeDataUpdateBounds(i,j) = bufferVolumeDataUpdateBounds(i,j)
   END DO
  END DO

  DO j=1,2
   DO i=object%numberOfDataDimensions+1,5
    object%volumeDataTotalBounds(i,j)  = 1
    object%volumeDataUpdateBounds(i,j) = 1
   END DO
  END DO

  DO j=1,2
   DO i=1,object%numberOfDataDimensions
    object%pointerDataTotalBounds(i,j) = object%volumeDataTotalBounds(i,j) + 1   &
                                       - object%volumeDataTotalBounds(i,1)
    object%pointerDataUpdateBounds(i,j) = object%volumeDataUpdateBounds(i,j) + 1 &
                                        - object%volumeDataTotalBounds(i,1)
   END DO
  END DO

  ALLOCATE(object%realSPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realSPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realSPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realSPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realSPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%numberOfDataDimensions = 1
  object%dataTypeID             = realSPDataTypeID
  object%realSPData            = .TRUE.
  object%isInitialized          = .TRUE.

  RETURN

 100 CONTINUE

  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE CreateDataVolume5DRealSPObject1D

SUBROUTINE GetPointerToRealSPVolumeData5D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:,:,:,:,:), POINTER :: pointerToVolumeData
  INTEGER, DIMENSION(5,2), INTENT(OUT) :: pointerDataTotalBounds, &
                                          pointerDataUpdateBounds
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j
  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToRealSPVolumeData5D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL GetPointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                              pointerToVolumeData  = pointerToVolumeData,           &
                              nodeMPIDataObject    = nodeMPIDataObject,             &
                              enableChecking       = enableChecking,                &
                              errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  DO j=1,2
   DO i=1,5
    pointerDataTotalBounds(i,j)  = object%pointerDataTotalBounds(i,j)
    pointerDataUpdateBounds(i,j) = object%pointerDataUpdateBounds(i,j)
   END DO
  END DO

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetPointerToRealSPVolumeData5D

SUBROUTINE GetPointerToRealSPVolumeData4D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:,:,:,:), POINTER :: pointerToVolumeData
  INTEGER, DIMENSION(4,2), INTENT(OUT) :: pointerDataTotalBounds, &
                                          pointerDataUpdateBounds
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j
  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToRealSPVolumeData4D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL GetPointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                              pointerToVolumeData  = pointerToVolumeData,           &
                              nodeMPIDataObject    = nodeMPIDataObject,             &
                              enableChecking       = enableChecking,                &
                              errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  DO j=1,2
   DO i=1,4
    pointerDataTotalBounds(i,j)  = object%pointerDataTotalBounds(i,j)
    pointerDataUpdateBounds(i,j) = object%pointerDataUpdateBounds(i,j)
   END DO
  END DO

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetPointerToRealSPVolumeData4D

SUBROUTINE GetPointerToRealSPVolumeData3D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:,:,:), POINTER :: pointerToVolumeData
  INTEGER, DIMENSION(3,2), INTENT(OUT) :: pointerDataTotalBounds, &
                                          pointerDataUpdateBounds
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j
  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToRealSPVolumeData3D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL GetPointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                              pointerToVolumeData  = pointerToVolumeData,           &
                              nodeMPIDataObject    = nodeMPIDataObject,             &
                              enableChecking       = enableChecking,                &
                              errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  DO j=1,2
   DO i=1,3
    pointerDataTotalBounds(i,j)  = object%pointerDataTotalBounds(i,j)
    pointerDataUpdateBounds(i,j) = object%pointerDataUpdateBounds(i,j)
   END DO
  END DO

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetPointerToRealSPVolumeData3D

SUBROUTINE GetPointerToRealSPVolumeData2D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:,:), POINTER :: pointerToVolumeData
  INTEGER, DIMENSION(2,2), INTENT(OUT) :: pointerDataTotalBounds, &
                                          pointerDataUpdateBounds
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j
  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToRealSPVolumeData2D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL GetPointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                              pointerToVolumeData  = pointerToVolumeData,           &
                              nodeMPIDataObject    = nodeMPIDataObject,             &
                              enableChecking       = enableChecking,                &
                              errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  DO j=1,2
   DO i=1,2
    pointerDataTotalBounds(i,j)  = object%pointerDataTotalBounds(i,j)
    pointerDataUpdateBounds(i,j) = object%pointerDataUpdateBounds(i,j)
   END DO
  END DO

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetPointerToRealSPVolumeData2D

SUBROUTINE GetPointerToRealSPVolumeData1D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:), POINTER :: pointerToVolumeData
  INTEGER, DIMENSION(1,2), INTENT(OUT) :: pointerDataTotalBounds, &
                                          pointerDataUpdateBounds
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j
  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToRealSPVolumeData1D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL GetPointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                              pointerToVolumeData  = pointerToVolumeData,           &
                              nodeMPIDataObject    = nodeMPIDataObject,             &
                              enableChecking       = enableChecking,                &
                              errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  DO j=1,2
   DO i=1,1
    pointerDataTotalBounds(i,j)  = object%pointerDataTotalBounds(i,j)
    pointerDataUpdateBounds(i,j) = object%pointerDataUpdateBounds(i,j)
   END DO
  END DO

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE GetPointerToRealSPVolumeData1D

SUBROUTINE RemovePointerToRealSPVolumeData5D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:,:,:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealSPVolumeData5D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL RemovePointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                                 pointerToVolumeData  = pointerToVolumeData,           &
                                 nodeMPIDataObject    = nodeMPIDataObject,             &
                                 enableChecking       = enableChecking,                &
                                 errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE RemovePointerToRealSPVolumeData5D

SUBROUTINE RemovePointerToRealSPVolumeData4D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:,:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealSPVolumeData4D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL RemovePointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                                 pointerToVolumeData  = pointerToVolumeData,           &
                                 nodeMPIDataObject    = nodeMPIDataObject,             &
                                 enableChecking       = enableChecking,                &
                                 errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE RemovePointerToRealSPVolumeData4D

SUBROUTINE RemovePointerToRealSPVolumeData3D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealSPVolumeData3D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL RemovePointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                                 pointerToVolumeData  = pointerToVolumeData,           &
                                 nodeMPIDataObject    = nodeMPIDataObject,             &
                                 enableChecking       = enableChecking,                &
                                 errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE RemovePointerToRealSPVolumeData3D

SUBROUTINE RemovePointerToRealSPVolumeData2D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealSPVolumeData2D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL RemovePointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                                 pointerToVolumeData  = pointerToVolumeData,           &
                                 nodeMPIDataObject    = nodeMPIDataObject,             &
                                 enableChecking       = enableChecking,                &
                                 errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE RemovePointerToRealSPVolumeData2D

SUBROUTINE RemovePointerToRealSPVolumeData1D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realSPDef), DIMENSION(:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealSPVolumeData1D'

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

   IF (.NOT.(object%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(object%realSPDataVolume5D)) THEN ! OK
    CONTINUE
   ELSE
    charStringObject%charString = 'volume data is not allocated; incorrect data type?'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  CALL RemovePointerToVolumeData(object               = object%realSPDataVolume5D(1), &
                                 pointerToVolumeData  = pointerToVolumeData,           &
                                 nodeMPIDataObject    = nodeMPIDataObject,             &
                                 enableChecking       = enableChecking,                &
                                 errorInfoObject      = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   GO TO 101
  END IF

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE RemovePointerToRealSPVolumeData1D

