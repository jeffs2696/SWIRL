SUBROUTINE CreateDataVolume5DRealDPObject5D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(5,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realDPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):, &
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
    'SUBROUTINE CreateDataVolume5DRealDPObject5D (1)'

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

  ALLOCATE(object%realDPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realDPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realDPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realDPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realDPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%dataTypeID             = realDPDataTypeID
  object%realDPData            = .TRUE.
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

END SUBROUTINE CreateDataVolume5DRealDPObject5D

SUBROUTINE CreateDataVolume5DRealDPObject4D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(4,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realDPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):, &
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
    'SUBROUTINE CreateDataVolume5DRealDPObject4D (1)'

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

  ALLOCATE(object%realDPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realDPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realDPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realDPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realDPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%dataTypeID             = realDPDataTypeID
  object%realDPData            = .TRUE.
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

END SUBROUTINE CreateDataVolume5DRealDPObject4D

SUBROUTINE CreateDataVolume5DRealDPObject3D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(3,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realDPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):, &
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
    'SUBROUTINE CreateDataVolume5DRealDPObject3D (1)'

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

  ALLOCATE(object%realDPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realDPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realDPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realDPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realDPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%numberOfDataDimensions = 3
  object%dataTypeID             = realDPDataTypeID
  object%realDPData            = .TRUE.
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

END SUBROUTINE CreateDataVolume5DRealDPObject3D

SUBROUTINE CreateDataVolume5DRealDPObject2D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(2,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realDPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):, &
                                        bufferVolumeDataTotalBounds(2,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j,iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume5DRealDPObject2D (1)'

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

  ALLOCATE(object%realDPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realDPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realDPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realDPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realDPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%dataTypeID             = realDPDataTypeID
  object%realDPData            = .TRUE.
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

END SUBROUTINE CreateDataVolume5DRealDPObject2D

SUBROUTINE CreateDataVolume5DRealDPObject1D(object,                       &
                                             bufferVolumeDataTotalBounds,  &
                                             bufferVolumeDataUpdateBounds, &
                                             bufferVolumeData,             &
                                             nodeMPIDataObject,            &
                                             enableChecking,               &
                                             errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(1,2), INTENT(IN) :: bufferVolumeDataTotalBounds, &
                                         bufferVolumeDataUpdateBounds
  REAL(KIND=realDPDef), DIMENSION(bufferVolumeDataTotalBounds(1,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,j,iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume5DRealDPObject1D (1)'

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

  ALLOCATE(object%realDPDataVolume5D(1), &
            STAT = iError)

  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for realDPDataVolume5D.'
   GO TO 100
  END IF

  CALL CreateObject(object                 = object%realDPDataVolume5D(1), &
                    bufferVolumeDataBounds = bufferVolumeDataTotalBounds,   &
                    bufferVolumeData       = bufferVolumeData,              &
                    nodeMPIDataObject      = nodeMPIDataObject,             &
                    enableChecking         = enableChecking,                &
                    errorInfoObject        = errorInfoObject)

  IF (CheckForLocalError(errorInfoObject)) THEN
   DEALLOCATE(object%realDPDataVolume5D, &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for realDPDataVolume5D.'
    GO TO 100
   END IF

   GO TO 101
  END IF

  object%numberOfDataDimensions = 1
  object%dataTypeID             = realDPDataTypeID
  object%realDPData            = .TRUE.
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

END SUBROUTINE CreateDataVolume5DRealDPObject1D

SUBROUTINE GetPointerToRealDPVolumeData5D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:,:,:,:,:), POINTER :: pointerToVolumeData
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
    'SUBROUTINE GetPointerToRealDPVolumeData5D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL GetPointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE GetPointerToRealDPVolumeData5D

SUBROUTINE GetPointerToRealDPVolumeData4D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:,:,:,:), POINTER :: pointerToVolumeData
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
    'SUBROUTINE GetPointerToRealDPVolumeData4D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL GetPointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE GetPointerToRealDPVolumeData4D

SUBROUTINE GetPointerToRealDPVolumeData3D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:,:,:), POINTER :: pointerToVolumeData
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
    'SUBROUTINE GetPointerToRealDPVolumeData3D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL GetPointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE GetPointerToRealDPVolumeData3D

SUBROUTINE GetPointerToRealDPVolumeData2D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:,:), POINTER :: pointerToVolumeData
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
    'SUBROUTINE GetPointerToRealDPVolumeData2D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL GetPointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE GetPointerToRealDPVolumeData2D

SUBROUTINE GetPointerToRealDPVolumeData1D(object,                  &
                                           pointerToVolumeData,     &
                                           pointerDataTotalBounds,  &
                                           pointerDataUpdateBounds, &
                                           nodeMPIDataObject,       &
                                           enableChecking,          &
                                           errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:), POINTER :: pointerToVolumeData
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
    'SUBROUTINE GetPointerToRealDPVolumeData1D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL GetPointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE GetPointerToRealDPVolumeData1D

SUBROUTINE RemovePointerToRealDPVolumeData5D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:,:,:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealDPVolumeData5D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL RemovePointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE RemovePointerToRealDPVolumeData5D

SUBROUTINE RemovePointerToRealDPVolumeData4D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:,:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealDPVolumeData4D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL RemovePointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE RemovePointerToRealDPVolumeData4D

SUBROUTINE RemovePointerToRealDPVolumeData3D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealDPVolumeData3D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL RemovePointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE RemovePointerToRealDPVolumeData3D

SUBROUTINE RemovePointerToRealDPVolumeData2D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealDPVolumeData2D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL RemovePointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE RemovePointerToRealDPVolumeData2D

SUBROUTINE RemovePointerToRealDPVolumeData1D(object,                 &
                                              pointerToVolumeData,    &
                                              nodeMPIDataObject,      &
                                              enableChecking,         &
                                              errorInfoObject)

  TYPE(DataVolume5DType), INTENT(INOUT) :: object
  REAL(KIND=realDPDef), DIMENSION(:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToRealDPVolumeData1D'

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

   IF (ALLOCATED(object%realDPDataVolume5D)) THEN ! OK
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

  CALL RemovePointerToVolumeData(object               = object%realDPDataVolume5D(1), &
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

END SUBROUTINE RemovePointerToRealDPVolumeData1D

