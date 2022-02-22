INTERFACE CreateObject
  MODULE PROCEDURE CreateDataVolume5DLogicalObject
  MODULE PROCEDURE CreateDataVolume4DLogicalObject
  MODULE PROCEDURE CreateDataVolume3DLogicalObject
  MODULE PROCEDURE CreateDataVolume2DLogicalObject
  MODULE PROCEDURE CreateDataVolume1DLogicalObject
  MODULE PROCEDURE CreateDataVolumeLogicalObjectFromFile
END INTERFACE CreateObject

INTERFACE ReadObjectDataFromFile
  MODULE PROCEDURE CreateDataVolumeLogicalObjectFromFile
END INTERFACE ReadObjectDataFromFile

INTERFACE WriteObjectDataToFile
  MODULE PROCEDURE WriteDataVolumeLogicalObjectDataToFileV1SV1
END INTERFACE WriteObjectDataToFile

INTERFACE DestroyObject
  MODULE PROCEDURE DestroyDataVolume5DLogicalObject
END INTERFACE DestroyObject

INTERFACE GetPointerToVolumeData
  MODULE PROCEDURE GetPointerToVolumeData5D
  MODULE PROCEDURE GetPointerToVolumeData4D
  MODULE PROCEDURE GetPointerToVolumeData3D
  MODULE PROCEDURE GetPointerToVolumeData2D
  MODULE PROCEDURE GetPointerToVolumeData1D
END INTERFACE GetPointerToVolumeData

INTERFACE RemovePointerToVolumeData
  MODULE PROCEDURE RemovePointerToVolumeData5D
  MODULE PROCEDURE RemovePointerToVolumeData4D
  MODULE PROCEDURE RemovePointerToVolumeData3D
  MODULE PROCEDURE RemovePointerToVolumeData2D
  MODULE PROCEDURE RemovePointerToVolumeData1D
END INTERFACE RemovePointerToVolumeData

INTERFACE AddVolumeDataToMPIStructureDefinition
  MODULE PROCEDURE AddVolumeDataToMPIStructureDefinition5D
END INTERFACE AddVolumeDataToMPIStructureDefinition

INTERFACE CheckForPointerToVolumeData
  MODULE PROCEDURE CheckForPointerToVolumeData5D
END INTERFACE CheckForPointerToVolumeData

CONTAINS

SUBROUTINE CreateDataVolume5DLogicalObject(object,                 &
                                           bufferVolumeDataBounds, &
                                           bufferVolumeData,       &
                                           nodeMPIDataObject,      &
                                           enableChecking,         &
                                           errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:,:), INTENT(IN) :: bufferVolumeDataBounds  ! 5,2 is needed
  LOGICAL, DIMENSION(bufferVolumeDataBounds(1,1):, &
                       bufferVolumeDataBounds(2,1):, &
                       bufferVolumeDataBounds(3,1):, &
                       bufferVolumeDataBounds(4,1):, &
                       bufferVolumeDataBounds(5,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,      &
             j,      &
             k,      &
             l,      &
             m,      &
             iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume5DLogicalObject (2)'

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

  ALLOCATE(object%volumeData                  &
           (bufferVolumeDataBounds(1,1)      &
             :bufferVolumeDataBounds(1,2),   &
            bufferVolumeDataBounds(2,1)      &
             :bufferVolumeDataBounds(2,2),   &
            bufferVolumeDataBounds(3,1)      &
             :bufferVolumeDataBounds(3,2),   &
            bufferVolumeDataBounds(4,1)      &
             :bufferVolumeDataBounds(4,2),   &
            bufferVolumeDataBounds(5,1)      &
             :bufferVolumeDataBounds(5,2)),  &
           STAT = iError)
 
  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeData.'
   GO TO 100
  END IF

  object%isAllocated = .TRUE.
  object%hasPointerToVolumeData = .FALSE.

! put in the interior volume data

  DO m=bufferVolumeDataBounds(5,1),     &
       bufferVolumeDataBounds(5,2)

   DO l=bufferVolumeDataBounds(4,1),     &
        bufferVolumeDataBounds(4,2)

    DO k=bufferVolumeDataBounds(3,1),     &
         bufferVolumeDataBounds(3,2)

     DO j=bufferVolumeDataBounds(2,1),     &
          bufferVolumeDataBounds(2,2)

      DO i=bufferVolumeDataBounds(1,1),     &
           bufferVolumeDataBounds(1,2)

       object%volumeData(i,j,k,l,m) = bufferVolumeData(i,j,k,l,m)
  
      END DO
     END DO
    END DO
   END DO
  END DO

  object%isInitialized = .TRUE.

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

END SUBROUTINE CreateDataVolume5DLogicalObject

SUBROUTINE CreateDataVolume4DLogicalObject(object,                 &
                                           bufferVolumeDataBounds, &
                                           bufferVolumeData,       &
                                           nodeMPIDataObject,      &
                                           enableChecking,         &
                                           errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(:,:), INTENT(IN) :: bufferVolumeDataBounds  ! 4,2 is needed
  LOGICAL, DIMENSION(bufferVolumeDataBounds(1,1):, &
                       bufferVolumeDataBounds(2,1):, &
                       bufferVolumeDataBounds(3,1):, &
                       bufferVolumeDataBounds(4,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,      &
             j,      &
             k,      &
             l,      &
             m,      &
             iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume4DLogicalObject (2)'

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

  ALLOCATE(object%volumeData                  &
           (bufferVolumeDataBounds(1,1)      &
             :bufferVolumeDataBounds(1,2),   &
            bufferVolumeDataBounds(2,1)      &
             :bufferVolumeDataBounds(2,2),   &
            bufferVolumeDataBounds(3,1)      &
             :bufferVolumeDataBounds(3,2),   &
            bufferVolumeDataBounds(4,1)      &
             :bufferVolumeDataBounds(4,2),   &
            1),                              &
           STAT = iError)
 
  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeData.'
   GO TO 100
  END IF

  object%isAllocated = .TRUE.
  object%hasPointerToVolumeData = .FALSE.

! put in the interior volume data

  DO m = 1,1

   DO l=bufferVolumeDataBounds(4,1),     &
        bufferVolumeDataBounds(4,2)

    DO k=bufferVolumeDataBounds(3,1),     &
         bufferVolumeDataBounds(3,2)

     DO j=bufferVolumeDataBounds(2,1),     &
          bufferVolumeDataBounds(2,2)

      DO i=bufferVolumeDataBounds(1,1),     &
           bufferVolumeDataBounds(1,2)

       object%volumeData(i,j,k,l,m) = bufferVolumeData(i,j,k,l)
  
      END DO
     END DO
    END DO
   END DO
  END DO

  object%isInitialized = .TRUE.

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

END SUBROUTINE CreateDataVolume4DLogicalObject

SUBROUTINE CreateDataVolume3DLogicalObject(object,                 &
                                           bufferVolumeDataBounds, &
                                           bufferVolumeData,     &
                                           nodeMPIDataObject,      &
                                           enableChecking,         &
                                           errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(3,2), INTENT(IN) :: bufferVolumeDataBounds
  LOGICAL, DIMENSION(bufferVolumeDataBounds(1,1):, &
                                        bufferVolumeDataBounds(2,1):, &
                                        bufferVolumeDataBounds(3,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,      &
             j,      &
             k,      &
             l,      &
             m,      &
             iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume3DLogicalObject (2)'

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

  ALLOCATE(object%volumeData                 &
           (bufferVolumeDataBounds(1,1)      &
             :bufferVolumeDataBounds(1,2),   &
            bufferVolumeDataBounds(2,1)      &
             :bufferVolumeDataBounds(2,2),   &
            bufferVolumeDataBounds(3,1)      &
             :bufferVolumeDataBounds(3,2),   &
            1,                               &
            1),                              &
           STAT = iError)
 
  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeData.'
   GO TO 100
  END IF

  object%isAllocated = .TRUE.
  object%hasPointerToVolumeData = .FALSE.

! put in the interior volume data

  DO m=1,1

   DO l=1,1

    DO k=bufferVolumeDataBounds(3,1),     &
         bufferVolumeDataBounds(3,2)

     DO j=bufferVolumeDataBounds(2,1),     &
          bufferVolumeDataBounds(2,2)

      DO i=bufferVolumeDataBounds(1,1),     &
           bufferVolumeDataBounds(1,2)

       object%volumeData(i,j,k,l,m) = bufferVolumeData(i,j,k)
 
      END DO
     END DO
    END DO
   END DO
  END DO

  object%isInitialized = .TRUE.

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

END SUBROUTINE CreateDataVolume3DLogicalObject

SUBROUTINE CreateDataVolume2DLogicalObject(object,                 &
                                           bufferVolumeDataBounds, &
                                           bufferVolumeData,     &
                                           nodeMPIDataObject,      &
                                           enableChecking,         &
                                           errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(2,2), INTENT(IN) :: bufferVolumeDataBounds
  LOGICAL, DIMENSION(bufferVolumeDataBounds(1,1):, &
                                        bufferVolumeDataBounds(2,1):), INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,      &
             j,      &
             k,      &
             l,      &
             m,      &
             iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume2DLogicalObject (2)'

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

  ALLOCATE(object%volumeData                  &
           (bufferVolumeDataBounds(1,1)      &
             :bufferVolumeDataBounds(1,2),   &
            bufferVolumeDataBounds(2,1)      &
             :bufferVolumeDataBounds(2,2),   &
            1,1,1),                                              &
           STAT = iError)
 
  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeData.'
   GO TO 100
  END IF

  object%isAllocated = .TRUE.
  object%hasPointerToVolumeData = .FALSE.

! put in the interior volume data

  DO m=1,1
   DO l=1,1
    DO k=1,1

     DO j=bufferVolumeDataBounds(2,1),     &
          bufferVolumeDataBounds(2,2)

      DO i=bufferVolumeDataBounds(1,1),     &
           bufferVolumeDataBounds(1,2)

       object%volumeData(i,j,k,l,m) = bufferVolumeData(i,j)
 
      END DO
     END DO
    END DO
   END DO
  END DO

  object%isInitialized = .TRUE.

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

END SUBROUTINE CreateDataVolume2DLogicalObject

SUBROUTINE CreateDataVolume1DLogicalObject(object,                 &
                                           bufferVolumeDataBounds, &
                                           bufferVolumeData,     &
                                           nodeMPIDataObject,      &
                                           enableChecking,         &
                                           errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(1,2), INTENT(IN) :: bufferVolumeDataBounds
  LOGICAL, DIMENSION(bufferVolumeDataBounds(1,1):),  &
                                INTENT(IN) :: bufferVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,      &
             j,      &
             k,      &
             l,      &
             m,      &
             iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolume1DLogicalObject (2)'

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

  ALLOCATE(object%volumeData                  &
           (bufferVolumeDataBounds(1,1)      &
             :bufferVolumeDataBounds(1,2),   &
            1,1,1,1),                                            &
           STAT = iError)
 
  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeData.'
   GO TO 100
  END IF

  object%isAllocated = .TRUE.
  object%hasPointerToVolumeData = .FALSE.

! put in the interior volume data

  DO m=1,1
   DO l=1,1
    DO k=1,1
     DO j=1,1

      DO i=bufferVolumeDataBounds(1,1),     &
           bufferVolumeDataBounds(1,2)

       object%volumeData(i,j,k,l,m) = bufferVolumeData(i)
  
      END DO
     END DO
    END DO
   END DO
  END DO

  object%isInitialized = .TRUE.

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

END SUBROUTINE CreateDataVolume1DLogicalObject

SUBROUTINE CreateDataVolumeLogicalObjectFromFile(object,                 &
                                                 bufferVolumeDataBounds, &
                                                 fileUnit,               &
                                                 fileIsFormatted,        &
                                                 nodeMPIDataObject,      &
                                                 enableChecking,         &
                                                 errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(5,2), INTENT(IN) :: bufferVolumeDataBounds
  INTEGER, INTENT(IN) :: fileUnit
  LOGICAL, INTENT(IN) :: fileIsFormatted
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,      &
             j,      &
             k,      &
             l,      &
             m,      &
             iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateDataVolumeLogicalObjectFromFile (2)'

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

  ALLOCATE(object%volumeData                 &
           (bufferVolumeDataBounds(1,1)      &
             :bufferVolumeDataBounds(1,2),   &
            bufferVolumeDataBounds(2,1)      &
             :bufferVolumeDataBounds(2,2),   &
            bufferVolumeDataBounds(3,1)      &
             :bufferVolumeDataBounds(3,2),   &
            bufferVolumeDataBounds(4,1)      &
             :bufferVolumeDataBounds(4,2),   &
            bufferVolumeDataBounds(5,1)      &
             :bufferVolumeDataBounds(5,2)),  &
           STAT = iError)
 
  IF (iError /= 0) THEN
   charStringObject%charString = 'ALLOCATION error for volumeData.'
   GO TO 100
  END IF

  object%isAllocated = .TRUE.
  object%hasPointerToVolumeData = .FALSE.

! put in the interior volume data

  IF (fileIsFormatted) THEN
   READ(fileUnit,*) (((((object%volumeData(i,j,k,l,m), &
                        i=bufferVolumeDataBounds(1,1),bufferVolumeDataBounds(1,2)), &
                        j=bufferVolumeDataBounds(2,1),bufferVolumeDataBounds(2,2)), &
                        k=bufferVolumeDataBounds(3,1),bufferVolumeDataBounds(3,2)), &
                        l=bufferVolumeDataBounds(4,1),bufferVolumeDataBounds(4,2)), &
                        m=bufferVolumeDataBounds(5,1),bufferVolumeDataBounds(5,2))
  ELSE
   READ(fileUnit) (((((object%volumeData(i,j,k,l,m), &
                      i=bufferVolumeDataBounds(1,1),bufferVolumeDataBounds(1,2)), &
                      j=bufferVolumeDataBounds(2,1),bufferVolumeDataBounds(2,2)), &
                      k=bufferVolumeDataBounds(3,1),bufferVolumeDataBounds(3,2)), &
                      l=bufferVolumeDataBounds(4,1),bufferVolumeDataBounds(4,2)), &
                      m=bufferVolumeDataBounds(5,1),bufferVolumeDataBounds(5,2))
  END IF

  object%isInitialized = .TRUE.

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

END SUBROUTINE CreateDataVolumeLogicalObjectFromFile

SUBROUTINE WriteDataVolumeLogicalObjectDataToFileV1SV1(object,                 &
                                                       bufferVolumeDataBounds, &
                                                       fileUnit,               &
                                                       fileIsFormatted,        &
                                                       nodeMPIDataObject,      &
                                                       enableChecking,         &
                                                       errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(INOUT) :: object
  INTEGER, DIMENSION(5,2), INTENT(IN) :: bufferVolumeDataBounds
  INTEGER, INTENT(IN) :: fileUnit
  LOGICAL, INTENT(IN) :: fileIsFormatted
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: i,      &
             j,      &
             k,      &
             l,      &
             m

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE WriteDataVolumeLogicalObjectDataToFile (2)'

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
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

! write out the interior volume data

  IF (fileIsFormatted) THEN
   WRITE(fileUnit,*) (((((object%volumeData(i,j,k,l,m), &
                         i=bufferVolumeDataBounds(1,1),bufferVolumeDataBounds(1,2)), &
                         j=bufferVolumeDataBounds(2,1),bufferVolumeDataBounds(2,2)), &
                         k=bufferVolumeDataBounds(3,1),bufferVolumeDataBounds(3,2)), &
                         l=bufferVolumeDataBounds(4,1),bufferVolumeDataBounds(4,2)), &
                         m=bufferVolumeDataBounds(5,1),bufferVolumeDataBounds(5,2))
  ELSE
   WRITE(fileUnit) (((((object%volumeData(i,j,k,l,m), &
                       i=bufferVolumeDataBounds(1,1),bufferVolumeDataBounds(1,2)), &
                       j=bufferVolumeDataBounds(2,1),bufferVolumeDataBounds(2,2)), &
                       k=bufferVolumeDataBounds(3,1),bufferVolumeDataBounds(3,2)), &
                       l=bufferVolumeDataBounds(4,1),bufferVolumeDataBounds(4,2)), &
                       m=bufferVolumeDataBounds(5,1),bufferVolumeDataBounds(5,2))
  END IF

  RETURN
  GO TO 100 ! just to keep label 100

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

END SUBROUTINE WriteDataVolumeLogicalObjectDataToFileV1SV1

SUBROUTINE DestroyDataVolume5DLogicalObject(object,            &
                                            nodeMPIDataObject, &
                                            enableChecking,    &
                                            errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(INOUT) :: object
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER :: iError

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE DestroyDataVolume5DLogicalObject'

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
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (object%hasPointerToVolumeData) THEN
    charStringObject%charString = 'dataVolumeObject bufferVolume still has a pointer to it.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%isAllocated) THEN

   IF (object%hasPointerToVolumeData) THEN
    charStringObject%charString = 'dataVolumeObject bufferVolume still has a pointer to it.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)
   ELSE
    CONTINUE
   END IF

   DEALLOCATE(object%volumeData,               &
              STAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'DEALLOCATION error for volumeData.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

   ELSE 
    CONTINUE
   END IF
   object%isAllocated = .FALSE.
  END IF

  object%isInitialized = .FALSE.

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE DestroyDataVolume5DLogicalObject

SUBROUTINE GetPointerToVolumeData5D(object,              &
                                    pointerToVolumeData, &
                                    nodeMPIDataObject,   &
                                    enableChecking,      &
                                    errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:,:,:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToVolumeData5D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (ASSOCIATED(pointerToVolumeData)) THEN
    charStringObject%charString = 'Pointer is already associated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   charStringObject%charString = 'There is already a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  ELSE
   CONTINUE
  END IF

  pointerToVolumeData => object%volumeData
  object%hasPointerToVolumeData = .TRUE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE GetPointerToVolumeData5D

SUBROUTINE GetPointerToVolumeData4D(object,              &
                                    pointerToVolumeData, &
                                    nodeMPIDataObject,   &
                                    enableChecking,      &
                                    errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:,:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToVolumeData4D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (ASSOCIATED(pointerToVolumeData)) THEN
    charStringObject%charString = 'Pointer is already associated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   charStringObject%charString = 'There is already a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  ELSE
   CONTINUE
  END IF

  pointerToVolumeData => object%volumeData(:,:,:,:,1)
  object%hasPointerToVolumeData = .TRUE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE GetPointerToVolumeData4D

SUBROUTINE GetPointerToVolumeData3D(object,              &
                                    pointerToVolumeData, &
                                    nodeMPIDataObject,   &
                                    enableChecking,      &
                                    errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToVolumeData3D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (ASSOCIATED(pointerToVolumeData)) THEN
    charStringObject%charString = 'Pointer is already associated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   charStringObject%charString = 'There is already a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  ELSE
   CONTINUE
  END IF

  pointerToVolumeData => object%volumeData(:,:,:,1,1)
  object%hasPointerToVolumeData = .TRUE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE GetPointerToVolumeData3D

SUBROUTINE GetPointerToVolumeData2D(object,              &
                                    pointerToVolumeData, &
                                    nodeMPIDataObject,   &
                                    enableChecking,      &
                                    errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToVolumeData2D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (ASSOCIATED(pointerToVolumeData)) THEN
    charStringObject%charString = 'Pointer is already associated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   charStringObject%charString = 'There is already a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  ELSE
   CONTINUE
  END IF

  pointerToVolumeData => object%volumeData(:,:,1,1,1)
  object%hasPointerToVolumeData = .TRUE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE GetPointerToVolumeData2D

SUBROUTINE GetPointerToVolumeData1D(object,               &
                                    pointerToVolumeData,  &
                                    nodeMPIDataObject,    &
                                    enableChecking,       &
                                    errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE GetPointerToVolumeData1D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (ASSOCIATED(pointerToVolumeData)) THEN
    charStringObject%charString = 'Pointer is already associated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   charStringObject%charString = 'There is already a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  ELSE
   CONTINUE
  END IF

  pointerToVolumeData => object%volumeData(:,1,1,1,1)
  object%hasPointerToVolumeData = .TRUE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE GetPointerToVolumeData1D

SUBROUTINE RemovePointerToVolumeData5D(object,              &
                                       pointerToVolumeData, &
                                       nodeMPIDataObject,   &
                                       enableChecking,      &
                                       errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:,:,:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToVolumeData5D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (.NOT.(ASSOCIATED(pointerToVolumeData, &
                        object%volumeData))) THEN
    charStringObject%charString = 'Pointer is not associated with this volumeData.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   CONTINUE
  ELSE
   charStringObject%charString = 'There is not a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  NULLIFY(pointerToVolumeData)
  object%hasPointerToVolumeData = .FALSE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE RemovePointerToVolumeData5D

SUBROUTINE RemovePointerToVolumeData4D(object,              &
                                       pointerToVolumeData, &
                                       nodeMPIDataObject,   &
                                       enableChecking,      &
                                       errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:,:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToVolumeData4D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (.NOT.(ASSOCIATED(pointerToVolumeData, &
                        object%volumeData(:,:,:,:,1)))) THEN
    charStringObject%charString = 'Pointer is not associated with this volumeData.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   CONTINUE
  ELSE
   charStringObject%charString = 'There is not a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  NULLIFY(pointerToVolumeData)
  object%hasPointerToVolumeData = .FALSE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE RemovePointerToVolumeData4D

SUBROUTINE RemovePointerToVolumeData3D(object,              &
                                       pointerToVolumeData, &
                                       nodeMPIDataObject,   &
                                       enableChecking,      &
                                       errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:,:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToVolumeData3D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (.NOT.(ASSOCIATED(pointerToVolumeData, &
                        object%volumeData(:,:,:,1,1)))) THEN
    charStringObject%charString = 'Pointer is not associated with this volumeData.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   CONTINUE
  ELSE
   charStringObject%charString = 'There is not a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  NULLIFY(pointerToVolumeData)
  object%hasPointerToVolumeData = .FALSE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE RemovePointerToVolumeData3D

SUBROUTINE RemovePointerToVolumeData2D(object,              &
                                       pointerToVolumeData, &
                                       nodeMPIDataObject,   &
                                       enableChecking,      &
                                       errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:,:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToVolumeData2D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (.NOT.(ASSOCIATED(pointerToVolumeData, &
                        object%volumeData(:,:,1,1,1)))) THEN
    charStringObject%charString = 'Pointer is not associated with this volumeData.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   CONTINUE
  ELSE
   charStringObject%charString = 'There is not a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  NULLIFY(pointerToVolumeData)
  object%hasPointerToVolumeData = .FALSE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE RemovePointerToVolumeData2D

SUBROUTINE RemovePointerToVolumeData1D(object,               &
                                       pointerToVolumeData,  &
                                       nodeMPIDataObject,    &
                                       enableChecking,       &
                                       errorInfoObject)

  TYPE(DataVolume5DLogicalType), TARGET, INTENT(INOUT) :: object
  LOGICAL, DIMENSION(:), POINTER :: pointerToVolumeData
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RemovePointerToVolumeData1D'

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

   IF (ALLOCATED(object%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

   IF (.NOT.(ASSOCIATED(pointerToVolumeData, &
                        object%volumeData(:,1,1,1,1)))) THEN
    charStringObject%charString = 'Pointer is not associated with this volumeData.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  IF (object%hasPointerToVolumeData) THEN
   CONTINUE
  ELSE
   charStringObject%charString = 'There is not a pointer to this volume data.'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

   GO TO 101
  END IF

  NULLIFY(pointerToVolumeData)
  object%hasPointerToVolumeData = .FALSE.
 
  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE RemovePointerToVolumeData1D

SUBROUTINE AddVolumeDataToMPIStructureDefinition5D(structMessageObject,    &
                                                   nodeMPIDataObject,      &
                                                   messageData,            &
                                                   iOrder,                 &    
                                                   iStart,                 &
                                                   iEnd,                   &
                                                   deltaI,                 &
                                                   numberOfDataDimensions, &
                                                   enableChecking,         &
                                                   errorInfoObject)

  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(DataVolume5DLogicalType), INTENT(IN) :: messageData
  INTEGER, INTENT(IN) :: numberOfDataDimensions
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder,iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  INTEGER, DIMENSION(5) :: iStartShift5D, &
                           iEndShift5D,   &
                           iOrderShift5D, &
                           deltaIShift5D
  INTEGER :: nD,dI,nDim

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation
  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE AddVolumeDataToMPIStructureDefinition5D'

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

   IF (.NOT.(messageData%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(messageData%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'bufferVolumeID dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  nDim = numberOfDataDimensions

! shift the start and end, since the MPI routine is starting the arrays at 1

  DO nD = 1,nDim
   dI = LBOUND(messageData%volumeData,nD)-1
   iStartShift5D(nD) = iStart(nD) + dI
   iEndShift5D(nD)   = iEnd(nD)   + dI
   iOrderShift5D(nD) = iOrder(nD)
   deltaIShift5D(nD) = deltaI(nD)
  END DO

  DO nD = nDim+1,4
   iStartShift5D(nD) = 1
   iEndShift5D(nD)   = 1
   iOrderShift5D(nD) = nD
   deltaIShift5D(nD) = 1
  END DO

! put the data in

  CALL AddDataToMPIStructureDefinition(nodeMPIDataObject     = nodeMPIDataObject,            &
                                       structMessageObject   = structMessageObject,          &
                                       messageData           = messageData%volumeData,       &
                                       iOrder                = iOrderShift5D,                &
                                       iStart                = iStartShift5D,                &
                                       iEnd                  = iEndShift5D,                  &
                                       deltaI                = deltaIShift5D,                &
                                       enableChecking        = enableChecking,               &
                                       errorInfoObject       = errorInfoObject)

   IF (CheckForLocalError(errorInfoObject)) GO TO 101

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE AddVolumeDataToMPIStructureDefinition5D

SUBROUTINE CheckForPointerToVolumeData5D(dataVolumeObject,     &
                                         volumeDataHasPointer, &
                                         enableChecking,       &
                                         errorInfoObject)

  TYPE(DataVolume5DLogicalType), INTENT(IN) :: dataVolumeObject
  LOGICAL, INTENT(OUT) :: volumeDataHasPointer
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local data

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CheckForPointerToVolumeData5D'

  IF (enableChecking) THEN

   IF (.NOT.(dataVolumeObject%isInitialized)) THEN
    charStringObject%charString = 'dataVolumeObject is not initialized.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   ELSE
    CONTINUE
   END IF

   IF (ALLOCATED(dataVolumeObject%volumeData)) THEN
    CONTINUE
   ELSE
    charStringObject%charString = 'bufferVolumeID dataVolume is not allocated.'
    CALL SetError(object          = errorInfoObject, &
                  errorInfoString = charStringObject)

    GO TO 101
   END IF

  ELSE
   CONTINUE ! feeling lucky!
  END IF

  volumeDataHasPointer = dataVolumeObject%hasPointerToVolumeData

  RETURN

 101 CONTINUE

  charStringObject%charString = ' in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  charStringObject%charString = moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN
END SUBROUTINE CheckForPointerToVolumeData5D

