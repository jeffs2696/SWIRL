SUBROUTINE AddChar0DDataToMPIStruct(nodeMPIDataObject,     &
                                    structMessageObject,   &
                                    messageData,           &
                                    enableChecking,        &
                                    errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  CHARACTER, INTENT(IN) :: messageData
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddChar0DDataToMPIStruct: '

  IF (enableChecking) THEN

   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    charStringObject%charString = 'nodeMPIDataObject is not initialized.'

    GO TO 100
   END IF

   IF (.NOT. structMessageObject%initialized) THEN
    charStringObject%charString = 'structMessageObject is not initialized.'

    GO TO 100
   END IF

  END IF

 CALL AddDataToMPIStruct(object          = structMessageObject%mpiStructLL, &
                          messageData     = messageData,                     &
                          errorInfoObject = errorInfoObject)

  IF (enableChecking) THEN
   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   ELSE
    CONTINUE
   END IF
  END IF

  RETURN

 100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE AddChar0DDataToMPIStruct

SUBROUTINE AddChar1DDataToMPIStruct(nodeMPIDataObject,     &
                                    structMessageObject,   &
                                    messageData,           &
                                    iStart,                &
                                    iEnd,                  &
                                    deltaI,                &
                                    enableChecking,        &
                                    errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  CHARACTER, DIMENSION(:), INTENT(IN) :: messageData
  INTEGER, INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddChar1DDataToMPIStruct: '

  IF (enableChecking) THEN

   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    charStringObject%charString = 'nodeMPIDataObject is not initialized.'

    GO TO 100
   END IF

   IF (.NOT. structMessageObject%initialized) THEN
    charStringObject%charString = 'structMessageObject is not initialized.'

    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStruct(object          = structMessageObject%mpiStructLL, &
                          messageData     = messageData,                     &
                          iStart          = iStart,                          &
                          iEnd            = iEnd,                            &
                          deltaI          = deltaI,                          &
                          errorInfoObject = errorInfoObject)

  IF (enableChecking) THEN
   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   ELSE
    CONTINUE
   END IF
  END IF

  RETURN

 100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE AddChar1DDataToMPIStruct

SUBROUTINE AddChar2DDataToMPIStruct(nodeMPIDataObject,     &
                                    structMessageObject,   &
                                    messageData,           &
                                    iOrder,                &
                                    iStart,                &
                                    iEnd,                  &
                                    deltaI,                &
                                    enableChecking,        &
                                    errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  CHARACTER, DIMENSION(:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddChar2DDataToMPIStruct: '

  IF (enableChecking) THEN

   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    charStringObject%charString = 'nodeMPIDataObject is not initialized.'

    GO TO 100
   END IF

   IF (.NOT. structMessageObject%initialized) THEN
    charStringObject%charString = 'structMessageObject is not initialized.'

    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStruct(object          = structMessageObject%mpiStructLL, &
                          messageData     = messageData,                     &
                          iOrder          = iOrder,                          &
                          iStart          = iStart,                          &
                          iEnd            = iEnd,                            &
                          deltaI          = deltaI,                          &
                          errorInfoObject = errorInfoObject)

  IF (enableChecking) THEN
   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   ELSE
    CONTINUE
   END IF
  END IF

  RETURN

 100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE AddChar2DDataToMPIStruct

SUBROUTINE AddChar3DDataToMPIStruct(nodeMPIDataObject,     &
                                    structMessageObject,   &
                                    messageData,           &
                                    iOrder,                &
                                    iStart,                &
                                    iEnd,                  &
                                    deltaI,                &
                                    enableChecking,        &
                                    errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  CHARACTER, DIMENSION(:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddChar3DDataToMPIStruct: '

  IF (enableChecking) THEN

   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    charStringObject%charString = 'nodeMPIDataObject is not initialized.'

    GO TO 100
   END IF

   IF (.NOT. structMessageObject%initialized) THEN
    charStringObject%charString = 'structMessageObject is not initialized.'

    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStruct(object          = structMessageObject%mpiStructLL, &
                          messageData     = messageData,                     &
                          iOrder          = iOrder,                          &
                          iStart          = iStart,                          &
                          iEnd            = iEnd,                            &
                          deltaI          = deltaI,                          &
                          errorInfoObject = errorInfoObject)

  IF (enableChecking) THEN
   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   ELSE
    CONTINUE
   END IF
  END IF

  RETURN

 100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE AddChar3DDataToMPIStruct

SUBROUTINE AddChar4DDataToMPIStruct(nodeMPIDataObject,     &
                                    structMessageObject,   &
                                    messageData,           &
                                    iOrder,                &
                                    iStart,                &
                                    iEnd,                  &
                                    deltaI,                &
                                    enableChecking,        &
                                    errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  CHARACTER, DIMENSION(:,:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddChar4DDataToMPIStruct: '

  IF (enableChecking) THEN

   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    charStringObject%charString = 'nodeMPIDataObject is not initialized.'

    GO TO 100
   END IF

   IF (.NOT. structMessageObject%initialized) THEN
    charStringObject%charString = 'structMessageObject is not initialized.'

    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStruct(object          = structMessageObject%mpiStructLL, &
                          messageData     = messageData,                     &
                          iOrder          = iOrder,                          &
                          iStart          = iStart,                          &
                          iEnd            = iEnd,                            &
                          deltaI          = deltaI,                          &
                          errorInfoObject = errorInfoObject)

  IF (enableChecking) THEN
   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   ELSE
    CONTINUE
   END IF
  END IF

  RETURN

 100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE AddChar4DDataToMPIStruct

SUBROUTINE AddChar5DDataToMPIStruct(nodeMPIDataObject,     &
                                    structMessageObject,   &
                                    messageData,           &
                                    iOrder,                &
                                    iStart,                &
                                    iEnd,                  &
                                    deltaI,                &
                                    enableChecking,        &
                                    errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  CHARACTER, DIMENSION(:,:,:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddChar5DDataToMPIStruct: '

  IF (enableChecking) THEN

   IF (.NOT. CheckMPIInitialization(                &
              object           = nodeMPIDataObject, &
              errorDetected    = errorDetected,     &
              errorInformation = errorInformation)) THEN

    charStringObject%charString = 'nodeMPIDataObject is not initialized.'

    GO TO 100
   END IF

   IF (.NOT. structMessageObject%initialized) THEN
    charStringObject%charString = 'structMessageObject is not initialized.'

    GO TO 100
   END IF

  END IF

  CALL AddDataToMPIStruct(object          = structMessageObject%mpiStructLL, &
                          messageData     = messageData,                     &
                          iOrder          = iOrder,                          &
                          iStart          = iStart,                          &
                          iEnd            = iEnd,                            &
                          deltaI          = deltaI,                          &
                          errorInfoObject = errorInfoObject)

  IF (enableChecking) THEN
   IF (CheckForLocalError(errorInfoObject)) THEN
    GO TO 101
   ELSE
    CONTINUE
   END IF
  END IF

  RETURN

 100 CONTINUE
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)

 101 CONTINUE

  charStringObject%charString = ' in '//location//moduleLocation
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  RETURN

END SUBROUTINE AddChar5DDataToMPIStruct

