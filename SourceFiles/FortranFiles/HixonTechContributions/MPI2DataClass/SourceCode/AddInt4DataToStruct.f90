SUBROUTINE AddInt40DDataToMPIStruct(nodeMPIDataObject,     &
                                    structMessageObject,   &
                                    messageData,           &
                                    enableChecking,        &
                                    errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  INTEGER(KIND=int4Kind), INTENT(IN) :: messageData
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddInt40DDataToMPIStruct: '

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

END SUBROUTINE AddInt40DDataToMPIStruct

SUBROUTINE AddInt41DDataToMPIStruct(nodeMPIDataObject,     &
                                    structMessageObject,   &
                                    messageData,           &
                                    iStart,                &
                                    iEnd,                  &
                                    deltaI,                &
                                    enableChecking,        &
                                    errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  INTEGER(KIND=int4Kind), DIMENSION(:), INTENT(IN) :: messageData
  INTEGER, INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddInt41DDataToMPIStruct: '

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

END SUBROUTINE AddInt41DDataToMPIStruct

SUBROUTINE AddInt42DDataToMPIStruct(nodeMPIDataObject,     &
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
  INTEGER(KIND=int4Kind), DIMENSION(:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddInt42DDataToMPIStruct: '

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

END SUBROUTINE AddInt42DDataToMPIStruct

SUBROUTINE AddInt43DDataToMPIStruct(nodeMPIDataObject,     &
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
  INTEGER(KIND=int4Kind), DIMENSION(:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddInt43DDataToMPIStruct: '

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

END SUBROUTINE AddInt43DDataToMPIStruct

SUBROUTINE AddInt44DDataToMPIStruct(nodeMPIDataObject,     &
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
  INTEGER(KIND=int4Kind), DIMENSION(:,:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddInt44DDataToMPIStruct: '

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

END SUBROUTINE AddInt44DDataToMPIStruct

SUBROUTINE AddInt45DDataToMPIStruct(nodeMPIDataObject,     &
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
  INTEGER(KIND=int4Kind), DIMENSION(:,:,:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddInt45DDataToMPIStruct: '

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

END SUBROUTINE AddInt45DDataToMPIStruct

