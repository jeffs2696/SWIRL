SUBROUTINE AddReal80DDataToMPIStruct(nodeMPIDataObject,     &
                                     structMessageObject,   &
                                     messageData,           &
                                     enableChecking,        &
                                     errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  REAL(KIND=real8Kind), INTENT(IN) :: messageData
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddReal80DDataToMPIStruct: '

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

END SUBROUTINE AddReal80DDataToMPIStruct

SUBROUTINE AddReal81DDataToMPIStruct(nodeMPIDataObject,     &
                                     structMessageObject,   &
                                     messageData,           &
                                     iStart,                &
                                     iEnd,                  &
                                     deltaI,                &
                                     enableChecking,        &
                                     errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(IN) :: nodeMPIDataObject
  TYPE(structMessageObjectType), INTENT(INOUT) :: structMessageObject
  REAL(KIND=real8Kind), DIMENSION(:), INTENT(IN) :: messageData
  INTEGER, INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddReal81DDataToMPIStruct: '

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

END SUBROUTINE AddReal81DDataToMPIStruct

SUBROUTINE AddReal82DDataToMPIStruct(nodeMPIDataObject,     &
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
  REAL(KIND=real8Kind), DIMENSION(:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddReal82DDataToMPIStruct: '

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

END SUBROUTINE AddReal82DDataToMPIStruct

SUBROUTINE AddReal83DDataToMPIStruct(nodeMPIDataObject,     &
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
  REAL(KIND=real8Kind), DIMENSION(:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddReal83DDataToMPIStruct: '

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

END SUBROUTINE AddReal83DDataToMPIStruct

SUBROUTINE AddReal84DDataToMPIStruct(nodeMPIDataObject,     &
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
  REAL(KIND=real8Kind), DIMENSION(:,:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddReal84DDataToMPIStruct: '

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

END SUBROUTINE AddReal84DDataToMPIStruct

SUBROUTINE AddReal85DDataToMPIStruct(nodeMPIDataObject,     &
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
  REAL(KIND=real8Kind), DIMENSION(:,:,:,:,:), INTENT(IN) :: messageData
  INTEGER, DIMENSION(:), INTENT(IN) :: iOrder
  INTEGER, DIMENSION(:), INTENT(IN) :: iStart,iEnd,deltaI
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variable def

  LOGICAL :: errorDetected
  CHARACTER(LEN=80) :: errorInformation

  CHARACTER(LEN=*), PARAMETER :: location =  &
     'SUBROUTINE AddReal85DDataToMPIStruct: '

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

END SUBROUTINE AddReal85DDataToMPIStruct

