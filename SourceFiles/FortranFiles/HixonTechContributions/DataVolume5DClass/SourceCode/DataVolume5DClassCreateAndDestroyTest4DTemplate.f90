!--------------create a dataVolume5DClass object------------------------------

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '------------------------------------'
   WRITE(0,*) 'Testing varTypeDataVolume5DObject...'
   WRITE(0,*)
  END IF

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '------------------------------------'
   WRITE(0,*) '---4D testing-----------------------'
   WRITE(0,*)
  END IF

!--------------create a dataVolume5DClass object------------------------------
  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: create a dataVolume5DObject.'
   WRITE(0,*) '      No error is expected.'
  END IF

  errorExpected = .FALSE.

  CALL CreateObject(object                       = varTypeDataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = varTypeBufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = varTypeBufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = varTypeData4D,                         &
                    nodeMPIDataObject            = mpiData,                               &
                    enableChecking               = enableChecking,                        &
                    errorInfoObject              = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   GO TO 101
  ELSE
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
  END IF

! Test the data in the object

  errorExpected = .FALSE.

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: check the data in the dataVolume5DObject'
   WRITE(0,*) '      No error is expected.'
  END IF

  CALL GetPointerToVolumeData(object                  = varTypeDataVolume5DObject,              &
                              pointerToVolumeData     = varTypePtr4D,                           &
                              pointerDataTotalBounds  = varTypePointerVolumeDataTotalBounds4D,  &
                              pointerDataUpdateBounds = varTypePointerVolumeDataUpdateBounds4D, &
                              nodeMPIDataObject       = mpiData,                                &
                              enableChecking          = enableChecking,                         &
                              errorInfoObject         = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   GO TO 101
  ELSE
   CONTINUE
  END IF

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Checking data: varTypePtr4D bounds are: '
   WRITE(0,*) '  dimension 1: ',LBOUND(varTypePtr4D,1),UBOUND(varTypePtr4D,1)
   WRITE(0,*) '  dimension 2: ',LBOUND(varTypePtr4D,2),UBOUND(varTypePtr4D,2)
   WRITE(0,*) '  dimension 3: ',LBOUND(varTypePtr4D,3),UBOUND(varTypePtr4D,3)
   WRITE(0,*) '  dimension 4: ',LBOUND(varTypePtr4D,4),UBOUND(varTypePtr4D,4)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = varTypePtr4D,        &
                      expectedDataValue = varTypeData4D,       &
                      correctValue      = correctValue,        &
                      errorLocation     = errorLocation,       &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect varTypeData4D value'
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)
  END IF

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   GO TO 101
  ELSE
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) '...correct value for interior varTypeData...'
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
  END IF

! get the volume data bounds and check

  errorExpected = .FALSE.

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: check the bounds of the dataVolume5DObject'
   WRITE(0,*) '      No error is expected.'
  END IF

  CALL GetVolumeDataBounds(dataVolumeObject       = varTypeDataVolume5DObject,                 &
                           volumeDataTotalBounds  = varTypeBufferVolumeDataTotalBounds4DTest,  &
                           volumeDataUpdateBounds = varTypeBufferVolumeDataUpdateBounds4DTest, &
                           enableChecking         = enableChecking,                            &
                           errorInfoObject        = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   GO TO 101
  ELSE
   CONTINUE
  END IF

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Checking data: volume data total bounds are: '
   WRITE(0,*) '  dimension 1: ',varTypeBufferVolumeDataTotalBounds4DTest(1,1:2)
   WRITE(0,*) '  dimension 2: ',varTypeBufferVolumeDataTotalBounds4DTest(2,1:2)
   WRITE(0,*) '  dimension 3: ',varTypeBufferVolumeDataTotalBounds4DTest(3,1:2)
   WRITE(0,*) '  dimension 4: ',varTypeBufferVolumeDataTotalBounds4DTest(4,1:2)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = varTypeBufferVolumeDataTotalBounds4DTest, &
                      expectedDataValue = varTypeBufferVolumeDataTotalBounds4D,     &
                      correctValue      = correctValue,                             &
                      errorLocation     = errorLocation,                            &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect varTypeData4D value'
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)
  END IF

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   GO TO 101
  ELSE
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) '...correct value for total volumeBounds...'
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
  END IF

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Checking data: volume data update bounds are: '
   WRITE(0,*) '  dimension 1: ',varTypeBufferVolumeDataUpdateBounds4DTest(1,1:2)
   WRITE(0,*) '  dimension 2: ',varTypeBufferVolumeDataUpdateBounds4DTest(2,1:2)
   WRITE(0,*) '  dimension 3: ',varTypeBufferVolumeDataUpdateBounds4DTest(3,1:2)
   WRITE(0,*) '  dimension 4: ',varTypeBufferVolumeDataUpdateBounds4DTest(4,1:2)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = varTypeBufferVolumeDataUpdateBounds4DTest, &
                      expectedDataValue = varTypeBufferVolumeDataUpdateBounds4D,     &
                      correctValue      = correctValue,                              &
                      errorLocation     = errorLocation,                             &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect varTypeData4D value'
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)
  END IF

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   GO TO 101
  ELSE
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) '...correct value for update volumeBounds...'
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
  END IF

! cause an error:  try to create an object that's already been created.

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: create a dataVolume5DObject that already exists.'
   WRITE(0,*) '      An error is expected.'
  END IF

  errorExpected = .TRUE.

  CALL CreateObject(object                       = varTypeDataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = varTypeBufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = varTypeBufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = varTypeData4D,                         &
                    nodeMPIDataObject            = mpiData,                               &
                    enableChecking               = enableChecking,                        &
                    errorInfoObject              = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: create an (already initialized) dataVolume5DObject; no error returned.'
   GO TO 100
  ELSE ! Need to reset the error information class object
   CALL WriteObject(object = errorInfoObject)
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Resetting the errorInfoObject.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
  END IF

! test: check for pointer when there is a pointer

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: Check for a pointer when the pointer exists.'
   WRITE(0,*) '      No error is expected.'
  END IF

  errorExpected = .FALSE.

  pointerIsExpected = .TRUE.

  CALL CheckForPointerToVolumeData(dataVolumeObject     = varTypeDataVolume5DObject, &
                                   volumeDataHasPointer = foundPointer,              &
                                   enableChecking       = enableChecking,            &
                                   errorInfoObject      = errorInfoObject)
    
  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: did not find an existing pointer.'
   GO TO 100
  ELSE ! Need to reset the error information class object
   IF (foundPointer .NEQV. pointerIsExpected) THEN
    passedTest = .FALSE.
    IF (masterNode .AND. verbose) THEN
     WRITE(0,*) 'Test FAILED.'
     WRITE(0,*) '--------------------------------'
     WRITE(0,*) '    '
     WRITE(0,*) 'Pointer is expected? ',pointerIsExpected
     WRITE(0,*) 'Pointer was found?   ',foundPointer
     WRITE(0,*)
    END IF
    charStringObject%charString =  &
        'FAIL: did not find an existing pointer.'
    GO TO 100
   ELSE
    CONTINUE ! all is well
   END IF

   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
  END IF

! cause an error:  try to create an object that's already been created.

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: create an (already initialized) dataVolume5DObject.'
   WRITE(0,*) '      An error is expected.'
  END IF

  errorExpected = .TRUE.

  CALL CreateObject(object                       = varTypeDataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = varTypeBufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = varTypeBufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = varTypeData4D,                         &
                    nodeMPIDataObject            = mpiData,                               &
                    enableChecking               = enableChecking,                        &
                    errorInfoObject              = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: create an (already initialized) dataVolume5DObject; no error returned.'
   GO TO 100
  ELSE ! Need to reset the error information class object
   CALL WriteObject(object = errorInfoObject)
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Resetting the errorInfoObject.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
  END IF

!--------------destroy a dataVolume5DClass object------------------------------

! cause an error:  try to destroy an object with an outstanding pointer

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: destroy a dataVolume5DObject with an outstanding pointer.'
   WRITE(0,*) '      An error is expected.'
  END IF

  errorExpected = .TRUE.

  CALL DestroyObject(object            = varTypeDataVolume5DObject,  &
                     nodeMPIDataObject = mpiData,                    &
                     enableChecking    = enableChecking,             &
                     errorInfoObject   = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: destroy dataVolume5DObject with outstanding pointer; no error found.'
   GO TO 100
  ELSE ! Need to reset the error information class object
   CALL WriteObject(object = errorInfoObject)
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Resetting the errorInfoObject.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
  END IF

! remove the pointer and try again

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: remove a pointer to dataVolume5DObject.'
   WRITE(0,*) '      No error is expected.'
  END IF

  errorExpected = .FALSE.

  CALL RemovePointerToVolumeData(object               = varTypeDataVolume5DObject, &
                                 pointerToVolumeData  = varTypePtr4D,              &
                                 nodeMPIDataObject    = mpiData,                   &
                                 enableChecking       = enableChecking,            &
                                 errorInfoObject      = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   GO TO 101
  ELSE
   WRITE(0,*) 'Test PASSED.'
   WRITE(0,*) '--------------------------------'
   WRITE(0,*)
  END IF

! test: check for pointer when there is no pointer

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: Check for a pointer when no pointer exists.'
   WRITE(0,*) '      No error is expected.'
  END IF

  errorExpected = .FALSE.

  pointerIsExpected = .FALSE.

  CALL CheckForPointerToVolumeData(dataVolumeObject     = varTypeDataVolume5DObject, &
                                   volumeDataHasPointer = foundPointer,              &
                                   enableChecking       = enableChecking,            &
                                   errorInfoObject      = errorInfoObject)
    
  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: did not find an existing pointer.'
   GO TO 100
  ELSE ! Need to reset the error information class object
   IF (foundPointer .NEQV. pointerIsExpected) THEN
    passedTest = .FALSE.
    IF (masterNode .AND. verbose) THEN
     WRITE(0,*) 'Test FAILED.'
     WRITE(0,*) '--------------------------------'
     WRITE(0,*) '    '
     WRITE(0,*) 'Pointer is expected? ',pointerIsExpected
     WRITE(0,*) 'Pointer was found?   ',foundPointer
     WRITE(0,*)
    END IF
    charStringObject%charString =  &
        'FAIL: did not find an existing pointer.'
    GO TO 100
   ELSE
    CONTINUE ! all is well
   END IF

   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
  END IF

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: destroy a dataVolume5DObject.'
   WRITE(0,*) '      No error is expected.'
  END IF

  errorExpected = .FALSE.

  CALL DestroyObject(object            = varTypeDataVolume5DObject,      &
                     nodeMPIDataObject = mpiData,            &
                     enableChecking    = enableChecking,     &
                     errorInfoObject   = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   GO TO 101
  ELSE
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
  END IF
  
! cause an error:  try to destroy an object that's already been destroyed.

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Test: destroy an (uninitialized) dataVolume5DObject.'
   WRITE(0,*) '      An error is expected.'
  END IF

  errorExpected = .TRUE.

  CALL DestroyObject(object            = varTypeDataVolume5DObject,      &
                     nodeMPIDataObject = mpiData,            &
                     enableChecking    = enableChecking,     &
                     errorInfoObject   = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
         'FAIL: destroy an (uninitialized) dataVolume5DObject; no error returned.'
   GO TO 100
  ELSE ! Need to reset the error information class object
   CALL WriteObject(object = errorInfoObject)
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Resetting the errorInfoObject.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   CALL DestroyObject(object = errorInfoObject)
   CALL CreateObject(object = errorInfoObject)
  END IF

  IF (masterNode) THEN
   IF (passedTest) THEN
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Completed testing object create and destroy routines.'
    WRITE(0,*) '   All routines have PASSED the tests.'
    WRITE(0,*) '--------------------------------'
   ELSE
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Completed testing object create and destroy routines.'
    WRITE(0,*) '      An unknown routine has FAILED the tests.'
    WRITE(0,*) '--------------------------------'
    charStringObject%charString =  &
          'FAIL: unknown test has failed.'
    GO TO 100
   END IF
  END IF

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '-------------------------------------------'
   WRITE(0,*) 'Testing varTypeDataVolume5DObject complete.'
   WRITE(0,*) '-------------------------------------------'
  END IF
