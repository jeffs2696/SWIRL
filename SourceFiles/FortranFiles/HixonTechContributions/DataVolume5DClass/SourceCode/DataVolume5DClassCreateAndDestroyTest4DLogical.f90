!--------------create a dataVolume5DClass object------------------------------

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '------------------------------------'
   WRITE(0,*) 'Testing logicalDataVolume5DObject...'
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

  CALL CreateObject(object                       = logicalDataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = logicalBufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = logicalBufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = logicalData4D,                         &
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

  CALL GetPointerToVolumeData(object                  = logicalDataVolume5DObject,              &
                              pointerToVolumeData     = logicalPtr4D,                           &
                              pointerDataTotalBounds  = logicalPointerVolumeDataTotalBounds4D,  &
                              pointerDataUpdateBounds = logicalPointerVolumeDataUpdateBounds4D, &
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
   WRITE(0,*) 'Checking data: logicalPtr4D bounds are: '
   WRITE(0,*) '  dimension 1: ',LBOUND(logicalPtr4D,1),UBOUND(logicalPtr4D,1)
   WRITE(0,*) '  dimension 2: ',LBOUND(logicalPtr4D,2),UBOUND(logicalPtr4D,2)
   WRITE(0,*) '  dimension 3: ',LBOUND(logicalPtr4D,3),UBOUND(logicalPtr4D,3)
   WRITE(0,*) '  dimension 4: ',LBOUND(logicalPtr4D,4),UBOUND(logicalPtr4D,4)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = logicalPtr4D,        &
                      expectedDataValue = logicalData4D,       &
                      correctValue      = correctValue,        &
                      errorLocation     = errorLocation,       &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect logicalData4D value'
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
    WRITE(0,*) '...correct value for interior logicalData...'
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

  CALL GetVolumeDataBounds(dataVolumeObject       = logicalDataVolume5DObject,                 &
                           volumeDataTotalBounds  = logicalBufferVolumeDataTotalBounds4DTest,  &
                           volumeDataUpdateBounds = logicalBufferVolumeDataUpdateBounds4DTest, &
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
   WRITE(0,*) '  dimension 1: ',logicalBufferVolumeDataTotalBounds4DTest(1,1:2)
   WRITE(0,*) '  dimension 2: ',logicalBufferVolumeDataTotalBounds4DTest(2,1:2)
   WRITE(0,*) '  dimension 3: ',logicalBufferVolumeDataTotalBounds4DTest(3,1:2)
   WRITE(0,*) '  dimension 4: ',logicalBufferVolumeDataTotalBounds4DTest(4,1:2)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = logicalBufferVolumeDataTotalBounds4DTest, &
                      expectedDataValue = logicalBufferVolumeDataTotalBounds4D,     &
                      correctValue      = correctValue,                             &
                      errorLocation     = errorLocation,                            &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect logicalData4D value'
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
   WRITE(0,*) '  dimension 1: ',logicalBufferVolumeDataUpdateBounds4DTest(1,1:2)
   WRITE(0,*) '  dimension 2: ',logicalBufferVolumeDataUpdateBounds4DTest(2,1:2)
   WRITE(0,*) '  dimension 3: ',logicalBufferVolumeDataUpdateBounds4DTest(3,1:2)
   WRITE(0,*) '  dimension 4: ',logicalBufferVolumeDataUpdateBounds4DTest(4,1:2)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = logicalBufferVolumeDataUpdateBounds4DTest, &
                      expectedDataValue = logicalBufferVolumeDataUpdateBounds4D,     &
                      correctValue      = correctValue,                              &
                      errorLocation     = errorLocation,                             &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect logicalData4D value'
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

  CALL CreateObject(object                       = logicalDataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = logicalBufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = logicalBufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = logicalData4D,                         &
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

  CALL CheckForPointerToVolumeData(dataVolumeObject     = logicalDataVolume5DObject, &
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

  CALL CreateObject(object                       = logicalDataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = logicalBufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = logicalBufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = logicalData4D,                         &
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

  CALL DestroyObject(object            = logicalDataVolume5DObject,  &
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

  CALL RemovePointerToVolumeData(object               = logicalDataVolume5DObject, &
                                 pointerToVolumeData  = logicalPtr4D,              &
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

  CALL CheckForPointerToVolumeData(dataVolumeObject     = logicalDataVolume5DObject, &
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

  CALL DestroyObject(object            = logicalDataVolume5DObject,      &
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

  CALL DestroyObject(object            = logicalDataVolume5DObject,      &
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
   WRITE(0,*) 'Testing logicalDataVolume5DObject complete.'
   WRITE(0,*) '-------------------------------------------'
  END IF
