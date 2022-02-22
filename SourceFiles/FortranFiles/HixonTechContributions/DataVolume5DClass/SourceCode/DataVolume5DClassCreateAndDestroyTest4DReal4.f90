!--------------create a dataVolume5DClass object------------------------------

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '------------------------------------'
   WRITE(0,*) 'Testing real4DataVolume5DObject...'
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

  CALL CreateObject(object                       = real4DataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = real4BufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = real4BufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = real4Data4D,                         &
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

  CALL GetPointerToVolumeData(object                  = real4DataVolume5DObject,              &
                              pointerToVolumeData     = real4Ptr4D,                           &
                              pointerDataTotalBounds  = real4PointerVolumeDataTotalBounds4D,  &
                              pointerDataUpdateBounds = real4PointerVolumeDataUpdateBounds4D, &
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
   WRITE(0,*) 'Checking data: real4Ptr4D bounds are: '
   WRITE(0,*) '  dimension 1: ',LBOUND(real4Ptr4D,1),UBOUND(real4Ptr4D,1)
   WRITE(0,*) '  dimension 2: ',LBOUND(real4Ptr4D,2),UBOUND(real4Ptr4D,2)
   WRITE(0,*) '  dimension 3: ',LBOUND(real4Ptr4D,3),UBOUND(real4Ptr4D,3)
   WRITE(0,*) '  dimension 4: ',LBOUND(real4Ptr4D,4),UBOUND(real4Ptr4D,4)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = real4Ptr4D,        &
                      expectedDataValue = real4Data4D,       &
                      correctValue      = correctValue,        &
                      errorLocation     = errorLocation,       &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect real4Data4D value'
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
    WRITE(0,*) '...correct value for interior real4Data...'
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

  CALL GetVolumeDataBounds(dataVolumeObject       = real4DataVolume5DObject,                 &
                           volumeDataTotalBounds  = real4BufferVolumeDataTotalBounds4DTest,  &
                           volumeDataUpdateBounds = real4BufferVolumeDataUpdateBounds4DTest, &
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
   WRITE(0,*) '  dimension 1: ',real4BufferVolumeDataTotalBounds4DTest(1,1:2)
   WRITE(0,*) '  dimension 2: ',real4BufferVolumeDataTotalBounds4DTest(2,1:2)
   WRITE(0,*) '  dimension 3: ',real4BufferVolumeDataTotalBounds4DTest(3,1:2)
   WRITE(0,*) '  dimension 4: ',real4BufferVolumeDataTotalBounds4DTest(4,1:2)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = real4BufferVolumeDataTotalBounds4DTest, &
                      expectedDataValue = real4BufferVolumeDataTotalBounds4D,     &
                      correctValue      = correctValue,                             &
                      errorLocation     = errorLocation,                            &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect real4Data4D value'
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
   WRITE(0,*) '  dimension 1: ',real4BufferVolumeDataUpdateBounds4DTest(1,1:2)
   WRITE(0,*) '  dimension 2: ',real4BufferVolumeDataUpdateBounds4DTest(2,1:2)
   WRITE(0,*) '  dimension 3: ',real4BufferVolumeDataUpdateBounds4DTest(3,1:2)
   WRITE(0,*) '  dimension 4: ',real4BufferVolumeDataUpdateBounds4DTest(4,1:2)
   WRITE(0,*) '      No error is expected.'
  END IF
! check data

  CALL CheckDataValue(dataValue         = real4BufferVolumeDataUpdateBounds4DTest, &
                      expectedDataValue = real4BufferVolumeDataUpdateBounds4D,     &
                      correctValue      = correctValue,                              &
                      errorLocation     = errorLocation,                             &
                      errorInfoObject   = errorInfoObject)

  IF (.NOT. correctValue) THEN

   WRITE(charStringObject%charString,'(a11,4(1x,i5))') ' Location: ',errorLocation(1:4)
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'ERROR:  incorrect real4Data4D value'
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

  CALL CreateObject(object                       = real4DataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = real4BufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = real4BufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = real4Data4D,                         &
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

  CALL CheckForPointerToVolumeData(dataVolumeObject     = real4DataVolume5DObject, &
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

  CALL CreateObject(object                       = real4DataVolume5DObject,             &
                    bufferVolumeDataTotalBounds  = real4BufferVolumeDataTotalBounds4D,  &
                    bufferVolumeDataUpdateBounds = real4BufferVolumeDataUpdateBounds4D, &
                    bufferVolumeData             = real4Data4D,                         &
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

  CALL DestroyObject(object            = real4DataVolume5DObject,  &
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

  CALL RemovePointerToVolumeData(object               = real4DataVolume5DObject, &
                                 pointerToVolumeData  = real4Ptr4D,              &
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

  CALL CheckForPointerToVolumeData(dataVolumeObject     = real4DataVolume5DObject, &
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

  CALL DestroyObject(object            = real4DataVolume5DObject,      &
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

  CALL DestroyObject(object            = real4DataVolume5DObject,      &
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
   WRITE(0,*) 'Testing real4DataVolume5DObject complete.'
   WRITE(0,*) '-------------------------------------------'
  END IF
