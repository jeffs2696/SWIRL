MODULE DataVolume5DClassRestartFileIOTest

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE MPI2DataClassNoErrClass
  USE ErrorInformationClass
  USE DataVolume5DClass
  USE Check0DData
  USE Check1DData
  USE Check4DData
  USE Check5DData

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: RestartFileIOTest

  INTEGER, PARAMETER :: real8Def = REAL64

  TYPE(CharacterStringType) :: charStringObject

  CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE DataVolume5DClassRestartFileIOTest: '

INTERFACE RestartFileIOTest
  MODULE PROCEDURE RestartFileIODataVolume5DTest
END INTERFACE RestartFileIOTest

! this tests the DataVolume5DObject functions:
!
!  WriteObjectToFile
!  ReadObjectFromFile


CONTAINS

SUBROUTINE RestartFileIODataVolume5DTest(mpiData,        &
                                         verbose,        &
                                         enableChecking, &
                                         passedTest,     &
                                         errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(INOUT) :: mpiData

  LOGICAL, INTENT(IN) :: verbose, &
                         enableChecking
  LOGICAL, INTENT(INOUT) :: passedTest

  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  LOGICAL :: masterNode = .TRUE.

  LOGICAL :: errorFound,              &
             errorExpected

  LOGICAL :: correctValue = .TRUE.

  LOGICAL :: fileIsFormatted

  INTEGER, PARAMETER :: numberOfTestObjects = 1

  INTEGER :: iError

! dataVolume5D class objects

  TYPE(dataVolume5DType), DIMENSION(numberOfTestObjects) :: dataVolume5DObject

  TYPE(dataVolume5DType) :: dataVolume5DObjectUnInit

! these will be read in by the master for checking

  INTEGER :: n

  TYPE(dataVolume5DType), DIMENSION(numberOfTestObjects) :: dataVolume5DObjectTest

  INTEGER :: myMPINodeID, &
             numberOfMPINodes

  CHARACTER(LEN=80) :: restartFileName
  INTEGER :: restartFileUnitNumber

  LOGICAL, PARAMETER :: forceSorting = .TRUE.

! testing only REAL64 data for this.

  REAL(KIND=real8Def), PARAMETER :: real8Lo = -78434168.0_real8Def, &
                                    real8Hi =  84901325.0_real8Def

  REAL(KIND=real8Def), DIMENSION(3:7,51:72,4:5,-1:15,-3:5) :: real8Data5D
  INTEGER, DIMENSION(5,2) :: real8BufferVolumeDataTotalBounds5D = &
                          RESHAPE((/ 3, &! 1,1 
                                    51, &! 2,1 
                                     4, &! 3,1 
                                    -1, &! 4,1 
                                    -3, &! 5,1 
                                     7, &! 1,2 
                                    72, &! 2,2 
                                     5, &! 3,2 
                                    15, &! 4,2 
                                     5/),(/5,2/)) ! 5,2 

  INTEGER, DIMENSION(5,2) :: real8BufferVolumeDataUpdateBounds5D = &
                          RESHAPE((/ 5, &! 1,1 
                                    53, &! 2,1 
                                     6, &! 3,1 
                                     1, &! 4,1 
                                    -1, &! 5,1 
                                     4, &! 1,2 
                                    71, &! 2,2 
                                     4, &! 3,2 
                                    14, &! 4,2 
                                     4/),(/5,2/)) ! 5,2 

  INTEGER :: i,j,k,l,m
  LOGICAL :: fileIsOpen


! DOUBLE PRECISION :: maxWaitTimeInSeconds

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE RestartFileIODataVolume5DTest'

!------end of variable definition----------------------------

  CONTINUE ! execution begins here

  passedTest = .TRUE.
  errorExpected = .FALSE.

  errorFound = .FALSE.

  masterNode = IsMasterNode(object = mpiData)

  IF (masterNode) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Testing object Restart File I/O routines.'
   WRITE(0,*) '--------------------------------'
  END IF

  myMPINodeID = GetNodeID(object = mpiData)

  numberOfMPINodes = GetNumberOfNodes(object = mpiData)

  IF (masterNode) THEN

! generate the test objects

   DO n=1,numberOfTestObjects

    DO m= real8BufferVolumeDataTotalBounds5D(5,1), &
          real8BufferVolumeDataTotalBounds5D(5,2)
     DO l= real8BufferVolumeDataTotalBounds5D(4,1), &
           real8BufferVolumeDataTotalBounds5D(4,2)
      DO k= real8BufferVolumeDataTotalBounds5D(3,1), &
            real8BufferVolumeDataTotalBounds5D(3,2)
       DO j= real8BufferVolumeDataTotalBounds5D(2,1), &
             real8BufferVolumeDataTotalBounds5D(2,2)
        DO i= real8BufferVolumeDataTotalBounds5D(1,1), &
              real8BufferVolumeDataTotalBounds5D(1,2)
         CALL RANDOM_NUMBER(real8Data5D(i,j,k,l,m))
        END DO
       END DO
      END DO
     END DO
    END DO

    CALL CreateObject(object                       = dataVolume5DObject(n),               &
                      bufferVolumeDataTotalBounds  = real8BufferVolumeDataTotalBounds5D,  &
                      bufferVolumeDataUpdateBounds = real8BufferVolumeDataUpdateBounds5D, &
                      bufferVolumeData             = real8Data5D,                         &
                      nodeMPIDataObject            = mpiData,                             &
                      enableChecking               = enableChecking,                      &
                      errorInfoObject              = errorInfoObject)

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

   END DO

! ----------------------------RestartFileIO testing starts--------------------------

! open the restart file

   restartFileName = '../Code/testFile.dat'
   restartFileUnitNumber = 22

   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Testing function WriteObjectToFile.'
    WRITE(0,*) '      An error is expected.'
   END IF

  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.

!  dataVolume5DObject is not initialized 

  IF (masterNode) THEN

   CALL WriteObjectToFile(object            = dataVolume5DObjectUnInit, &
                          fileUnit          = restartFileUnitNumber,    &
                          fileIsFormatted   = fileIsFormatted,          &
                          nodeMPIDataObject = mpiData,                  &
                          enableChecking    = enableChecking,           &
                          errorInfoObject   = errorInfoObject)

  ELSE
   CONTINUE
  END IF

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: calling WriteObjectToFile uninit; no error returned.'
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

! calling with unopened restart file

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Testing function WriteObjectToFile.'
   WRITE(0,*) '      An error is expected.'
  END IF

  errorExpected = .TRUE.

  IF (masterNode) THEN

!  restart file has not been opened

   CALL WriteObjectToFile(object            = dataVolume5DObject(1), &
                          fileUnit          = restartFileUnitNumber, &
                          fileIsFormatted   = fileIsFormatted,       &
                          nodeMPIDataObject = mpiData,               &
                          enableChecking    = enableChecking,        &
                          errorInfoObject   = errorInfoObject)
  
  ELSE
   CONTINUE
  END IF

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: calling WriteObjectToFile fileNotOpen; no error returned.'
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

! actually open the restart file

  IF (masterNode) THEN

   OPEN(NEWUNIT = restartFileUnitNumber, &
        FILE    = restartFileName,       &
        FORM    = 'FORMATTED',         &
        IOSTAT  = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'Error in opening restart file'
    GO TO 100
   END IF

   fileIsFormatted = .TRUE.

! Write to restart file

   DO n=1,numberOfTestObjects
    CALL WriteObjectToFile(object            = dataVolume5DObject(n), &
                           fileUnit          = restartFileUnitNumber, &
                           fileIsFormatted   = fileIsFormatted,       &
                           nodeMPIDataObject = mpiData,               &
                           enableChecking    = enableChecking,        &
                           errorInfoObject   = errorInfoObject)

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

   END DO

! all done with the restart file; shut it down

   CLOSE(UNIT = restartFileUnitNumber, &
         IOSTAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'Error in closing restart file (1)'
    GO TO 100
   END IF

   WRITE(0,*) 'Closed fileUnit ',restartFileUnitNumber

   INQUIRE(UNIT=restartFileUnitNumber,OPENED = fileIsOpen)

   WRITE(0,*) 'FileUnit = ',restartFileUnitNumber
   WRITE(0,*) 'FileIsOpen = ',fileIsOpen


! get rid of the dataVolume5DObjects
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test PASSED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'DataVolume5DObjects have been written to restart file'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF

  ELSE
   CONTINUE
  END IF

! now work on reading the data back in.

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Testing function ReadObjectFromFile.'
   WRITE(0,*) '      An error is expected.'
  END IF

  restartFileUnitNumber = 87 ! just give it a value

  errorExpected = .TRUE.

  IF (masterNode) THEN

!  dataVolume5DObject is already initialized 

   CALL ReadObjectFromFile(object            = dataVolume5DObject(1), &
                           fileUnit          = restartFileUnitNumber, &
                           fileIsFormatted   = fileIsFormatted,       &
                           nodeMPIDataObject = mpiData,               &
                           enableChecking    = enableChecking,        &
                           errorInfoObject   = errorInfoObject)

  ELSE
   CONTINUE
  END IF

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: calling ReadObjectFromFile uninit; no error returned.'
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

! calling with unopened restart file

  IF (masterNode .AND. verbose) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Testing function ReadObjectFromFile.'
   WRITE(0,*) '      An error is expected.'
  END IF

  errorExpected = .TRUE.

  IF (masterNode) THEN
!  restart file has not been opened

   CALL ReadObjectFromFile(object            = dataVolume5DObjectTest(1), &
                           fileUnit          = restartFileUnitNumber,     &
                           fileIsFormatted   = fileIsFormatted,           &
                           nodeMPIDataObject = mpiData,                   &
                           enableChecking    = enableChecking,            &
                           errorInfoObject   = errorInfoObject)

  ELSE
   CONTINUE
  END IF

  errorFound = CheckForGlobalError(errorInfoObject)

  IF (errorFound .NEQV. errorExpected) THEN
   passedTest = .FALSE.
   IF (masterNode .AND. verbose) THEN
    WRITE(0,*) 'Test FAILED.'
    WRITE(0,*) '--------------------------------'
    WRITE(0,*)
   END IF
   charStringObject%charString =  &
       'FAIL: calling ReadObjectFromFile fileNotOpen; no error returned.'
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

! actually open the restart file

   OPEN(UNIT = restartFileUnitNumber, &
        FILE = restartFileName,       &
        FORM = 'FORMATTED',         &
        IOSTAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'Error in opening restart file'
    GO TO 100
   END IF

   fileIsFormatted = .TRUE.

! Read from restart file

   DO n=1,numberOfTestObjects
    CALL ReadObjectFromFile(object            = dataVolume5DObjectTest(n),  &
                            fileUnit          = restartFileUnitNumber, &
                            fileIsFormatted   = fileIsFormatted,           &
                            nodeMPIDataObject = mpiData,               &
                            enableChecking    = enableChecking,        &
                            errorInfoObject   = errorInfoObject)

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

   END DO

! all done with the restart file; shut it down

   CLOSE(UNIT = restartFileUnitNumber, &
        IOSTAT = iError)

   IF (iError /= 0) THEN
    charStringObject%charString = 'Error in closing restart file (2)'
    GO TO 100
   END IF

! and test the data that has been read in
    
   DO n=1,numberOfTestObjects

!  check the dataVolume5DObjects

    IF (masterNode) THEN
     WRITE(0,*) '------------------------------------'
     WRITE(0,*) 
     WRITE(0,*) 'Testing dataVolume5DObject ',n,'...'
     WRITE(0,*) 
    END IF

    errorExpected = .FALSE.

!   pointIDTest = GetPointID(dataVolume5DObject   = dataVolume5DObjectTest(n), &
!                            enableChecking  = enableChecking, &
!                            errorInfoObject = errorInfoObject)

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

!   CALL CheckDataValue(dataValue         = pointIDTest,     &
!                       expectedDataValue = pointID(n),      &
!                       correctValue      = correctValue,    &
!                       errorInfoObject   = errorInfoObject)
 
!   IF (.NOT. correctValue) THEN
 
!    WRITE(charStringObject%charString,'(a11,i8)') ' found:    ',pointIDTest
!    CALL SetError(object          = errorInfoObject, &
!                  errorInfoString = charStringObject)
 
!    WRITE(charStringObject%charString,'(a11,i8)') ' expected: ',pointID(n)
!    CALL AddErrorInformation(object          = errorInfoObject, &
!                             errorInfoString = charStringObject)
 
!    charStringObject%charString = 'ERROR:  incorrect pointID value'
!    CALL AddErrorInformation(object          = errorInfoObject, &
!                             errorInfoString = charStringObject)
!   END IF

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

    IF (masterNode .AND. verbose) THEN
!    WRITE(0,*) '...correct value for pointID: ', pointIDTest
     WRITE(0,*)
    END IF

!   numberOfDimensionsTest = GetPointNumberOfDimensions(dataVolume5DObject   = dataVolume5DObjectTest(n), &
!                                                       enableChecking  = enableChecking, &
!                                                       errorInfoObject = errorInfoObject)

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

!   CALL CheckDataValue(dataValue         = numberOfDimensionsTest,  &
!                       expectedDataValue = numberOfDimensions(n),   &
!                       correctValue      = correctValue,            &
!                       errorInfoObject   = errorInfoObject)
 
!   IF (.NOT. correctValue) THEN
 
!    WRITE(charStringObject%charString,'(a11,i8)') ' found:    ',numberOfDimensionsTest
!    CALL SetError(object          = errorInfoObject, &
!                  errorInfoString = charStringObject)
  
!    WRITE(charStringObject%charString,'(a11,i8)') ' expected: ',numberOfDimensions(n)
!    CALL AddErrorInformation(object          = errorInfoObject, &
!                             errorInfoString = charStringObject)
 
!    charStringObject%charString = 'ERROR:  incorrect numberOfDimensions value'
!    CALL AddErrorInformation(object          = errorInfoObject, &
!                             errorInfoString = charStringObject)
!   END IF

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

    IF (masterNode .AND. verbose) THEN
!    WRITE(0,*) '...correct value for numberOfDimensions: ', numberOfDimensionsTest
     WRITE(0,*)
    END IF

!   correctValue = PointsAreEqual(dataVolume5DObject1  = dataVolume5DObjectTest(n), &
!                                 dataVolume5DObject2  = dataVolume5DObject(n),     &
!                                 enableChecking  = enableChecking,       &
!                                 errorInfoObject = errorInfoObject)

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

    IF (.NOT. (correctValue)) THEN
     passedTest = .FALSE.
     IF (masterNode .AND. verbose) THEN
      WRITE(0,*) 'Test FAILED (pointObject4D): ',n
      WRITE(0,*) '--------------------------------'
      WRITE(0,*)
     END IF
     GO TO 101
    ELSE
     CONTINUE
    END IF

    IF (masterNode .AND. verbose) THEN
     WRITE(0,*) '...correct point values. '
     WRITE(0,*)
     WRITE(0,*) 'Test PASSED.'
     WRITE(0,*) '--------------------------------'
     WRITE(0,*)
    END IF

   END DO

! get rid of the dataVolume5DObjects

   DO n=1,numberOfTestObjects

    CALL DestroyObject(object            = dataVolume5DObject(n), &
                       nodeMPIDataObject = mpiData,          &
                       enableChecking    = enableChecking,   &
                       errorInfoObject   = errorInfoObject)

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

    CALL DestroyObject(object            = dataVolume5DObjectTest(n), &
                       nodeMPIDataObject = mpiData,              &
                       enableChecking    = enableChecking,       &
                       errorInfoObject   = errorInfoObject)

    errorFound = CheckForLocalError(errorInfoObject)

    IF (errorFound) THEN
     passedTest = .FALSE.
     GO TO 101
    END IF

   END DO

   errorFound = CheckForLocalError(errorInfoObject)

   IF (errorFound) THEN
    IF (masterNode) THEN
     WRITE(0,*) 'master: error found.'
    END IF
    passedTest = .FALSE.
    GO TO 101
   END IF

  ELSE ! not the master node; does not participate in test
   CONTINUE
  END IF

  errorFound = CheckForGlobalError(errorInfoObject)

! end of testing 

   IF (masterNode) THEN
    IF (passedTest) THEN
     WRITE(0,*) '--------------------------------'
     WRITE(0,*) 'Completed testing object RestartFile I/O routines.'
     WRITE(0,*) '   All routines have PASSED the tests.'
     WRITE(0,*) '--------------------------------'
    ELSE
     WRITE(0,*) '--------------------------------'
     WRITE(0,*) 'Completed testing object RestartFile I/O routines.'
     WRITE(0,*) '      An unknown routine has FAILED the tests.'
     WRITE(0,*) '--------------------------------'
     charStringObject%charString =  &
           'FAIL: unknown test has failed.'
     GO TO 100
    END IF
   END IF
  RETURN

100 CONTINUE

  IF (.NOT. passedTest) THEN
   IF (masterNode) THEN
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Testing object RestartFile I/O routines.'
    WRITE(0,*) '      Tests have FAILED.'
    WRITE(0,*) '--------------------------------'
   END IF
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

  END IF

 101 CONTINUE

  IF (.NOT. passedTest) THEN
   charStringObject%charString = ' in '//location
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = 'in '//moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

  END IF

  RETURN
END SUBROUTINE RestartFileIODataVolume5DTest

END MODULE DataVolume5DClassRestartFileIOTest
