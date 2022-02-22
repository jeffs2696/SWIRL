PROGRAM Main

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE MessagePassingInterface
  USE ErrorInformationClass
  USE QuickSortClass
  USE CheckSorting

  IMPLICIT NONE

  INTEGER, PARAMETER :: listLength = 100001

  LOGICAL :: passedTest,       &
             masterNode,       &
             mpiIsInitialized, &
             errorExpected,    &
             errorFound,       &
             enableChecking,   &
             arrayIsSorted

  INTEGER, PARAMETER :: mpiCommWorld  = MPI_COMM_WORLD, &
                        mpiSuccess    = MPI_SUCCESS,    &
                        mpiStatusSize = MPI_STATUS_SIZE

  INTEGER :: mpiIerror,       &
             mpiCommRank

  TYPE(ErrorInformationType) :: errorInfoObject
! TYPE(CharacterStringType) :: charStringObject

! four arrays to test

  INTEGER(KIND=4), DIMENSION(listLength) :: arrayInt4, arrayInt4Sorted
  INTEGER(KIND=8), DIMENSION(listLength) :: arrayInt8, arrayInt8Sorted
  REAL(KIND=4), DIMENSION(listLength) :: arrayReal4,arrayReal4Sorted
  REAL(KIND=8), DIMENSION(listLength) :: arrayReal8,arrayReal8Sorted
  INTEGER, DIMENSION(listLength) :: sortedDataLocation
  INTEGER :: i

  TYPE(QuickSortType) :: sortingObject

  CONTINUE ! execution starts here

  enableChecking = .TRUE.
  passedTest = .TRUE.
  arrayIsSorted = .TRUE.

! start MPI for the ErrorInformationClass object

  CALL HTMPI_INIT(mpiIerror) ! IERROR

  passedTest = .TRUE.

  IF (mpiIerror /= mpiSuccess) THEN
   passedTest = .FALSE.
   GO TO 100
  END IF

  CALL HTMPI_COMM_RANK(mpiCommWorld, & ! COMM
                       mpiCommRank,  & ! RANK
                       mpiIerror)      ! IERROR

  IF (mpiCommRank == 0) THEN
   masterNode = .TRUE.
  ELSE
   masterNode = .FALSE.
  END IF

  IF (masterNode) WRITE(0,*) 'MPI is running now.'

  CALL HTMPI_BARRIER(MPI_COMM_WORLD, & ! COMM
                     mpiIError)        ! IERROR

  CALL CreateObject(object = errorInfoObject)

  CALL RANDOM_SEED

  CALL RANDOM_NUMBER(arrayReal4)
  CALL RANDOM_NUMBER(arrayReal8)

  DO i=1,listLength
   arrayReal4(i) = (2.0_real4Kind*(arrayReal4(i) - 0.5_real4Kind))*15000.0_real4Kind
   arrayReal4Sorted(i) = arrayReal4(i)
   arrayReal8(i) = (2.0_real8Kind*(arrayReal8(i) - 0.5_real8Kind))*12890000.0_real8Kind
   arrayReal8Sorted(i) = arrayReal8(i)
  END DO

  errorExpected = .FALSE.
  
  CALL CreateObject(object          = sortingObject,  &
                    enableChecking  = enableChecking, &
                    errorInfoObject = errorInfoObject)

  errorFound = CheckForLocalError(errorInfoObject)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: unexpected error found.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  IF (masterNode) WRITE(0,*) 'Test: Sorting Real8 array of length ',listLength

  CALL SortArrayData(object          = sortingObject,      &
                     arrayData       = arrayReal8Sorted,   &
                     locationList    = sortedDataLocation, &
                     enableChecking  = enableChecking,     &
                     errorInfoObject = errorInfoObject)

  errorFound = CheckForLocalError(errorInfoObject)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: unexpected error found.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  WRITE(0,*)  'sorted: '
  WRITE(0,*)  ' original array? ',IsSorted(arrayReal8)
  WRITE(0,*)  ' sorted array?   ',IsSorted(arrayReal8Sorted)
  WRITE(0,*)  ' location list?  ',LocationListIsCorrect(arrayReal8,       &
                                                        arrayReal8Sorted, &
                                                        sortedDataLocation)
  
  errorExpected = .FALSE.
  errorFound    = IsSorted(arrayReal8)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: original array was sorted.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.
  errorFound    = IsSorted(arrayReal8Sorted)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: real8 array was not successfully sorted.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.
  errorFound    = LocationListIsCorrect(arrayReal8,       &
                                        arrayReal8Sorted, &
                                        sortedDataLocation)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: real8 location list was not correct.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED: real8 array sorting is correct.'
  END IF

  IF (masterNode) WRITE(0,*) 'Test: Sorting Real4 array of length ',listLength

  errorExpected = .FALSE.

  CALL SortArrayData(object          = sortingObject,      &
                     arrayData       = arrayReal4Sorted,   &
                     locationList    = sortedDataLocation, &
                     enableChecking  = enableChecking,     &
                     errorInfoObject = errorInfoObject)

  errorFound = CheckForLocalError(errorInfoObject)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: unexpected error found.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  WRITE(0,*)  'sorted: '
  WRITE(0,*)  ' original array? ',IsSorted(arrayReal4)
  WRITE(0,*)  ' sorted array?   ',IsSorted(arrayReal4Sorted)
  WRITE(0,*)  ' location list?  ',LocationListIsCorrect(arrayReal4,       &
                                                        arrayReal4Sorted, &
                                                        sortedDataLocation)
  
  errorExpected = .FALSE.
  errorFound    = IsSorted(arrayReal4)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: original array was sorted.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.
  errorFound    = IsSorted(arrayReal4Sorted)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: real4 array was not successfully sorted.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.
  errorFound    = LocationListIsCorrect(arrayReal4,       &
                                        arrayReal4Sorted, &
                                        sortedDataLocation)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: real4 location list was not correct.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED: real4 array sorting is correct.'
  END IF

! int4

  CALL RANDOM_NUMBER(arrayReal4)

  DO i=1,listLength
   arrayInt4(i) = INT((2.0_real4Kind*(arrayReal4(i) - 0.5_real4Kind))*15000.0_real4Kind)
   arrayInt4Sorted(i) = arrayInt4(i)
  END DO

  IF (masterNode) WRITE(0,*) 'Test: Sorting Int4 array of length ',listLength

  errorExpected = .FALSE.

  CALL SortArrayData(object          = sortingObject,      &
                     arrayData       = arrayInt4Sorted,   &
                     locationList    = sortedDataLocation, &
                     enableChecking  = enableChecking,     &
                     errorInfoObject = errorInfoObject)

  errorFound = CheckForLocalError(errorInfoObject)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: unexpected error found.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  WRITE(0,*)  'sorted: '
  WRITE(0,*)  ' original array? ',IsSorted(arrayInt4)
  WRITE(0,*)  ' sorted array?   ',IsSorted(arrayInt4Sorted)
  WRITE(0,*)  ' location list?  ',LocationListIsCorrect(arrayInt4,       &
                                                        arrayInt4Sorted, &
                                                        sortedDataLocation)
  
  errorExpected = .FALSE.
  errorFound    = IsSorted(arrayInt4)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: original array was sorted.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.
  errorFound    = IsSorted(arrayInt4Sorted)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: int4 array was not successfully sorted.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.
  errorFound    = LocationListIsCorrect(arrayInt4,       &
                                        arrayInt4Sorted, &
                                        sortedDataLocation)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: int4 location list was not correct.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED: int4 array sorting is correct.'
  END IF

! int8

  CALL RANDOM_NUMBER(arrayReal4)

  DO i=1,listLength
   arrayInt8(i) = INT((2.0_real4Kind*(arrayReal4(i) - 0.5_real4Kind))*15000.0_real4Kind,INT64)
   arrayInt8Sorted(i) = arrayInt8(i)
  END DO

  IF (masterNode) WRITE(0,*) 'Test: Sorting Int8 array of length ',listLength

  errorExpected = .FALSE.

  CALL SortArrayData(object          = sortingObject,      &
                     arrayData       = arrayInt8Sorted,   &
                     locationList    = sortedDataLocation, &
                     enableChecking  = enableChecking,     &
                     errorInfoObject = errorInfoObject)

  errorFound = CheckForLocalError(errorInfoObject)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: unexpected error found.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  WRITE(0,*)  'sorted: '
  WRITE(0,*)  ' original array? ',IsSorted(arrayInt8)
  WRITE(0,*)  ' sorted array?   ',IsSorted(arrayInt8Sorted)
  WRITE(0,*)  ' location list?  ',LocationListIsCorrect(arrayInt8,       &
                                                        arrayInt8Sorted, &
                                                        sortedDataLocation)
  
  errorExpected = .FALSE.
  errorFound    = IsSorted(arrayInt8)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: original array was sorted.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.
  errorFound    = IsSorted(arrayInt8Sorted)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: int8 array was not successfully sorted.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  errorExpected = .TRUE.
  errorFound    = LocationListIsCorrect(arrayInt8,       &
                                        arrayInt8Sorted, &
                                        sortedDataLocation)

  IF (errorExpected .NEQV. errorFound) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: int8 location list was not correct.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED: int8 array sorting is correct.'
  END IF

 100 CONTINUE

  CALL DestroyObject(object          = sortingObject,  &
                     enableChecking  = enableChecking, &
                     errorInfoObject = errorInfoObject)

  IF (passedTest) THEN
   IF (masterNode) THEN
    WRITE(0,999)
    OPEN(33,FILE = 'QuickSortClassTest.PASSED',FORM = 'FORMATTED')
    WRITE(33,999)
    CLOSE(33)
   END IF
  ELSE
   IF (masterNode) THEN
    WRITE(0,998)
    OPEN(33,FILE = 'QuickSortClassTest.FAILED',FORM = 'FORMATTED')
    WRITE(33,998)
    CLOSE(33)
   END IF
   CALL WriteObject(object = errorInfoObject)
  END IF

  CALL DestroyObject(object = errorInfoObject)

  CALL HTMPI_INITIALIZED(mpiIsInitialized, & ! FLAG
                         mpiIerror)        ! IERROR

  IF (mpiIsInitialized) THEN
   CALL HTMPI_FINALIZE(mpiIerror) ! IERROR
  END IF

  STOP
 998 FORMAT(1x,'FAIL:  Testing routine FAILED test.',/)
 999 FORMAT(1x,'PASS:  Testing routine PASSED all tests.',/)
END PROGRAM MAIN
