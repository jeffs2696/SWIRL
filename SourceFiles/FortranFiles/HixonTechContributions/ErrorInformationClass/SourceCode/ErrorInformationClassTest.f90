PROGRAM MAIN
  USE ErrorInformationClass
  USE MessagePassingInterface
  IMPLICIT NONE

  LOGICAL :: passedTest,    &
             masterNode,    &
             errorExpected, &
             errorFound,    &
             mpiIsInitialized

  INTEGER, PARAMETER :: mpiCommWorld  = MPI_COMM_WORLD, &
                        mpiSuccess    = MPI_SUCCESS,    &
                        mpiStatusSize = MPI_STATUS_SIZE

  INTEGER :: mpiIerror,       &
             mpiCommRank

  TYPE(ErrorInformationType) :: errorInfoObject
  TYPE(CharacterStringType) :: charStringObject

  passedTest = .TRUE.

! try to create before MPI is initialized

! WRITE(0,*) 'Error test: MPI is not initialized.'

! errorExpected = .TRUE.

! CALL CreateObject(object = errorInfoObject)

! errorFound = CheckForGlobalError(errorInfoObject) 

! IF (errorFound) THEN
!  CALL WriteObject(object = errorInfoObject)
! END IF

! IF (errorFound .EQV. errorExpected) THEN
!  WRITE(0,*) 'Test PASSED.'
!  CALL DestroyObject(object = errorInfoObject)
! ELSE
!  WRITE(0,*) 'Test FAILED: error not caught.'
!  passedTest = .FALSE.
!  GO TO 100
! END IF

! start MPI

! WRITE(0,*) 'Initializing MPI now.'

  masterNode = .TRUE. ! just-in-case initialization

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

! 3-node:  OK to here

  IF (masterNode) WRITE(0,*) 'ObjectIsInitialized? ',ObjectIsInitialized(errorInfoObject)

! continue testing

  IF (masterNode)  &
    WRITE(0,*) 'Error test: CheckForGlobalError called before CreateObject.'

  errorExpected = .TRUE. ! this is OK, but the CheckForGlobalError should
                         !  have a problem
  CALL DestroyObject(object = errorInfoObject)
  IF (masterNode) WRITE(0,*) 'ObjectIsInitialized? ',ObjectIsInitialized(errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject) 
  IF (masterNode) WRITE(0,*) 'ObjectIsInitialized? ',ObjectIsInitialized(errorInfoObject)

  IF (errorFound) THEN
   IF (masterNode) THEN
    WRITE(0,*) 'master: error found.'
   END IF
   CALL WriteObject(object = errorInfoObject)
  END IF

  CALL HTMPI_BARRIER(MPI_COMM_WORLD, & ! COMM
                     mpiIError)        ! IERROR

  IF (errorFound .EQV. errorExpected) THEN
   IF (masterNode) WRITE(0,*) 'Test PASSED.'

   CALL DestroyObject(object = errorInfoObject)

  ELSE
   IF (masterNode) WRITE(0,*) 'Test FAILED: error not caught.'
   passedTest = .FALSE.
   GO TO 100
  END IF

  CALL HTMPI_BARRIER(MPI_COMM_WORLD, & ! COMM
                     mpiIError)        ! IERROR

  IF (masterNode)  &
    WRITE(0,*) 'Error test: create object and check for error.'

  errorExpected = .FALSE.
  CALL CreateObject(object = errorInfoObject)
  errorFound = CheckForGlobalError(errorInfoObject) 

  IF (errorFound) THEN
   IF (masterNode) THEN
    WRITE(0,*) 'master: error found.'
   END IF
   CALL WriteObject(object = errorInfoObject)
  END IF

  IF (errorFound .NEQV. errorExpected) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: unexpected error found.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED.'
   CALL DestroyObject(object = errorInfoObject)
  END IF

! Need to test for an error from a non-master node

  CALL HTMPI_BARRIER(MPI_COMM_WORLD, & ! COMM
                     mpiIError)        ! IERROR

  IF (masterNode)  &
    WRITE(0,*) 'Error test: nonMasterNode error'

  errorExpected = .FALSE.
  CALL CreateObject(object = errorInfoObject)

  errorFound = CheckForGlobalError(errorInfoObject) 

  errorExpected = .TRUE.

  IF (masterNode) THEN
   CONTINUE ! no error
  ELSE
   charStringObject%charString = ' ARGH!  Horrible error detected! ACK!'
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)
   IF (mpiCommRank == 1) THEN
    charStringObject%charString = ' CALLed from Shangri-La: '
    CALL AddErrorInformation(object          = errorInfoObject, &
                             errorInfoString = charStringObject)
   END IF       
  END IF

! OK to here

  CALL HTMPI_BARRIER(MPI_COMM_WORLD, & ! COMM
                     mpiIError)        ! IERROR

  errorFound = CheckForGlobalError(errorInfoObject) 

  CALL HTMPI_BARRIER(MPI_COMM_WORLD, & ! COMM
                     mpiIError)        ! IERROR

  IF (errorFound) THEN
   IF (masterNode) THEN
    WRITE(0,*) 'master: error found.'
   END IF
   CALL WriteObject(object = errorInfoObject)
  END IF

  CALL HTMPI_BARRIER(MPI_COMM_WORLD, & ! COMM
                     mpiIError)        ! IERROR

  IF (errorFound .NEQV. errorExpected) THEN
   IF (masterNode) WRITE(0,*) 'Test FAILED: unexpected error found.'
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(0,*) 'Test PASSED.'
   CALL DestroyObject(object = errorInfoObject)
  END IF

 100 CONTINUE

  IF (passedTest) THEN
   IF (masterNode) THEN
    WRITE(0,999)
    OPEN(33,FILE = 'ErrorInformationClassTest.PASSED',FORM = 'FORMATTED')
    WRITE(33,999)
    CLOSE(33)
   END IF
  ELSE
   IF (masterNode) THEN
    WRITE(0,998)
    OPEN(33,FILE = 'ErrorInformationClassTest.FAILED',FORM = 'FORMATTED')
    WRITE(33,998)
    CLOSE(33)
   END IF
  END IF

  CALL HTMPI_INITIALIZED(mpiIsInitialized, & ! FLAG
                         mpiIerror)        ! IERROR

  IF (mpiIsInitialized) THEN
   CALL HTMPI_FINALIZE(mpiIerror) ! IERROR
  END IF

  STOP

 998 FORMAT(1x,'FAIL:  Testing routine FAILED test.',/)
 999 FORMAT(1x,'PASS:  Testing routine PASSED all tests.',/)

END PROGRAM MAIN
