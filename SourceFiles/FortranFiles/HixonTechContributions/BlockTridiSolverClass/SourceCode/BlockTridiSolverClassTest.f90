PROGRAM MAIN
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE BlockTridiSolverClass
  USE ErrorInformationClass
  USE MessagePassingInterface
  USE BlockTridiSolverClassTest_General
  USE BlockTridiSolverClassTest_1D
  USE BlockTridiSolverClassTest_2D
  USE BlockTridiSolverClassTest_3D
  USE BlockTridiSolverClassTest_4D
  IMPLICIT NONE

  LOGICAL :: enableChecking = .TRUE.

  LOGICAL :: passedTest,    &
             masterNode,    &
             errorExpected, &
             errorFound,    &
             mpiIsInitialized

  INTEGER, PARAMETER :: mpiCommWorld  = MPI_COMM_WORLD,  &
                        mpiSuccess    = MPI_SUCCESS,     &
                        mpiStatusSize = MPI_STATUS_SIZE, &
                        minDim        = 2,               &
                        maxDim        = 4,               &
                        rDef          = REAL64

  INTEGER :: mpiIerror,       &
             mpiCommRank

  TYPE(BlockTridiSolverType) :: blockTridiSolverObject

  TYPE(ErrorInformationType) :: errorInfoObject
! TYPE(CharacterStringType) :: charStringObject

  REAL(KIND=rDef) :: errorTolerance = 5.0e-09_rDef

  passedTest = .TRUE.

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

! create the ErrorInformationObject

  errorExpected = .FALSE.
  CALL CreateObject(object = errorInfoObject)
  errorFound = CheckForGlobalError(errorInfoObject) 

  IF (errorFound .NEQV. errorExpected) THEN
   IF (masterNode) THEN
    WRITE(0,*) 'master: error found.'
    passedTest = .FALSE.
    GO TO 100
   END IF
   CALL WriteObject(object = errorInfoObject)
  END IF

  CALL BlockTridiSolverClassTestGeneral(blockTridiSolverObject = blockTridiSolverObject, &
                                        masterNode             = masterNode,          &
                                        enableChecking         = enableChecking,      &
                                        errorInfoObject        = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL BlockTridiSolverClassTest1D(blockTridiSolverObject = blockTridiSolverObject, &
                                   masterNode             = masterNode,          &
                                   errorTolerance         = errorTolerance,      &
                                   enableChecking         = enableChecking,      &
                                   errorInfoObject        = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL BlockTridiSolverClassTest2D(blockTridiSolverObject = blockTridiSolverObject, &
                                   masterNode             = masterNode,          &
                                   errorTolerance         = errorTolerance,      &
                                   enableChecking         = enableChecking,      &
                                   errorInfoObject        = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL BlockTridiSolverClassTest3D(blockTridiSolverObject = blockTridiSolverObject, &
                                   masterNode             = masterNode,          &
                                   errorTolerance         = errorTolerance,      &
                                   enableChecking         = enableChecking,      &
                                   errorInfoObject        = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL BlockTridiSolverClassTest4D(blockTridiSolverObject = blockTridiSolverObject, &
                                   masterNode             = masterNode,          &
                                   errorTolerance         = errorTolerance,      &
                                   enableChecking         = enableChecking,      &
                                   errorInfoObject        = errorInfoObject)

  IF (CheckForGlobalError(errorInfoObject)) THEN
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

 100 CONTINUE

  IF (passedTest) THEN
   IF (masterNode) THEN
    WRITE(0,999)
    OPEN(33,FILE = 'BlockTridiSolverClassTest.PASSED',FORM = 'FORMATTED')
    WRITE(33,999)
    CLOSE(33)
   END IF
  ELSE
   IF (masterNode) THEN
    WRITE(0,998)
    OPEN(33,FILE = 'BlockTridiSolverClassTest.FAILED',FORM = 'FORMATTED')
    WRITE(33,998)
    CLOSE(33)
   END IF
  END IF

  errorFound = CheckForGlobalError(errorInfoObject) 
  IF (errorFound) THEN
   CALL WriteObject(object = errorInfoObject)
  ELSE
   CONTINUE
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
