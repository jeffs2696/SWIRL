PROGRAM MessagePassingInterfaceTest
  USE MessagePassingInterface
  IMPLICIT NONE

  INTEGER, PARAMETER :: mpiCommWorld  = MPI_COMM_WORLD, &
                        mpiSuccess    = MPI_SUCCESS,    &
                        mpiStatusSize = MPI_STATUS_SIZE

  INTEGER :: mpiIerror,           &
             mpiCommSize,         &
             mpiCommRank,         &
             mpiVersion,          &
             mpiSubVersion,       &
             mpiCount,            &
             mpiSendCount,        &
             mpiRecvCount,        &
             mpiDatatype,         &
             mpiSendDatatype,     &
             mpiRecvDatatype,     &
             mpiRoot,             &
             mpiSendTag,          &
             mpiRecvTag,          &
             mpiTag,              &
             mpiDest,             &
             mpiSource,           &
             mpiRequest,          &
             n,                   &
             countTest,           &
             sourceTest,          &
             tagTest,             &
             mpiCharacterTypeDef, &
             mpiInt4TypeDef,      & 
             mpiInt8TypeDef,      &
             mpiLogicalTypeDef,   &
             mpiReal4TypeDef,     &
             mpiReal8TypeDef

!            errorTest,           &

  INTEGER, DIMENSION(mpiStatusSize) :: mpiStatus

  LOGICAL :: passedTest = .TRUE.,  &
             masterNode = .FALSE., &
             mpiFlag,              &
             isEqual

  DOUBLE PRECISION :: initialTime,finalTime,timePrecision

! message buffer data

  CHARACTER :: charVar1, charVar2

  CHARACTER, DIMENSION(5) :: charArray1, charArray2

  LOGICAL :: logicalVar1, logicalVar2

  LOGICAL, DIMENSION(4) :: logicalArray1, logicalArray2

  INTEGER(KIND=int4Kind) :: int4Var1, int4Var2

  INTEGER(KIND=int4Kind), DIMENSION(5) :: int4Array1, int4Array2

  INTEGER(KIND=int8Kind) :: int8Var1,int8Var2

  INTEGER(KIND=int8Kind), DIMENSION(5) :: int8Array1, int8Array2

  REAL(KIND=real4Kind) :: real4Var1, real4Var2

  REAL(KIND=real4Kind), DIMENSION(5) :: real4Array1, real4Array2

  REAL(KIND=real8Kind) :: real8Var1, real8Var2

  REAL(KIND=real8Kind), DIMENSION(5) :: real8Array1, real8Array2

  REAL(KIND=real8Kind), DIMENSION(3) :: real8Array1SR1,real8Array1SR2,real8Array1SRT
  REAL(KIND=real8Kind), DIMENSION(5) :: real8Array2SR1,real8Array2SR2,real8Array2SRT

! MPI_STRUCT data

! removing int8 data

! INTEGER, PARAMETER :: numParts1 = 8, &
!                       numParts2 = 5

  INTEGER, PARAMETER :: numParts1 = 9, &
                        numParts2 = 6

  INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(numParts1) :: dataAddressDeltas1
  INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(numParts2) :: dataAddressDeltas2

  INTEGER(KIND=MPI_ADDRESS_KIND) :: initialDataAddress1, &
                                    initialDataAddress2
                                     

  INTEGER, DIMENSION(numParts1) :: dataLength1, &
                                   dataType1
  INTEGER, DIMENSION(numParts2) :: dataLength2, &
                                   dataType2

  INTEGER :: dataStructureType1, &
             dataStructureType2

! start timing now

! initialTime = HTMPI_WTIME()

  DO n=1,SIZE(mpiStatus)
   mpiStatus(n) = mpiSuccess
  END DO

! testing MPI_INITIALIZED call (1)

  CALL HTMPI_INITIALIZED(mpiFlag,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL: error in MPI_INITIALIZED call (1) IERROR = ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

! expect a .FALSE. result, since MPI_INIT has not been called

  IF (mpiFlag) THEN
   WRITE(6,*) 'FAIL: MPI_INITIALIZED call (1) returned incorrect result.'
   passedTest = .FALSE.
   GO TO 100
  END IF

  CALL HTMPI_INIT(mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   passedTest = .FALSE.
   GO TO 100
  END IF

  CALL HTMPI_COMM_RANK(mpiCommWorld,mpiCommRank,mpiIerror)

! start timing now

  initialTime = HTMPI_WTIME()

  IF (mpiCommRank == 0) masterNode = .TRUE.

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_COMM_RANK call IERROR = ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,103) 
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_INIT call. '
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_COMM_RANK call.'
  END IF

! testing MPI_INITIALIZED call (2)

  CALL HTMPI_INITIALIZED(mpiFlag,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   IF (masterNode) WRITE(6,*) 'FAIL:  MPI_INITIALIZED call (2) IERROR = ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

! expect a .TRUE. result, since MPI_INIT has been called

  IF (mpiFlag) THEN
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_INITIALIZED tests 1 and 2.'
  ELSE
   IF (masterNode) WRITE(6,*) 'FAIL:  MPI_INITIALIZED call (2) returned incorrect result.'
   passedTest = .FALSE.
   GO TO 100
  END IF

  CALL HTMPI_GET_VERSION(mpiVersion,mpiSubVersion,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_GET_VERSION call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_GET_VERSION call.'
  END IF

  CALL HTMPI_COMM_SIZE(mpiCommWorld,mpiCommSize,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_COMM_SIZE call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_COMM_SIZE call.'
  END IF

  IF (masterNode) WRITE(6,*) 
  IF (masterNode) WRITE(6,*) 'MPI Implementation Parameters: '
  IF (masterNode) WRITE(6,*) 

  IF (masterNode) WRITE(6,105) mpiVersion,mpiSubVersion

  IF (masterNode) WRITE(6,104) mpiCommSize

  timePrecision = HTMPI_WTICK()
  IF (masterNode) WRITE(6,101) timePrecision
  IF (masterNode) WRITE(6,*) 

!-----------MPIKindDefs testing-------------------------------

  mpiLogicalTypeDef   = GetMPITypeDef(logicalVar1)
  mpiCharacterTypeDef = GetMPITypeDef(charVar1)
  mpiInt4TypeDef      = GetMPITypeDef(int4Var1)

! taking this out for the moment.

  mpiInt8TypeDef      = GetMPITypeDef(int8Var1)

  mpiReal4TypeDef     = GetMPITypeDef(real4Var1)
  mpiReal8TypeDef     = GetMPITypeDef(real8Var1)

  IF (masterNode) THEN
   WRITE(6,*) 'Logical type def = ', &
    GetMPITypeDef(logicalVar1)
   WRITE(6,*) 'Character type def = ', &
    GetMPITypeDef(charVar1)
   WRITE(6,*) 'Int4 type def = ', &
    GetMPITypeDef(int4Var1)

   WRITE(6,*) 'Int8 type def = ', &
    GetMPITypeDef(int8Var1)

   WRITE(6,*) 'Real4 type def = ', &
    GetMPITypeDef(real4Var1)
   WRITE(6,*) 'Real8 type def = ', &
    GetMPITypeDef(real8Var1)

   WRITE(6,*) 'trying again: '

   WRITE(6,*) 'Logical type def = ', &
    GetMPITypeDef(logicalVar1)
   WRITE(6,*) 'Character type def = ', &
    GetMPITypeDef(charVar1)
   WRITE(6,*) 'Int4 type def = ', &
    GetMPITypeDef(int4Var1)

   WRITE(6,*) 'Int8 type def = ', &
    GetMPITypeDef(int8Var1)

   WRITE(6,*) 'Real4 type def = ', &
    GetMPITypeDef(real4Var1)
   WRITE(6,*) 'Real8 type def = ', &
    GetMPITypeDef(real8Var1)
  END IF

!------------------CharacterArray------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  CharacterArray message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   charArray1 = (/'A','B','C','D','E'/)
   charArray2 = (/'B','C','D','E','F'/)
  ELSE
   charArray1 = (/'C','D','E','F','G'/)
   charArray2 = (/'D','E','F','G','H'/)
  END IF

  mpiCount = 5
  mpiDatatype = mpiCharacterTypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(charArray1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (characterArray) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (characterArray) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 1
   CALL HTMPI_SEND(charArray1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (characterArray) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 1
   CALL HTMPI_RECV(charArray2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (characterArray) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (charArray1(n) == charArray2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (characterArray) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (characterArray) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (characterArray) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (characterArray) call.'
    WRITE(6,*) ' expected ',charArray1,'; recved ',charArray2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   charArray1 = (/'E','F','G','H','I'/)
   charArray2 = (/'F','G','H','I','J'/)
  ELSE
   charArray1 = (/'G','H','I','J','K'/)
   charArray2 = (/'H','I','J','K','L'/)
  END IF

  mpiCount = 5
  mpiDatatype = mpiCharacterTypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(charArray2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (characterArray) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 2
   CALL HTMPI_ISEND(charArray2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (characterArray) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 2
   CALL HTMPI_IRECV(charArray1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (characterArray) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (charArray1(n) == charArray2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (characterArray) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (characterArray) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (characterArray) call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (characterArray) call.'
    WRITE(6,*) ' expected ',charArray2,'; recved ',charArray1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  CharacterArray message passing.'
   WRITE(6,*)
  END IF

!------------------CharacterVariable---------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  CharacterVariable message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   charVar1 = 'A'
   charVar2 = 'B'
  ELSE
   charVar1 = 'C'
   charVar2 = 'D'
  END IF

  mpiCount = 1
  mpiDatatype = mpiCharacterTypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(charVar1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (characterVariable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (characterVariable) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 3
   CALL HTMPI_SEND(charVar1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (characterVariable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 3
   CALL HTMPI_RECV(charVar2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (characterVariable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (charVar2 == charVar1) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (characterVariable) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (characterVariable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (characterVariable) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (characterVariable) call.'
    WRITE(6,*) ' expected A; recved ',charVar2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   charVar1 = 'E'
   charVar2 = 'F'
  ELSE
   charVar1 = 'G'
   charVar2 = 'H'
  END IF

  mpiCount = 1
  mpiDatatype = mpiCharacterTypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(charVar2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (characterVariable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 4
   CALL HTMPI_ISEND(charVar2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (characterVariable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 4
   CALL HTMPI_IRECV(charVar1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (characterVariable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

 997 CONTINUE
    CALL HTMPI_TEST(mpiRequest,  &
                    mpiFlag,     &
                    mpiStatus,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_TEST call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (.NOT. mpiFlag) GO TO 997

! check for successful completion

   IF (charVar2 == charVar1) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (characterVariable) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (characterVariable) call.'
    WRITE(6,*) 'PASS:  MPI_WAIT call.'
    WRITE(6,*) 'PASS:  MPI_TEST call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (characterVariable) call.'
    WRITE(6,*) 'FAIL:  MPI_WAIT call.'
    WRITE(6,*) 'FAIL:  MPI_TEST call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (characterVariable) call.'
    WRITE(6,*) ' expected F; recved ',charVar1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  CharacterVariable message passing.'
   WRITE(6,*)
  END IF

!------------------LogicalArray------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  LogicalArray message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   logicalArray1 = (/.TRUE.,.FALSE.,.TRUE.,.FALSE./)
   logicalArray2 = (/.FALSE.,.TRUE.,.FALSE.,.TRUE./)
  ELSE
   logicalArray1 = (/.TRUE.,.TRUE.,.TRUE.,.FALSE./)
   logicalArray2 = (/.FALSE.,.TRUE.,.TRUE.,.TRUE./)
  END IF

  mpiCount = 4
  mpiDatatype = mpiLogicalTypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(logicalArray1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (logicalArray) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (logicalArray) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 5
   CALL HTMPI_SEND(logicalArray1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (logicalArray) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 5
   CALL HTMPI_RECV(logicalArray2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (logicalArray) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (logicalArray1(n) .EQV. logicalArray2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (logicalArray) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (logicalArray) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (logicalArray) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (logicalArray) call.'
    WRITE(6,*) ' expected ',logicalArray1,'; recved ',logicalArray2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   logicalArray1 = (/.TRUE.,.TRUE.,.TRUE.,.FALSE./)
   logicalArray2 = (/.FALSE.,.TRUE.,.TRUE.,.TRUE./)
  ELSE
   logicalArray1 = (/.TRUE.,.FALSE.,.TRUE.,.FALSE./)
   logicalArray2 = (/.FALSE.,.TRUE.,.FALSE.,.TRUE./)
  END IF

  mpiCount = 4
  mpiDatatype = mpiLogicalTypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(logicalArray2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (logicalArray) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 6
   CALL HTMPI_ISEND(logicalArray2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (logicalArray) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 6
   CALL HTMPI_IRECV(logicalArray1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (logicalArray) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (logicalArray1(n) .EQV. logicalArray2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (logicalArray) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (logicalArray) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (logicalArray) call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (logicalArray) call.'
    WRITE(6,*) ' expected ',logicalArray2,'; recved ',logicalArray1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  LogicalArray message passing.'
   WRITE(6,*)
  END IF

!------------------LogicalVariable---------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  LogicalVariable message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   logicalVar1 = .TRUE.
   logicalVar2 = .TRUE.
  ELSE
   logicalVar1 = .FALSE.
   logicalVar2 = .FALSE.
  END IF

  mpiCount = 1
  mpiDatatype = mpiLogicalTypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(logicalVar1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (logicalVariable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (logicalVariable) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 7
   CALL HTMPI_SEND(logicalVar1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (logicalVariable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 7
   CALL HTMPI_RECV(logicalVar2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (logicalVariable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (logicalVar2 .EQV. logicalVar1) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (logicalVariable) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (logicalVariable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (logicalVariable) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (logicalVariable) call.'
    WRITE(6,*) ' expected A; recved ',logicalVar2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   logicalVar1 = .FALSE.
   logicalVar2 = .FALSE.
  ELSE
   logicalVar1 = .TRUE.
   logicalVar2 = .TRUE.
  END IF

  mpiCount = 1
  mpiDatatype = mpiLogicalTypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(logicalVar2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (logicalVariable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 8
   CALL HTMPI_ISEND(logicalVar2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (logicalVariable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 8
   CALL HTMPI_IRECV(logicalVar1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (logicalVariable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (logicalVar2 .EQV. logicalVar1) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (logicalVariable) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (logicalVariable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (logicalVariable) call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (logicalVariable) call.'
    WRITE(6,*) ' expected ',logicalVar2,'; recved ',logicalVar1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  LogicalVariable message passing.'
   WRITE(6,*)
  END IF

!------------------Integer4Array------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  Integer4Array message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   int4Array1 = (/1_int4Kind, 2_int4Kind, 3_int4Kind, 4_int4Kind, 5_int4Kind/)
   int4Array2 = (/2_int4Kind, 3_int4Kind, 4_int4Kind, 5_int4Kind, 6_int4Kind/)
  ELSE
   int4Array1 = (/3_int4Kind, 4_int4Kind, 5_int4Kind, 6_int4Kind, 7_int4Kind/)
   int4Array2 = (/4_int4Kind, 5_int4Kind, 6_int4Kind, 7_int4Kind, 8_int4Kind/)
  END IF

  mpiCount = 5
  mpiDatatype = MPI_INTEGER
  mpiRoot = 0

  CALL HTMPI_BCAST(int4Array1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (integer4Array) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (integer4Array) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 9
   CALL HTMPI_SEND(int4Array1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (integer4Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 9
   CALL HTMPI_RECV(int4Array2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (integer4Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (int4Array1(n) == int4Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (integer4Array) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (integer4Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (integer4Array) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (integer4Array) call.'
    WRITE(6,*) ' expected ',int4Array1,'; recved ',int4Array2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   int4Array1 = (/3_int4Kind, 4_int4Kind, 5_int4Kind, 6_int4Kind, 7_int4Kind/)
   int4Array2 = (/4_int4Kind, 5_int4Kind, 6_int4Kind, 7_int4Kind, 8_int4Kind/)
  ELSE
   int4Array1 = (/1_int4Kind, 2_int4Kind, 3_int4Kind, 4_int4Kind, 5_int4Kind/)
   int4Array2 = (/2_int4Kind, 3_int4Kind, 4_int4Kind, 5_int4Kind, 6_int4Kind/)
  END IF

  mpiCount = 5
  mpiDatatype = MPI_INTEGER
  mpiRoot = 1

  CALL HTMPI_BCAST(int4Array2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (integer4Array) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 10
   CALL HTMPI_ISEND(int4Array2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (integer4Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 10
   CALL HTMPI_IRECV(int4Array1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (integer4Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (int4Array1(n) == int4Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (integer4Array) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (integer4Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (integer4Array) call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (integer4Array) call.'
    WRITE(6,*) ' expected ',int4Array2,'; recved ',int4Array1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  Integer4Array message passing.'
   WRITE(6,*)
  END IF

!------------------Integer4Variable---------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  Integer4Variable message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   int4Var1 = 1_int4Kind
   int4Var2 = -21_int4Kind
  ELSE
   int4Var1 = 23_int4Kind
   int4Var2 = 42_int4Kind
  END IF

  mpiCount = 1
  mpiDatatype = MPI_INTEGER
  mpiRoot = 0

  CALL HTMPI_BCAST(int4Var1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (integer4Variable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (integer4Variable) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 11
   CALL HTMPI_SEND(int4Var1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (integer4Variable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 11
   CALL HTMPI_RECV(int4Var2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (integer4Variable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (int4Var2 == int4Var1) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (integer4Variable) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (integer4Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (integer4Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (integer4Variable) call.'
    WRITE(6,*) ' expected A; recved ',int4Var2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   int4Var1 = 203_int4Kind
   int4Var2 = 420_int4Kind
  ELSE
   int4Var1 = -23_int4Kind
   int4Var2 = -42_int4Kind
  END IF

  mpiCount = 1
  mpiDatatype = MPI_INTEGER
  mpiRoot = 1

  CALL HTMPI_BCAST(int4Var2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (integer4Variable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 12
   CALL HTMPI_ISEND(int4Var2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (integer4Variable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 12
   CALL HTMPI_IRECV(int4Var1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (integer4Variable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_TEST call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (int4Var2 == int4Var1) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (integer4Variable) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (integer4Variable) call.'
    WRITE(6,*) 'PASS:  MPI_WAIT call.'
    WRITE(6,*) 'PASS:  MPI_TEST call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (integer4Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_WAIT call.'
    WRITE(6,*) 'FAIL:  MPI_TEST call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (integer4Variable) call.'
    WRITE(6,*) ' expected ',int4Var2,'; recved ',int4Var1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  Integer4Variable message passing.'
   WRITE(6,*)
  END IF

!------------------Integer8Array------------------------------
! initially testing BCAST from master and blocking send/recv back

! IF (passedTest .AND. masterNode) THEN
!  WRITE(6,*)
!  WRITE(6,*) 'WARNING: Integer8 message passing has been disabled.'
!  WRITE(6,*)
!  GO TO 331
! END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  Integer8Array message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   int8Array1 = (/1_int8Kind, 2_int8Kind, 3_int8Kind, 4_int8Kind, 5_int8Kind/)
   int8Array2 = (/2_int8Kind, 3_int8Kind, 4_int8Kind, 5_int8Kind, 6_int8Kind/)
  ELSE
   int8Array1 = (/3_int8Kind, 4_int8Kind, 5_int8Kind, 6_int8Kind, 7_int8Kind/)
   int8Array2 = (/4_int8Kind, 5_int8Kind, 6_int8Kind, 7_int8Kind, 8_int8Kind/)
  END IF

  mpiCount = 5
  mpiDatatype = mpiInt8TypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(int8Array1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (integer8Array) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (integer8Array) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 9
   CALL HTMPI_SEND(int8Array1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (integer8Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 9
   CALL HTMPI_RECV(int8Array2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (integer8Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (int8Array1(n) == int8Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (integer8Array) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (integer8Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (integer8Array) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (integer8Array) call.'
    WRITE(6,*) ' expected ',int8Array1,'; recved ',int8Array2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   int8Array1 = (/3_int8Kind, 4_int8Kind, 5_int8Kind, 6_int8Kind, 7_int8Kind/)
   int8Array2 = (/4_int8Kind, 5_int8Kind, 6_int8Kind, 7_int8Kind, 8_int8Kind/)
  ELSE
   int8Array1 = (/1_int8Kind, 2_int8Kind, 3_int8Kind, 4_int8Kind, 5_int8Kind/)
   int8Array2 = (/2_int8Kind, 3_int8Kind, 4_int8Kind, 5_int8Kind, 6_int8Kind/)
  END IF

  mpiCount = 5
  mpiDatatype = mpiInt8TypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(int8Array2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (integer8Array) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 10
   CALL HTMPI_ISEND(int8Array2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (integer8Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 10
   CALL HTMPI_IRECV(int8Array1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (integer8Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (int8Array1(n) == int8Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (integer8Array) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (integer8Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (integer8Array) call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (integer8Array) call.'
    WRITE(6,*) ' expected ',int8Array2,'; recved ',int8Array1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  Integer8Array message passing.'
   WRITE(6,*)
  END IF

!------------------Integer8Variable---------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  Integer8Variable message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   int8Var1 = 1_int8Kind
   int8Var2 = -21_int8Kind
  ELSE
   int8Var1 = 23_int8Kind
   int8Var2 = 42_int8Kind
  END IF

  mpiCount = 1
  mpiDatatype = mpiInt8TypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(int8Var1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (integer8Variable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (integer8Variable) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 11
   CALL HTMPI_SEND(int8Var1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (integer8Variable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 11
   CALL HTMPI_RECV(int8Var2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (integer8Variable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (int8Var2 == int8Var1) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (integer8Variable) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (integer8Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (integer8Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (integer8Variable) call.'
    WRITE(6,*) ' expected A; recved ',int8Var2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   int8Var1 = 203_int8Kind
   int8Var2 = 420_int8Kind
  ELSE
   int8Var1 = -23_int8Kind
   int8Var2 = -42_int8Kind
  END IF

  mpiCount = 1
  mpiDatatype = mpiInt8TypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(int8Var2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (integer8Variable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 12
   CALL HTMPI_ISEND(int8Var2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (integer8Variable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 12
   CALL HTMPI_IRECV(int8Var1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (integer8Variable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_TEST call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (int8Var2 == int8Var1) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (integer8Variable) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (integer8Variable) call.'
    WRITE(6,*) 'PASS:  MPI_WAIT call.'
    WRITE(6,*) 'PASS:  MPI_TEST call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (integer8Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_WAIT call.'
    WRITE(6,*) 'FAIL:  MPI_TEST call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (integer8Variable) call.'
    WRITE(6,*) ' expected ',int8Var2,'; recved ',int8Var1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  Integer8Variable message passing.'
   WRITE(6,*)
  END IF

!331 CONTINUE

!------------------Real4Array------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  Real4Array message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   real4Array1 = (/1.1_real4Kind, 2.1_real4Kind, 3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind/)
   real4Array2 = (/2.1_real4Kind, 3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind/)
  ELSE
   real4Array1 = (/3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind, 7.1_real4Kind/)
   real4Array2 = (/4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind, 7.1_real4Kind, 8.1_real4Kind/)
  END IF

  mpiCount = 5
  mpiDatatype = mpiReal4TypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(real4Array1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (real4Array) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (real4Array) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 17
   CALL HTMPI_SEND(real4Array1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (real4Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 17
   CALL HTMPI_RECV(real4Array2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (real4Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (real4Array1(n) == real4Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (real4Array) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (real4Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (real4Array) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (real4Array) call.'
    WRITE(6,*) ' expected ',real4Array1,'; recved ',real4Array2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   real4Array1 = (/3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind, 7.1_real4Kind/)
   real4Array2 = (/4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind, 7.1_real4Kind, 8.1_real4Kind/)
  ELSE
   real4Array1 = (/1.1_real4Kind, 2.1_real4Kind, 3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind/)
   real4Array2 = (/2.1_real4Kind, 3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind/)
  END IF

  mpiCount = 5
  mpiDatatype = mpiReal4TypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(real4Array2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (real4Array) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 18
   CALL HTMPI_ISEND(real4Array2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (real4Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 18

   CALL HTMPI_PROBE(mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiStatus,    &
                    mpiIerror)

    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_PROBE (real4Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF

   CALL HTMPI_IRECV(real4Array1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (real4Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (real4Array1(n) == real4Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (real4Array) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (real4Array) call.'
    WRITE(6,*) 'PASS:  MPI_PROBE call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (real4Array) call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (real4Array) call.'
    WRITE(6,*) 'FAIL:  MPI_PROBE call.'
    WRITE(6,*) ' expected ',real4Array2,'; recved ',real4Array1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  Real4Array message passing.'
   WRITE(6,*)
  END IF

!------------------Real4Variable---------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  Real4Variable message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   real4Var1 = 1.1_real4Kind
   real4Var2 = -21.1_real4Kind
  ELSE
   real4Var1 = 23.1_real4Kind
   real4Var2 = 42.1_real4Kind
  END IF

  mpiCount = 1
  mpiDatatype = mpiReal4TypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(real4Var1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (real4Variable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (real4Variable) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 19
   CALL HTMPI_SEND(real4Var1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (real4Variable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 19
   CALL HTMPI_RECV(real4Var2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (real4Variable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (real4Var2 == real4Var1) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (real4Variable) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (real4Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (real4Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (real4Variable) call.'
    WRITE(6,*) ' expected A; recved ',real4Var2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   real4Var1 = 203.1_real4Kind
   real4Var2 = 420.1_real4Kind
  ELSE
   real4Var1 = -23.1_real4Kind
   real4Var2 = -42.1_real4Kind
  END IF

  mpiCount = 1
  mpiDatatype = mpiReal4TypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(real4Var2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (real4Variable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 20
   CALL HTMPI_ISEND(real4Var2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (real4Variable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 20
   CALL HTMPI_IRECV(real4Var1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (real4Variable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_TEST call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (real4Var2 == real4Var1) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (real4Variable) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (real4Variable) call.'
    WRITE(6,*) 'PASS:  MPI_WAIT call.'
    WRITE(6,*) 'PASS:  MPI_TEST call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (real4Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_WAIT call.'
    WRITE(6,*) 'FAIL:  MPI_TEST call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (real4Variable) call.'
    WRITE(6,*) ' expected ',real4Var2,'; recved ',real4Var1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  Real4Variable message passing.'
   WRITE(6,*)
  END IF

!------------------Real8Array------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  Real8Array message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   real8Array1 = (/1.23_real8Kind, 2.23_real8Kind, 3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind/)
   real8Array2 = (/2.23_real8Kind, 3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind/)
   real8Array1SRT = (/1.35_real8Kind,2.35_real8Kind,3.35_real8Kind/)
   real8Array2SRT = (/1.45_real8Kind,2.45_real8Kind,3.45_real8Kind,4.45_real8Kind,5.45_real8Kind/)
  ELSE
   real8Array1 = (/3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind, 7.23_real8Kind/)
   real8Array2 = (/4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind, 7.23_real8Kind, 8.23_real8Kind/)
   real8Array1SRT = (/1.35_real8Kind,2.35_real8Kind,3.35_real8Kind/)
   real8Array2SRT = (/1.45_real8Kind,2.45_real8Kind,3.45_real8Kind,4.45_real8Kind,5.45_real8Kind/)
  END IF

  mpiCount = 5
  mpiDatatype = mpiReal8TypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(real8Array1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (real8Array) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (real8Array) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 21
   CALL HTMPI_SEND(real8Array1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (real8Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 21
   CALL HTMPI_RECV(real8Array2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (real8Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (real8Array1(n) == real8Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (real8Array) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (real8Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (real8Array) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (real8Array) call.'
    WRITE(6,*) ' expected ',real8Array1,'; recved ',real8Array2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   real8Array1 = (/3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind, 7.23_real8Kind/)
   real8Array2 = (/4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind, 7.23_real8Kind, 8.23_real8Kind/)
  ELSE
   real8Array1 = (/1.23_real8Kind, 2.23_real8Kind, 3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind/)
   real8Array2 = (/2.23_real8Kind, 3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind/)
  END IF

  mpiCount = 5
  mpiDatatype = mpiReal8TypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(real8Array2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (real8Array) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 22
   CALL HTMPI_ISEND(real8Array2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (real8Array) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 22
   CALL HTMPI_IRECV(real8Array1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    MPI_ANY_SOURCE,    &
                    MPI_ANY_TAG,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (real8Array) call 1: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_WAIT (real8Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (real8Array1(n) == real8Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO
 
! try MPI diagnostics

   CALL HTMPI_GET_COUNT(mpiStatus,   &
                        mpiDatatype, &
                        countTest,   &
                        mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_GET_COUNT (real8Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (countTest /= mpiCount) THEN
    WRITE(6,*) 'FAIL:  error in MPI_GET_COUNT (real8Array) call: wrong count.'
    passedTest = .FALSE.
    GO TO 100
   END IF

   sourceTest = mpiStatus(MPI_SOURCE)
   tagTest    = mpiStatus(MPI_TAG)
!  errorTest  = mpiStatus(MPI_ERROR)

   WRITE(6,*) '------------------'
   WRITE(6,*) 'MPI_STATUS_SIZE = ',mpiStatusSize
   WRITE(6,*) 'MPI_SOURCE      = ',MPI_SOURCE,sourceTest
   WRITE(6,*) 'MPI_TAG         = ',MPI_TAG,tagTest
!  WRITE(6,*) 'mpiStatus       = ',mpiStatus
!  WRITE(6,*) 'MPI_ERROR       = ',MPI_ERROR,errorTest
   WRITE(6,*) '------------------'

   IF (sourceTest /= mpiSource) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (real8Array) call: wrong source.'
    passedTest = .FALSE.
    GO TO 100
   END IF

   IF (tagTest /= mpiTag) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (real8Array) call: wrong tag.'
    passedTest = .FALSE.
    GO TO 100
   END IF

!  IF (errorTest /= mpiSuccess) THEN
!   WRITE(6,*) 'WARNING:  error in MPI_IRECV (real8Array) call 2: status error.'
!   WRITE(6,*) 'status = ',errorTest
!   WRITE(6,*) 'status of the call, however, is OK -- there is no error: ',mpiIerror    
!   passedTest = .FALSE.
!   GO TO 100
!  END IF

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (real8Array) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (real8Array) call.'
    WRITE(6,*) 'PASS:  MPI_GET_COUNT (real8Array) call.'
    WRITE(6,*) 'PASS:  Wildcard source and tag (real8Array) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (real8Array) call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (real8Array) call.'
    WRITE(6,*) 'FAIL:  MPI_GET_COUNT (real8Array) call.'
    WRITE(6,*) 'FAIL:  Wildcard source and tag (real8Array) call.'
    WRITE(6,*) ' expected ',real8Array2,'; recved ',real8Array1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! test the MPI_SENDRECV routines

! two SENDRECV calls:  first time, the master node
!  sends the 5-long array and recvs the 3-long array.
!
! then, the master node recvs the 5-long array and
!  sends the 3-long array.

  IF (mpiCommRank == 1) THEN
! slave
   DO n=1,3
    real8Array1SR2(n) = real8Array1SRT(n)
   END DO 
   DO n=1,5
    real8Array2SR2(n) = 0.0_real8Kind
   END DO 

   mpiDest      = 0
   mpiSendTag   = 44
   mpiSendCount = 3
   mpiSource    = 0
   mpiRecvTag   = 47
   mpiRecvCount = 5
   mpiSendDatatype  = mpiReal8TypeDef
   mpiRecvDatatype  = mpiReal8TypeDef

   CALL HTMPI_SENDRECV(real8Array1SR2,  &
                       mpiSendCount,    &
                       mpiSendDatatype, &
                       mpiDest,         &
                       mpiSendTag,      &
                       real8Array2SR2,  &
                       mpiRecvCount,    &
                       mpiRecvDatatype, &
                       mpiSource,       &
                       mpiRecvTag,      &
                       mpiCommWorld,    &
                       mpiStatus,       &
                       mpiIError)

! now go the other way

   DO n=1,3
    real8Array1SR2(n) = 0.0_real8Kind
   END DO 

   mpiDest      = 0
   mpiSendTag   = 45
   mpiSendCount = 5
   mpiSource    = 0
   mpiRecvTag   = 48
   mpiRecvCount = 3
   mpiSendDatatype  = mpiReal8TypeDef
   mpiRecvDatatype  = mpiReal8TypeDef

   CALL HTMPI_SENDRECV(real8Array2SR2,  &
                       mpiSendCount,    &
                       mpiSendDatatype, &
                       mpiDest,         &
                       mpiSendTag,      &
                       real8Array1SR2,  &
                       mpiRecvCount,    &
                       mpiRecvDatatype, &
                       mpiSource,       &
                       mpiRecvTag,      &
                       mpiCommWorld,    &
                       mpiStatus,       &
                       mpiIError)

  ELSE IF (mpiCommRank == 0) THEN
! master
   DO n=1,3
    real8Array1SR1(n) = 0.0_real8Kind
   END DO 
   DO n=1,5
    real8Array2SR1(n) = real8Array2SRT(n)
   END DO 

   mpiDest      = 1
   mpiSendTag   = 47
   mpiSendCount = 5
   mpiSource    = 1
   mpiRecvTag   = 44
   mpiRecvCount = 3
   mpiSendDatatype  = mpiReal8TypeDef
   mpiRecvDatatype  = mpiReal8TypeDef

   CALL HTMPI_SENDRECV(real8Array2SR1,  &
                       mpiSendCount,    &
                       mpiSendDatatype, &
                       mpiDest,         &
                       mpiSendTag,      &
                       real8Array1SR1,  &
                       mpiRecvCount,    &
                       mpiRecvDatatype, &
                       mpiSource,       &
                       mpiRecvTag,      &
                       mpiCommWorld,    &
                       mpiStatus,       &
                       mpiIError)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_SENDRECV (real8Array) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiRecvCount
    IF (isEqual .AND. (real8Array1SRT(n) == real8Array1SR1(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SENDRECV (real8Array) call 1.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SENDRECV (real8Array) call 1.'
    WRITE(6,*) ' expected ',real8Array1SRT,'; recved ',real8Array1SR1
    passedTest = .FALSE.
    GO TO 100
   END IF

! now go the other way

   DO n=1,5
    real8Array2SR1(n) = 0.0_8
   END DO 

   mpiDest      = 1
   mpiSendTag   = 48
   mpiSendCount = 3
   mpiSource    = 1
   mpiRecvTag   = 45
   mpiRecvCount = 5
   mpiSendDatatype  = mpiReal8TypeDef
   mpiRecvDatatype  = mpiReal8TypeDef

   CALL HTMPI_SENDRECV(real8Array1SR1,  &
                       mpiSendCount,    &
                       mpiSendDatatype, &
                       mpiDest,         &
                       mpiSendTag,      &
                       real8Array2SR1,  &
                       mpiRecvCount,    &
                       mpiRecvDatatype, &
                       mpiSource,       &
                       mpiRecvTag,      &
                       mpiCommWorld,    &
                       mpiStatus,       &
                       mpiIError)

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiRecvCount
    IF (isEqual .AND. (real8Array2SRT(n) == real8Array2SR1(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
    END IF
   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SENDRECV (real8Array) call 2.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SENDRECV (real8Array) call 2.'
    WRITE(6,*) ' expected ',real8Array2SRT,'; recved ',real8Array2SR1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  Real8Array message passing.'
   WRITE(6,*)
  END IF

!------------------Real8Variable---------------------------------
! initially testing BCAST from master and blocking send/recv back

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  Real8Variable message passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   real8Var1 = 1.23_real8Kind
   real8Var2 = -21.23_real8Kind
  ELSE
   real8Var1 = 23.23_real8Kind
   real8Var2 = 42.23_real8Kind
  END IF

  mpiCount = 1
  mpiDatatype = mpiReal8TypeDef
  mpiRoot = 0

  CALL HTMPI_BCAST(real8Var1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (real8Variable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (real8Variable) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 23
   CALL HTMPI_SEND(real8Var1,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (real8Variable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 23
   CALL HTMPI_RECV(real8Var2,     &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (real8Variable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (real8Var2 == real8Var1) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (real8Variable) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (real8Variable) call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_SEND (real8Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_RECV (real8Variable) call.'
    WRITE(6,*) ' expected A; recved ',real8Var2
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   real8Var1 = 203.23_real8Kind
   real8Var2 = 420.23_real8Kind
  ELSE
   real8Var1 = -23.23_real8Kind
   real8Var2 = -42.23_real8Kind
  END IF

  mpiCount = 1
  mpiDatatype = mpiReal8TypeDef
  mpiRoot = 1

  CALL HTMPI_BCAST(real8Var2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (real8Variable) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 24
   CALL HTMPI_ISEND(real8Var2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (real8Variable) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 24
   CALL HTMPI_IRECV(real8Var1,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (real8Variable) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_TEST call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   IF (real8Var2 == real8Var1) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (real8Variable) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (real8Variable) call.'
    WRITE(6,*) 'PASS:  MPI_WAIT call.'
    WRITE(6,*) 'PASS:  MPI_TEST call.'
   ELSE
    WRITE(6,*) 'FAIL:  MPI_ISEND (real8Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_IRECV (real8Variable) call.'
    WRITE(6,*) 'FAIL:  MPI_WAIT call.'
    WRITE(6,*) 'FAIL:  MPI_TEST call.'
    WRITE(6,*) ' expected ',real8Var2,'; recved ',real8Var1
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  Real8Variable message passing.'
   WRITE(6,*)
  END IF

! trying out an MPI_STRUCT, using the previous tests again -- but
!   rolled together.

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'TESTING:  MPI_STRUCT multiple datatype passing.'
   WRITE(6,*)
  END IF

  IF (masterNode) THEN
   int4Array1 = (/1_int4Kind, 2_int4Kind, 3_int4Kind, 4_int4Kind, 5_int4Kind/)
   int4Array2 = (/2_int4Kind, 3_int4Kind, 4_int4Kind, 5_int4Kind, 6_int4Kind/)
  ELSE
   int4Array1 = (/3_int4Kind, 4_int4Kind, 5_int4Kind, 6_int4Kind, 7_int4Kind/)
   int4Array2 = (/4_int4Kind, 5_int4Kind, 6_int4Kind, 7_int4Kind, 8_int4Kind/)
  END IF

  IF (masterNode) THEN
   int8Array1 = (/1_int8Kind, 2_int8Kind, 3_int8Kind, 4_int8Kind, 5_int8Kind/)
   int8Array2 = (/2_int8Kind, 3_int8Kind, 4_int8Kind, 5_int8Kind, 6_int8Kind/)
  ELSE
   int8Array1 = (/3_int8Kind, 4_int8Kind, 5_int8Kind, 6_int8Kind, 7_int8Kind/)
   int8Array2 = (/4_int8Kind, 5_int8Kind, 6_int8Kind, 7_int8Kind, 8_int8Kind/)
  END IF

  IF (masterNode) THEN
   charArray1 = (/'A','B','C','D','E'/)
   charArray2 = (/'B','C','D','E','F'/)
  ELSE
   charArray1 = (/'C','D','E','F','G'/)
   charArray2 = (/'D','E','F','G','H'/)
  END IF

  IF (masterNode) THEN
   logicalArray1 = (/.TRUE.,.FALSE.,.TRUE.,.FALSE./)
   logicalArray2 = (/.FALSE.,.TRUE.,.FALSE.,.TRUE./)
  ELSE
   logicalArray1 = (/.TRUE.,.TRUE.,.TRUE.,.FALSE./)
   logicalArray2 = (/.FALSE.,.TRUE.,.TRUE.,.TRUE./)
  END IF

  IF (masterNode) THEN
   real4Array1 = (/1.1_real4Kind, 2.1_real4Kind, 3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind/)
   real4Array2 = (/2.1_real4Kind, 3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind/)
  ELSE
   real4Array1 = (/3.1_real4Kind, 4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind, 7.1_real4Kind/)
   real4Array2 = (/4.1_real4Kind, 5.1_real4Kind, 6.1_real4Kind, 7.1_real4Kind, 8.1_real4Kind/)
  END IF

  IF (masterNode) THEN
   real8Array1 = (/1.23_real8Kind, 2.23_real8Kind, 3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind/)
   real8Array2 = (/2.23_real8Kind, 3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind/)
  ELSE
   real8Array1 = (/3.23_real8Kind, 4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind, 7.23_real8Kind/)
   real8Array2 = (/4.23_real8Kind, 5.23_real8Kind, 6.23_real8Kind, 7.23_real8Kind, 8.23_real8Kind/)
  END IF

! construct and commit the first data structure

  CALL HTMPI_GET_ADDRESS(int4Array1(1),        &
                         dataAddressDeltas1(1),&
                         mpiIError)

  dataLength1(1) = 2
  dataType1(1) = mpiInt4TypeDef

  CALL HTMPI_GET_ADDRESS(int4Array1(3),        &
                         dataAddressDeltas1(2),&
                         mpiIError)

  dataLength1(2) = 3
  dataType1(2) = mpiInt4TypeDef

  CALL HTMPI_GET_ADDRESS(charArray1,           &
                         dataAddressDeltas1(3),&
                         mpiIError)

  dataLength1(3) = 5
  dataType1(3) = mpiCharacterTypeDef

  CALL HTMPI_GET_ADDRESS(logicalArray1(1),     &
                         dataAddressDeltas1(4),&
                         mpiIError)

  dataLength1(4) = 1
  dataType1(4) = mpiLogicalTypeDef

  CALL HTMPI_GET_ADDRESS(logicalArray1(2),     &
                         dataAddressDeltas1(5),&
                         mpiIError)

  dataLength1(5) = 3
  dataType1(5) = mpiLogicalTypeDef

  CALL HTMPI_GET_ADDRESS(real4Array1,          &
                         dataAddressDeltas1(6),&
                         mpiIError)

  dataLength1(6) = 5
  dataType1(6) = mpiReal4TypeDef

  CALL HTMPI_GET_ADDRESS(real8Array1,          &
                         dataAddressDeltas1(7),&
                         mpiIError)

  dataLength1(7) = 4
  dataType1(7) = mpiReal8TypeDef

  CALL HTMPI_GET_ADDRESS(real8Array1(5),       &
                         dataAddressDeltas1(8),&
                         mpiIError)

  dataLength1(8) = 1
  dataType1(8) = mpiReal8TypeDef

! removing int8 data

  CALL HTMPI_GET_ADDRESS(int8Array1(1),        &
                         dataAddressDeltas1(9),&
                         mpiIError)

  dataLength1(9) = 2
  dataType1(9) = mpiInt8TypeDef

  initialDataAddress1 = dataAddressDeltas1(1)

  DO n=1,numParts1
   dataAddressDeltas1(n) = dataAddressDeltas1(n)-initialDataAddress1
  END DO

  CALL HTMPI_TYPE_CREATE_STRUCT(numParts1,          &
                                dataLength1,        &
                                dataAddressDeltas1, &
                                dataType1,          &
                                dataStructureType1, &
                                mpiIError)             

  CALL HTMPI_TYPE_COMMIT(dataStructureType1, &
                         mpiIError)

! construct and commit the second data structure

  CALL HTMPI_GET_ADDRESS(int4Array2,           &
                         dataAddressDeltas2(1),&
                         mpiIError)

  dataLength2(1) = 5
  dataType2(1) = mpiInt4TypeDef

  CALL HTMPI_GET_ADDRESS(charArray2,           &
                         dataAddressDeltas2(2),&
                         mpiIError)

  dataLength2(2) = 5
  dataType2(2) = mpiCharacterTypeDef

  CALL HTMPI_GET_ADDRESS(logicalArray2,        &
                         dataAddressDeltas2(3),&
                         mpiIError)

  dataLength2(3) = 4
  dataType2(3) = mpiLogicalTypeDef

  CALL HTMPI_GET_ADDRESS(real4Array2,          &
                         dataAddressDeltas2(4),&
                         mpiIError)

  dataLength2(4) = 5
  dataType2(4) = mpiReal4TypeDef

  CALL HTMPI_GET_ADDRESS(real8Array2,          &
                         dataAddressDeltas2(5),&
                         mpiIError)

  dataLength2(5) = 5
  dataType2(5) = mpiReal8TypeDef

  CALL HTMPI_GET_ADDRESS(int8Array2,           &
                         dataAddressDeltas2(6),&
                         mpiIError)

  dataLength2(6) = 2
  dataType2(6) = mpiInt8TypeDef

  initialDataAddress2 = dataAddressDeltas2(1)

  DO n=1,numParts2
   dataAddressDeltas2(n) = dataAddressDeltas2(n)-initialDataAddress2
  END DO

  CALL HTMPI_TYPE_CREATE_STRUCT(numParts2,          &
                                dataLength2,        &
                                dataAddressDeltas2, &
                                dataType2,          &
                                dataStructureType2, &
                                mpiIError)             

  CALL HTMPI_TYPE_COMMIT(dataStructureType2, &
                         mpiIError)

  mpiCount = 1
  mpiDatatype = dataStructureType1
  mpiRoot = 0

  CALL HTMPI_BCAST(int4Array1,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (MPI_STRUCT) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BCAST (MPI_STRUCT) call.'
  END IF

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 9
   mpiDatatype = dataStructureType1
   CALL HTMPI_SEND(int4Array1,   &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiDest,      &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_SEND (MPI_STRUCT) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 9
   mpiDatatype = dataStructureType2
   CALL HTMPI_RECV(int4Array2,   &
                   mpiCount,     &
                   mpiDatatype,  &
                   mpiSource,    &
                   mpiTag,       &
                   mpiCommWorld, &
                   mpiStatus,    &
                   mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_RECV (MPI_STRUCT) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (int4Array1(n) == int4Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_SEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_RECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',int4Array1,'; recved ',int4Array2
    END IF
    IF (isEqual .AND. (charArray1(n) == charArray2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_SEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_RECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',charArray1,'; recved ',charArray2
    END IF
    IF (isEqual .AND. (logicalArray1(n) .EQV. logicalArray2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_SEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_RECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',logicalArray1,'; recved ',logicalArray2
    END IF
    IF (isEqual .AND. (real4Array1(n) == real4Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_SEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_RECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',real4Array1,'; recved ',real4Array2
    END IF
    IF (isEqual .AND. (real8Array1(n) == real8Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_SEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_RECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',real8Array1,'; recved ',real8Array2
    END IF

    IF (isEqual .AND. (int8Array1(n) == int8Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_SEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_RECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',int8Array1,'; recved ',int8Array2
    END IF

   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_SEND (MPI_STRUCT) call.'
    WRITE(6,*) 'PASS:  MPI_RECV (MPI_STRUCT) call.'
   ELSE
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

! good to here

! next testing BCAST from node1 and nonblocking send/recv back

  IF (masterNode) THEN
   int4Array1 = (/1_4, 2_4, 3_4, 4_4, 5_4/)
   int4Array2 = (/2_4, 3_4, 4_4, 5_4, 6_4/)
  ELSE
   int4Array1 = (/3_4, 4_4, 5_4, 6_4, 7_4/)
   int4Array2 = (/4_4, 5_4, 6_4, 7_4, 8_4/)
  END IF

  IF (masterNode) THEN
   charArray1 = (/'A','B','C','D','E'/)
   charArray2 = (/'B','C','D','E','F'/)
  ELSE
   charArray1 = (/'C','D','E','F','G'/)
   charArray2 = (/'D','E','F','G','H'/)
  END IF

  IF (masterNode) THEN
   logicalArray1 = (/.TRUE.,.FALSE.,.TRUE.,.FALSE./)
   logicalArray2 = (/.FALSE.,.TRUE.,.FALSE.,.TRUE./)
  ELSE
   logicalArray1 = (/.TRUE.,.TRUE.,.TRUE.,.FALSE./)
   logicalArray2 = (/.FALSE.,.TRUE.,.TRUE.,.TRUE./)
  END IF

  IF (masterNode) THEN
   real4Array1 = (/1.1_4, 2.1_4, 3.1_4, 4.1_4, 5.1_4/)
   real4Array2 = (/2.1_4, 3.1_4, 4.1_4, 5.1_4, 6.1_4/)
  ELSE
   real4Array1 = (/3.1_4, 4.1_4, 5.1_4, 6.1_4, 7.1_4/)
   real4Array2 = (/4.1_4, 5.1_4, 6.1_4, 7.1_4, 8.1_4/)
  END IF

  IF (masterNode) THEN
   real8Array1 = (/1.23_8, 2.23_8, 3.23_8, 4.23_8, 5.23_8/)
   real8Array2 = (/2.23_8, 3.23_8, 4.23_8, 5.23_8, 6.23_8/)
  ELSE
   real8Array1 = (/3.23_8, 4.23_8, 5.23_8, 6.23_8, 7.23_8/)
   real8Array2 = (/4.23_8, 5.23_8, 6.23_8, 7.23_8, 8.23_8/)
  END IF

  IF (masterNode) THEN
   int8Array1 = (/1_8, 2_8, 3_8, 4_8, 5_8/)
   int8Array2 = (/2_8, 3_8, 4_8, 5_8, 6_8/)
  ELSE
   int8Array1 = (/3_8, 4_8, 5_8, 6_8, 7_8/)
   int8Array2 = (/4_8, 5_8, 6_8, 7_8, 8_8/)
  END IF

  mpiCount = 1
  mpiDatatype = dataStructureType2
  mpiRoot = 1

  CALL HTMPI_BCAST(int4Array2,mpiCount,mpiDatatype,mpiRoot,mpiCommWorld,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   WRITE(6,*) 'FAIL:  error in MPI_BCAST (MPI_STRUCT) call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

! GO TO 121

  IF (mpiCommRank == 1) THEN
   mpiDest = 0
   mpiTag = 10
   mpiDatatype = dataStructureType2
   CALL HTMPI_ISEND(int4Array2,     &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiDest,      &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_ISEND (MPI_STRUCT) call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
    CALL HTMPI_WAIT(mpiRequest,  &
                    mpiStatus,   &
                    mpiIerror)
    IF (mpiIerror /= mpiSuccess) THEN
     WRITE(6,*) 'FAIL:  error in MPI_WAIT call: ',mpiIerror
     passedTest = .FALSE.
     GO TO 100
    END IF
  ELSE IF (mpiCommRank == 0) THEN
   mpiSource = 1
   mpiTag = 10
   mpiDatatype = dataStructureType1
   CALL HTMPI_IRECV(int4Array1,   &
                    mpiCount,     &
                    mpiDatatype,  &
                    mpiSource,    &
                    mpiTag,       &
                    mpiCommWorld, &
                    mpiRequest,   &
                    mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    WRITE(6,*) 'FAIL:  error in MPI_IRECV (MPI_STRUCT) call: ',mpiIerror
    passedTest = .FALSE.
    GO TO 100
   END IF

   CALL HTMPI_WAIT(mpiRequest,  &
                   mpiStatus,   &
                   mpiIerror)

! check for successful completion

   isEqual = .TRUE.
   DO n=1,mpiCount
    IF (isEqual .AND. (int4Array1(n) == int4Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_ISEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_IRECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',int4Array2,'; recved ',int4Array1
    END IF
    IF (isEqual .AND. (charArray1(n) == charArray2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_ISEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_IRECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',charArray2,'; recved ',charArray1
    END IF
    IF (isEqual .AND. (logicalArray1(n) .EQV. logicalArray2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_ISEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_IRECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',logicalArray2,'; recved ',logicalArray1
    END IF
    IF (isEqual .AND. (real4Array1(n) == real4Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_ISEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_IRECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',real4Array2,'; recved ',real4Array1
    END IF
    IF (isEqual .AND. (real8Array1(n) == real8Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_ISEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_IRECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',real8Array2,'; recved ',real8Array1
    END IF

    IF (isEqual .AND. (int8Array1(n) == int8Array2(n))) THEN
     isEqual = .TRUE.
    ELSE
     isEqual = .FALSE.
     WRITE(6,*) 'FAIL:  MPI_ISEND (MPI_STRUCT) call.'
     WRITE(6,*) 'FAIL:  MPI_IRECV (MPI_STRUCT) call.'
     WRITE(6,*) ' expected ',int8Array2,'; recved ',int8Array1
    END IF

   END DO

   IF (isEqual) THEN
    WRITE(6,*) 'PASS:  MPI_ISEND (MPI_STRUCT) call.'
    WRITE(6,*) 'PASS:  MPI_IRECV (MPI_STRUCT) call.'
   ELSE
    passedTest = .FALSE.
    GO TO 100
   END IF

  END IF

!121 CONTINUE

  IF (passedTest .AND. masterNode) THEN
   WRITE(6,*)
   WRITE(6,*) 'PASS:  MPI_STRUCT message passing.'
   WRITE(6,*)
  END IF

! release the struct datatypes

  CALL HTMPI_TYPE_FREE(dataStructureType1, mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   IF (masterNode) WRITE(6,*) 'FAIL:  error in MPI_TYPE_FREE call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   CONTINUE
  END IF

  CALL HTMPI_TYPE_FREE(dataStructureType2, mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   IF (masterNode) WRITE(6,*) 'FAIL:  error in MPI_TYPE_FREE call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_TYPE_FREE call.' 
  END IF

  GO TO 100

! testing MPI_FINALIZED call (1)

  CALL HTMPI_FINALIZED(mpiFlag,mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   IF (masterNode) &
    WRITE(6,*) 'FAIL: error in MPI_FINALIZED call (1) IERROR = ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  END IF

! expect a .FALSE. result, since MPI_INIT has not been called

  IF (mpiFlag) THEN
   IF (masterNode) &
    WRITE(6,*) 'FAIL: MPI_FINALIZED call (1) returned incorrect result.'
   passedTest = .FALSE.
   GO TO 100
  END IF

 100 CONTINUE

  CALL HTMPI_BARRIER(mpiCommWorld, mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   IF (masterNode) WRITE(6,*) 'FAIL:  error in MPI_BARRIER call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_BARRIER call.' 
  END IF

  finalTime = HTMPI_WTIME()
  IF (masterNode) WRITE(6,102) finalTime - initialTime

  CALL HTMPI_FINALIZE(mpiIerror)

  IF (mpiIerror /= mpiSuccess) THEN
   IF (masterNode) WRITE(6,*) 'FAIL:  error in MPI_FINALIZE call: ',mpiIerror
   passedTest = .FALSE.
   GO TO 100
  ELSE
   IF (masterNode) WRITE(6,*) 'PASS:  MPI_FINALIZE call.' 
  END IF

  IF (passedTest) THEN

! testing MPI_FINALIZED call (2)

   CALL HTMPI_FINALIZED(mpiFlag,mpiIerror)

   IF (mpiIerror /= mpiSuccess) THEN
    IF (masterNode) WRITE(6,*) 'FAIL:  MPI_FINALIZED call (2) IERROR = ',mpiIerror
    passedTest = .FALSE.
   END IF

! expect a .TRUE. result, since MPI_INIT has been called

   IF (mpiFlag) THEN
    IF (masterNode) WRITE(6,*) 'PASS:  MPI_FINALIZED tests 1 and 2.'
   ELSE
    IF (masterNode) WRITE(6,*) 'FAIL:  MPI_FINALIZED call (2) returned incorrect result.'
    passedTest = .FALSE.
   END IF

!  finalTime = HTMPI_WTIME()
!  IF (masterNode) WRITE(6,102) finalTime - initialTime

  END IF

  IF (passedTest) THEN
   IF (masterNode) THEN
    WRITE(6,999)
    OPEN(33,FILE = 'MessagePassingInterfaceTest.PASSED',FORM = 'FORMATTED')
    WRITE(33,999)
    CLOSE(33)
   END IF
  ELSE
   IF (masterNode) THEN
    WRITE(6,998)
    OPEN(33,FILE = 'MessagePassingInterfaceTest.FAILED',FORM = 'FORMATTED')
    WRITE(33,998)
    CLOSE(33)
   END IF
  END IF

 101 FORMAT(1x,'MPI timing precision = ',e13.5,' seconds.')
 102 FORMAT(1x,'Testing program took ',f10.5,' seconds.')
 103 FORMAT(1x,/,&
 1x,' ----------------------------------------- ',/,&
 1x,'| MessagePassingInterface testing routine |',/,&
 1x,'|  copyright 2007 by Hixon Technology     |',/,&
 1x,'|  All rights reserved.                   |',/,&
 1x,'|  No warranty for this code express      |',/,&
 1x,'|    or implied.                          |',/,&
 1x,'|  contact:  Ray Hixon                    |',/,&
 1x,'|   email: rhixon@wideopenwest.com        |',/,&
 1x,' ----------------------------------------- ',/,&
 1x)
 104 FORMAT(1x,'Number of MPI nodes = ',i5)
 105 FORMAT(1x,'MPI Version = ',i5,'.',i5)
 998 FORMAT(1x,'FAIL:  Testing routine FAILED test.',/)
 999 FORMAT(1x,'PASS:  Testing routine PASSED all tests.',/)

  STOP
END PROGRAM MessagePassingInterfaceTest

