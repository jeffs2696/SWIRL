  i  = iMinSweep(1,2)
  iFacA = 0.5_rDef*REAL(dISweep(2),rDef)

! if dISweep(1) = -1, then the A- matrix is subtracted at i+1
! if dISweep(1) = +1, then the A+ matrix is added at i-1

  DO n2=1,numberOfVariables
   DO n1=1,numberOfVariables
    abMatrixCurrent(n1,n2) = 2.0_rDef*deltaSigma(i,j,k)*(bMatrixXi(n1,n2,i,j,k)   &
                                                        +bMatrixEta(n1,n2,i,j,k)  &
                                                        +bMatrixZeta(n1,n2,i,j,k))
   END DO
   abMatrixCurrent(n2,n2) = abMatrixCurrent(n2,n2)                &
                          + 1.0_rDef                              &
                          + deltaSigma(i,j,k)*(epsilonXi(i,j,k)   &
                                              +epsilonEta(i,j,k)  &
                                              +epsilonZeta(i,j,k))
  END DO

  DO nV=1,numberOfVariables
   dQCurrent(nV) = deltaSigma(i,j,k)*(dQLastEta(nV,i)      &
                                     +dQLastZeta(nV,i,j))
  END DO

! LAPACK call to solve the equation A X = B and return the
!  solution in dQCurrent

  CALL DGESV(numberOfVariables, & ! N
             1,                 & ! NRHS
             abMatrixCurrent,   & ! A
             numberOfVariables, & ! LDA
             IPIV,              & ! IPIV
             dQCurrent,         & ! B
             numberOfVariables, & ! LDB
             INFO)                ! INFO

  IF (INFO /= 0) THEN
   WRITE(charStringObject%charString,'(a,i5)') &
    'ERROR returned from DGESV (3) call!  INFO = ',INFO
   GO TO 100
  ELSE
   CONTINUE
  END IF

  DO nV=1,numberOfVariables
   deltaQ(nV,i,j,k) = dQ(nV,i,j,k) + dQCurrent(nV)
  END DO

! and do the rest of the xi sweep

  DO i = iMinSweep(1,2)+dISweep(2),iMaxSweep(1,2),dISweep(2)
   ii = i - dISweep(2)

! if dISweep(1) = -1, then the A- matrix is subtracted at i+1
! if dISweep(1) = +1, then the A+ matrix is added at i-1

   DO n2=1,numberOfVariables
    DO n1=1,numberOfVariables
     abMatrixLast(n1,n2) = iFacA*aMatrixXi(n1,n2,ii,j,k) & ! +/-(1/2)*[A(i-/+1)]
                               + bMatrixXi(n1,n2,ii,j,k)

     abMatrixCurrent(n1,n2) = 2.0_rDef*deltaSigma(i,j,k)*(bMatrixXi(n1,n2,i,j,k)   &
                                                         +bMatrixEta(n1,n2,i,j,k)  &
                                                         +bMatrixZeta(n1,n2,i,j,k))
    END DO
    abMatrixLast(n2,n2) = abMatrixLast(n2,n2)  &
                        + 0.5_rDef*epsilonXi(ii,j,k) ! A+/- +/- (1/2)*epsilon(i-/+1)
!                       + iFacA*epsilonXi(ii,j,k) ! A+/- +/- (1/2)*epsilon(i-/+1)

    abMatrixCurrent(n2,n2) = abMatrixCurrent(n2,n2)                &
                           + 1.0_rDef                              &
                           + deltaSigma(i,j,k)*(epsilonXi(i,j,k)   &
                                               +epsilonEta(i,j,k)  &
                                               +epsilonZeta(i,j,k))
   END DO

   dQLastXi(:) = MATMUL(abMatrixLast,deltaQ(:,ii,j,k))

   DO nV=1,numberOfVariables
    dQCurrent(nV) = deltaSigma(i,j,k)*(dQLastXi(nV)         &
                                      +dQLastEta(nV,i)      &
                                      +dQLastZeta(nV,i,j))
   END DO

! LAPACK call to solve the equation A X = B and return the
!  solution in dQCurrent

   CALL DGESV(numberOfVariables, & ! N
              1,                 & ! NRHS
              abMatrixCurrent,   & ! A
              numberOfVariables, & ! LDA
              IPIV,              & ! IPIV
              dQCurrent,         & ! B
              numberOfVariables, & ! LDB
              INFO)                ! INFO

   IF (INFO /= 0) THEN
    WRITE(charStringObject%charString,'(a,i5)') &
     'ERROR returned from DGESV (4) call!  INFO = ',INFO
    GO TO 100
   ELSE
    CONTINUE
   END IF

   DO nV=1,numberOfVariables
    deltaQ(nV,i,j,k) = dQ(nV,i,j,k) + dQCurrent(nV)
   END DO

  END DO

