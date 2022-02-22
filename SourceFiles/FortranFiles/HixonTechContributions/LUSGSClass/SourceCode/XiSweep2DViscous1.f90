  i  = iMinSweep(1,1)
  iFacA = 0.5_rDef*REAL(dISweep(1),rDef)

! if dISweep(1) = -1, then the A- matrix is subtracted at i+1
! if dISweep(1) = +1, then the A+ matrix is added at i-1

  DO n2=1,numberOfVariables
   DO n1=1,numberOfVariables
    abMatrixCurrent(n1,n2) = 2.0_rDef*deltaSigma(i,j)*(bMatrixXi(n1,n2,i,j)   &
                                                      +bMatrixEta(n1,n2,i,j))
   END DO
   abMatrixCurrent(n2,n2) = abMatrixCurrent(n2,n2)                    &
                          + 1.0_rDef                                  &
                          + deltaSigma(i,j)*(epsilonXi(i,j)   & 
                                            +epsilonEta(i,j))
  END DO

  DO nV=1,numberOfVariables
   dQCurrent(nV) = deltaSigma(i,j)*(dQLastEta(nV,i)      &
                                   -rhsVector(nV,i,j))
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
    'ERROR returned from DGESV (1) call!  INFO = ',INFO
   GO TO 100
  ELSE
   CONTINUE
  END IF
   
  DO nV=1,numberOfVariables
   dQ(nV,i,j) = dQCurrent(nV)
  END DO

! and do the rest of the xi sweep

  DO i = iMinSweep(1,1)+dISweep(1),iMaxSweep(1,1),dISweep(1)
   ii = i - dISweep(1)

! if dISweep(1) = -1, then the A- matrix is subtracted at i+1
! if dISweep(1) = +1, then the A+ matrix is added at i-1

   DO n2=1,numberOfVariables
    DO n1=1,numberOfVariables
     abMatrixLast(n1,n2) = iFacA*aMatrixXi(n1,n2,ii,j) & ! +/-(1/2)*[A(i-/+1)]
                               + bMatrixXi(n1,n2,ii,j)

     abMatrixCurrent(n1,n2) = 2.0_rDef*deltaSigma(i,j)*(bMatrixXi(n1,n2,i,j)   &
                                                       +bMatrixEta(n1,n2,i,j))
    END DO
    abMatrixLast(n2,n2) = abMatrixLast(n2,n2)  &
                        + 0.5_rDef*epsilonXi(ii,j) ! A+/- +/- (1/2)*epsilon(i-/+1)
!                       + iFacA*epsilonXi(ii,j) ! A+/- +/- (1/2)*epsilon(i-/+1)

    abMatrixCurrent(n2,n2) = abMatrixCurrent(n2,n2)                    &
                           + 1.0_rDef                                  &
                           + deltaSigma(i,j)*(epsilonXi(i,j)   & 
                                             +epsilonEta(i,j))
   END DO

   dQLastXi(:) = MATMUL(abMatrixLast,dQ(:,ii,j)) 

   DO nV=1,numberOfVariables
    dQCurrent(nV) = deltaSigma(i,j)*(dQLastXi(nV)         &
                                    +dQLastEta(nV,i)      &
                                    -rhsVector(nV,i,j))
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
     'ERROR returned from DGESV (2) call!  INFO = ',INFO
    GO TO 100
   ELSE
    CONTINUE
   END IF
   
   DO nV=1,numberOfVariables
    dQ(nV,i,j) = dQCurrent(nV)
   END DO
  END DO

