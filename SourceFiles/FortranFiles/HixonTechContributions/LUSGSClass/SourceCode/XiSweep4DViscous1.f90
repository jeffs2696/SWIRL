  i  = iMinSweep(1,1)
  iFacA = 0.5_rDef*REAL(dISweep(1),rDef)

! if dISweep(1) = -1, then the A- matrix is subtracted at i+1
! if dISweep(1) = +1, then the A+ matrix is added at i-1

  DO n2=1,numberOfVariables
   DO n1=1,numberOfVariables
    abMatrixCurrent(n1,n2) = 2.0_rDef*deltaSigma(i,j,k,l)*(bMatrixXi(n1,n2,i,j,k,l)   &
                                                          +bMatrixEta(n1,n2,i,j,k,l)  &
                                                          +bMatrixZeta(n1,n2,i,j,k,l) &
                                                          +bMatrixTau(n1,n2,i,j,k,l))
   END DO
   abMatrixCurrent(n2,n2) = abMatrixCurrent(n2,n2)                    &
                          + 1.0_rDef                                  &
                          + deltaSigma(i,j,k,l)*(epsilonXi(i,j,k,l)   & 
                                                +epsilonEta(i,j,k,l)  &
                                                +epsilonZeta(i,j,k,l) &
                                                +epsilonTau(i,j,k,l))
  END DO

  DO nV=1,numberOfVariables
   dQCurrent(nV) = deltaSigma(i,j,k,l)*(dQLastEta(nV,i)      &
                                       +dQLastZeta(nV,i,j)   &
                                       +dQLastTau(nV,i,j,k)  &
                                       -rhsVector(nV,i,j,k,l))
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
   dQ(nV,i,j,k,l) = dQCurrent(nV)
  END DO

! and do the rest of the xi sweep

  DO i = iMinSweep(1,1)+dISweep(1),iMaxSweep(1,1),dISweep(1)
   ii = i - dISweep(1)

! if dISweep(1) = -1, then the A- matrix is subtracted at i+1
! if dISweep(1) = +1, then the A+ matrix is added at i-1

   DO n2=1,numberOfVariables
    DO n1=1,numberOfVariables
     abMatrixLast(n1,n2) = iFacA*aMatrixXi(n1,n2,ii,j,k,l) & ! +/-(1/2)*[A(i-/+1)]
                               + bMatrixXi(n1,n2,ii,j,k,l)

     abMatrixCurrent(n1,n2) = 2.0_rDef*deltaSigma(i,j,k,l)*(bMatrixXi(n1,n2,i,j,k,l)   &
                                                           +bMatrixEta(n1,n2,i,j,k,l)  &
                                                           +bMatrixZeta(n1,n2,i,j,k,l) &
                                                           +bMatrixTau(n1,n2,i,j,k,l))
    END DO
    abMatrixLast(n2,n2) = abMatrixLast(n2,n2)  &
                        + 0.5_rDef*epsilonXi(ii,j,k,l) ! A+/- +/- (1/2)*epsilon(i-/+1)
!                       + iFacA*epsilonXi(ii,j,k,l) ! A+/- +/- (1/2)*epsilon(i-/+1)

    abMatrixCurrent(n2,n2) = abMatrixCurrent(n2,n2)                    &
                           + 1.0_rDef                                  &
                           + deltaSigma(i,j,k,l)*(epsilonXi(i,j,k,l)   & 
                                                 +epsilonEta(i,j,k,l)  &
                                                 +epsilonZeta(i,j,k,l) &
                                                 +epsilonTau(i,j,k,l))
   END DO

   dQLastXi(:) = MATMUL(abMatrixLast,dQ(:,ii,j,k,l)) 

   DO nV=1,numberOfVariables
    dQCurrent(nV) = deltaSigma(i,j,k,l)*(dQLastXi(nV)         &
                                        +dQLastEta(nV,i)      &
                                        +dQLastZeta(nV,i,j)   &
                                        +dQLastTau(nV,i,j,k)  &
                                        -rhsVector(nV,i,j,k,l))
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
    dQ(nV,i,j,k,l) = dQCurrent(nV)
   END DO
  END DO

