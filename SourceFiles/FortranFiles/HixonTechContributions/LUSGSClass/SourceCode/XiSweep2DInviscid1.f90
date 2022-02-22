  i  = iMinSweep(1,1)
  iFacA = 0.5_rDef*REAL(dISweep(1),rDef)

  fac1 = deltaSigma(i,j)/(1.0_rDef + deltaSigma(i,j)*(epsilonXi(i,j)   &
                                                     +epsilonEta(i,j)))
                                                            
  DO nV=1,numberOfVariables
   dQ(nV,i,j) = fac1*(dQLastEta(nV,i)     &
                      - rhsVector(nV,i,j))
  END DO

  DO i = iMinSweep(1,1)+dISweep(1),iMaxSweep(1,1),dISweep(1)
   ii = i - dISweep(1)

! if dISweep(1) = -1, then the A- matrix is subtracted at i+1
! if dISweep(1) = +1, then the A+ matrix is added at i-1

   DO n2=1,numberOfVariables
    DO n1=1,numberOfVariables
     aMatrixLast(n1,n2) = iFacA*aMatrixXi(n1,n2,ii,j) ! +/-(1/2)*[A(i-/+1)]
    END DO
    aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + 0.5_rDef*epsilonXi(ii,j) ! A+/- +/- (1/2)*epsilon(i-/+1)
!   aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + iFacA*epsilonXi(ii,j) ! A+/- +/- (1/2)*epsilon(i-/+1)
   END DO

   dQLastXi(:) = MATMUL(aMatrixLast,dQ(:,ii,j))
   
   fac1 = deltaSigma(i,j)/(1.0_rDef + deltaSigma(i,j)*(epsilonXi(i,j)   &
                                                      +epsilonEta(i,j)))

   DO nV=1,numberOfVariables
    dQ(nV,i,j) = fac1*(dQLastXi(nV)        &
                     + dQLastEta(nV,i)     &
                     - rhsVector(nV,i,j))
   END DO
  END DO

