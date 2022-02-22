  i  = iMinSweep(1,1)
  iFacA = 0.5_rDef*REAL(dISweep(1),rDef)

  fac1 = deltaSigma(i,j,k)/(1.0_rDef + deltaSigma(i,j,k)*(epsilonXi(i,j,k)   &
                                                         +epsilonEta(i,j,k)  &
                                                         +epsilonZeta(i,j,k)))
                                                            
  DO nV=1,numberOfVariables
   dQ(nV,i,j,k) = fac1*(dQLastEta(nV,i)     &
                      + dQLastZeta(nV,i,j)  &
                      - rhsVector(nV,i,j,k))
  END DO

  DO i = iMinSweep(1,1)+dISweep(1),iMaxSweep(1,1),dISweep(1)
   ii = i - dISweep(1)

! if dISweep(1) = -1, then the A- matrix is subtracted at i+1
! if dISweep(1) = +1, then the A+ matrix is added at i-1

   DO n2=1,numberOfVariables
    DO n1=1,numberOfVariables
     aMatrixLast(n1,n2) = iFacA*aMatrixXi(n1,n2,ii,j,k) ! +/-(1/2)*[A(i-/+1)]
    END DO
    aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + 0.5_rDef*epsilonXi(ii,j,k) ! A+/- +/- (1/2)*epsilon(i-/+1)
!   aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + iFacA*epsilonXi(ii,j,k) ! A+/- +/- (1/2)*epsilon(i-/+1)
   END DO

   dQLastXi(:) = MATMUL(aMatrixLast,dQ(:,ii,j,k))
   
   fac1 = deltaSigma(i,j,k)/(1.0_rDef + deltaSigma(i,j,k)*(epsilonXi(i,j,k)   &
                                                          +epsilonEta(i,j,k)  &
                                                          +epsilonZeta(i,j,k)))

   DO nV=1,numberOfVariables
    dQ(nV,i,j,k) = fac1*(dQLastXi(nV)        &
                       + dQLastEta(nV,i)     &
                       + dQLastZeta(nV,i,j)  &
                       - rhsVector(nV,i,j,k))
   END DO
  END DO

