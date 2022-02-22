  i  = iMinSweep(1,2)
  iFacA = 0.5_rDef*REAL(dISweep(2),rDef)

  fac1 = (1.0_rDef + deltaSigma(i,j,k,l)*(epsilonXi(i,j,k,l)   &
                                         +epsilonEta(i,j,k,l)  &
                                         +epsilonZeta(i,j,k,l) &
                                         +epsilonTau(i,j,k,l)))

  fac2 = deltaSigma(i,j,k,l)/fac1
                                                            
  DO nV=1,numberOfVariables
   deltaQ(nV,i,j,k,l) = dQ(nV,i,j,k,l)              &
                      + fac2*(dQLastEta(nV,i)       &
                            + dQLastZeta(nV,i,j)    &
                            + dQLastTau(nV,i,j,k))
  END DO

  DO i = iMinSweep(1,2)+dISweep(2),iMaxSweep(1,2),dISweep(2)
   ii = i - dISweep(2)

! if dISweep(2) = -1, then the A- matrix is subtracted at i+1
! if dISweep(2) = +1, then the A+ matrix is added at i-1

   DO n2=1,numberOfVariables
    DO n1=1,numberOfVariables
     aMatrixLast(n1,n2) = iFacA*aMatrixXi(n1,n2,ii,j,k,l) ! +/-(1/2)*[A(i-/+1)]
    END DO
    aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + 0.5_rDef*epsilonXi(ii,j,k,l) ! A+/- +/- (1/2)*epsilon(i-/+1)
!   aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + iFacA*epsilonXi(ii,j,k,l) ! A+/- +/- (1/2)*epsilon(i-/+1)
   END DO

   dQLastXi(:) = MATMUL(aMatrixLast,deltaQ(:,ii,j,k,l))

   fac1 = (1.0_rDef + deltaSigma(i,j,k,l)*(epsilonXi(i,j,k,l)   &
                                          +epsilonEta(i,j,k,l)  &
                                          +epsilonZeta(i,j,k,l) &
                                          +epsilonTau(i,j,k,l)))

   fac2 = deltaSigma(i,j,k,l)/fac1
                                                            
   DO nV=1,numberOfVariables
    deltaQ(nV,i,j,k,l) = dQ(nV,i,j,k,l)              &
                       + fac2*(dQLastXi(nV)          &
                             + dQLastEta(nV,i)       &
                             + dQLastZeta(nV,i,j)    &
                             + dQLastTau(nV,i,j,k))
   END DO
  END DO

