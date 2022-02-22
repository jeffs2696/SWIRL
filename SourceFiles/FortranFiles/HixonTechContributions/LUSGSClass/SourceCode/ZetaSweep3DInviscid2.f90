! no k contribution to dQLast at iMinSweep(2)

  k  = iMinSweep(3,2)
  kFacA = 0.5_rDef*REAL(dISweep(2),rDef)

! do the xi sweep at k = kMinSweep

  DO j=iMinUpdate(2),iMaxUpdate(2)
   DO i=iMinUpdate(1),iMaxUpdate(1)
    DO n1=1,numberOfVariables
     dQLastZeta(n1,i,j) = 0.0_rDef
    END DO
   END DO
  END DO

  include 'EtaSweep3DInviscid2.f90'

  DO k = iMinSweep(3,2)+dISweep(2),iMaxSweep(3,2),dISweep(2)
   kk = k - dISweep(2)

! if dISweep(2) = -1, then the A- matrix is subtracted at k+1
! if dISweep(2) = +1, then the A+ matrix is added at k-1

   DO j=iMinUpdate(2),iMaxUpdate(2)
    DO i=iMinUpdate(1),iMaxUpdate(1)

     DO n2=1,numberOfVariables
      DO n1=1,numberOfVariables
       aMatrixLast(n1,n2) = kFacA*aMatrixZeta(n1,n2,i,j,kk) ! +/-(1/2)*[A(j-/+1)]
      END DO
      aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + 0.5_rDef*epsilonZeta(i,j,kk) ! A+/- +/- (1/2)*epsilon(j-/+1)
!     aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + kFacA*epsilonZeta(i,j,kk) ! A+/- +/- (1/2)*epsilon(j-/+1)
     END DO

     dQLastZeta(:,i,j) = MATMUL(aMatrixLast,deltaQ(:,i,j,kk))

    END DO
   END DO

   include 'EtaSweep3DInviscid2.f90'

  END DO

