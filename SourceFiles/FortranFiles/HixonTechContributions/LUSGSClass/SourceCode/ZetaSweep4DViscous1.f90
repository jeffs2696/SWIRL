! no k contribution to dQLast at iMinSweep(1)

  k  = iMinSweep(3,1)
  kFacA = 0.5_rDef*REAL(dISweep(1),rDef)

! do the xi sweep at k = kMinSweep

  DO j=iMinUpdate(2),iMaxUpdate(2)
   DO i=iMinUpdate(1),iMaxUpdate(1)
    DO n1=1,numberOfVariables
     dQLastZeta(n1,i,j) = 0.0_rDef
    END DO
   END DO
  END DO

  include 'EtaSweep4DViscous1.f90'

  DO k = iMinSweep(3,1)+dISweep(1),iMaxSweep(3,1),dISweep(1)
   kk = k - dISweep(1)

! if dISweep(1) = -1, then the A- matrix is subtracted at k+1
! if dISweep(1) = +1, then the A+ matrix is added at k-1

   DO j=iMinUpdate(2),iMaxUpdate(2)
    DO i=iMinUpdate(1),iMaxUpdate(1)
     DO n2=1,numberOfVariables
      DO n1=1,numberOfVariables
       abMatrixLast(n1,n2) = kFacA*aMatrixZeta(n1,n2,i,j,kk,l) & ! +/-(1/2)*[A(j-/+1)]
                                  +bMatrixZeta(n1,n2,i,j,kk,l)
      END DO
      abMatrixLast(n2,n2) = abMatrixLast(n2,n2) + 0.5_rDef*epsilonZeta(i,j,kk,l) ! A+/- +/- (1/2)*epsilon(j-/+1)
!     abMatrixLast(n2,n2) = abMatrixLast(n2,n2) + kFacA*epsilonZeta(i,j,kk,l) ! A+/- +/- (1/2)*epsilon(j-/+1)
     END DO

     dQLastZeta(:,i,j) = MATMUL(abMatrixLast,dQ(:,i,j,kk,l))

    END DO
   END DO

   include 'EtaSweep4DViscous1.f90'

  END DO

