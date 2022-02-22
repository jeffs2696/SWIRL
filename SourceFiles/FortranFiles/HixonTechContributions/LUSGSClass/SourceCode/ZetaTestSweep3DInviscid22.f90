
! + sweep 2:  know deltaQ, need deltaQStar

  k = iMinUpdate(3)

  DO j=iMinUpdate(2),iMaxUpdate(2)
   DO i=iMinUpdate(1),iMaxUpdate(1)
    DO nV=1,numberOfVariables
     ddKm1(nV,i,j) = 0.0_rDef
    END DO
   END DO
  END DO

! sweep in eta

  include 'EtaTestSweep3DInviscid22.f90'

! and sweep in zeta...

  DO k=iMinUpdate(3)+1,iMaxUpdate(3)
   DO j=iMinUpdate(2),iMaxUpdate(2)
    DO i=iMinUpdate(1),iMaxUpdate(1)

     DO jj = 1,numberOfVariables
      DO ii = 1,numberOfVariables
       aMatrixKm1(ii,jj) = deltaSigma(i,j,k)*(                   &
                            +0.5_rDef*aMatrixZeta(ii,jj,i,j,k-1))  ! +A^+
      END DO
      aMatrixKm1(jj,jj) = aMatrixKm1(jj,jj) &
                        +( deltaSigma(i,j,k)*(0.5_rDef*epsilonZeta(i,j,k-1))) ! +A^+
 
     END DO

     ddKm1(:,i,j) = MATMUL(aMatrixKm1,deltaQ(:,i,j,k-1))

    END DO
   END DO

! sweep in eta

   include 'EtaTestSweep3DInviscid22.f90'

  END DO
