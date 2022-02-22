
! + sweep 2:  know deltaQ, need deltaQStar

  l = iMinUpdate(4)

  DO k=iMinUpdate(3),iMaxUpdate(3)
   DO j=iMinUpdate(2),iMaxUpdate(2)
    DO i=iMinUpdate(1),iMaxUpdate(1)
     DO nV=1,numberOfVariables
      ddLm1(nV,i,j,k) = 0.0_rDef
     END DO
    END DO
   END DO
  END DO

! sweep in Zeta

  include 'ZetaTestSweep4DViscous22.f90'

! and sweep in zeta...

  DO l=iMinUpdate(4)+1,iMaxUpdate(4)
   DO k=iMinUpdate(3),iMaxUpdate(3)
    DO j=iMinUpdate(2),iMaxUpdate(2)
     DO i=iMinUpdate(1),iMaxUpdate(1)

      DO jj = 1,numberOfVariables
       DO ii = 1,numberOfVariables
        aMatrixLm1(ii,jj) = deltaSigma(i,j,k,l)*(                   &
                             +0.5_rDef*aMatrixTau(ii,jj,i,j,k,l-1) & ! +A^+
                             +bMatrixTau(ii,jj,i,j,k,l-1))
       END DO
       aMatrixLm1(jj,jj) = aMatrixLm1(jj,jj) &
                         +( deltaSigma(i,j,k,l)*(0.5_rDef*epsilonTau(i,j,k,l-1))) ! +A^+
  
      END DO
 
      ddLm1(:,i,j,k) = MATMUL(aMatrixLm1,deltaQ(:,i,j,k,l-1))
 
     END DO
    END DO
   END DO

! sweep in Zeta

   include 'ZetaTestSweep4DViscous22.f90'

  END DO
