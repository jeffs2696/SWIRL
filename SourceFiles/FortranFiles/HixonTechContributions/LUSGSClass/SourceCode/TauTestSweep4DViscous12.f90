! - sweep 1 -- have deltaQ*, need RHSVec

  l = iMaxUpdate(4)

  DO k=iMinUpdate(3),iMaxUpdate(3)
   DO j=iMinUpdate(2),iMaxUpdate(2)
    DO i=iMinUpdate(1),iMaxUpdate(1)
   
     DO nV=1,numberOfVariables
      ddLp1(nV,i,j,k) = 0.0_rDef
     END DO

    END DO
   END DO
  END DO

! and sweep...

  include 'ZetaTestSweep4DViscous12.f90'

  DO l=iMaxUpdate(4)-1,iMinUpdate(4),-1

   DO k=iMinUpdate(3),iMaxUpdate(3)
    DO j=iMinUpdate(2),iMaxUpdate(2)
     DO i=iMinUpdate(1),iMaxUpdate(1)
      DO jj = 1,numberOfVariables
       DO ii = 1,numberOfVariables
        aMatrixLp1(ii,jj) = deltaSigma(i,j,k,l)*(                &
                           -0.5_rDef*aMatrixTau(ii,jj,i,j,k,l+1) &
                           +bMatrixTau(ii,jj,i,j,k,l+1))
       END DO
       aMatrixLp1(jj,jj) = aMatrixLp1(jj,jj) &
                         + deltaSigma(i,j,k,l)*(0.5_rDef*epsilonTau(i,j,k,l+1)) ! -A^-
 
      END DO

      ddLp1(:,i,j,k) = MATMUL(aMatrixLp1,deltaQStar(:,i,j,k,l+1))
 
     END DO
    END DO
   END DO

! and sweep...

   include 'ZetaTestSweep4DViscous12.f90'

  END DO
