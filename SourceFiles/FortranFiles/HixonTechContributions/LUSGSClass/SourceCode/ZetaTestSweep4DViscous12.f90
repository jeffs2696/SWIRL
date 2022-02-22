! - sweep 1 -- have deltaQ*, need RHSVec

  k = iMaxUpdate(3)

  DO j=iMinUpdate(2),iMaxUpdate(2)
   DO i=iMinUpdate(1),iMaxUpdate(1)
   
    DO nV=1,numberOfVariables
     ddKp1(nV,i,j) = 0.0_rDef
    END DO

   END DO
  END DO

! and sweep...

  include 'EtaTestSweep4DViscous12.f90'

  DO k=iMaxUpdate(3)-1,iMinUpdate(3),-1

   DO j=iMinUpdate(2),iMaxUpdate(2)
    DO i=iMinUpdate(1),iMaxUpdate(1)
     DO jj = 1,numberOfVariables
      DO ii = 1,numberOfVariables
       aMatrixKp1(ii,jj) = deltaSigma(i,j,k,l)*(                 &
                          -0.5_rDef*aMatrixZeta(ii,jj,i,j,k+1,l) &
                          +bMatrixZeta(ii,jj,i,j,k+1,l))
      END DO
      aMatrixKp1(jj,jj) = aMatrixKp1(jj,jj) &
                        + deltaSigma(i,j,k,l)*(0.5_rDef*epsilonZeta(i,j,k+1,l)) ! -A^-
 
     END DO

     ddKp1(:,i,j) = MATMUL(aMatrixKp1,deltaQStar(:,i,j,k+1,l))

    END DO
   END DO

! and sweep...

   include 'EtaTestSweep4DViscous12.f90'

  END DO
