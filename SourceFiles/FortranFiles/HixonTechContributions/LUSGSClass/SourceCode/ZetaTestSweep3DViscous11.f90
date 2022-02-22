! + sweep 1 -- have deltaQ*, need RHSVec

  k = iMinUpdate(3)

  DO j=iMinUpdate(2),iMaxUpdate(2)
   DO i=iMinUpdate(1),iMaxUpdate(1)
   
    DO nV=1,numberOfVariables
     ddKm1(nV,i,j) = 0.0_rDef
    END DO

   END DO
  END DO

! and sweep...

  include 'EtaTestSweep3DViscous11.f90'

  DO k=iMinUpdate(3)+1,iMaxUpdate(3)

   DO j=iMinUpdate(2),iMaxUpdate(2)
    DO i=iMinUpdate(1),iMaxUpdate(1)
     DO jj = 1,numberOfVariables
      DO ii = 1,numberOfVariables
       aMatrixKm1(ii,jj) = deltaSigma(i,j,k)*(                 &
                           0.5_rDef*aMatrixZeta(ii,jj,i,j,k-1) &
                          +bMatrixZeta(ii,jj,i,j,k-1))
      END DO
      aMatrixKm1(jj,jj) = aMatrixKm1(jj,jj) &
                        + deltaSigma(i,j,k)*(0.5_rDef*epsilonZeta(i,j,k-1)) ! A^+
 
     END DO

     ddKm1(:,i,j) = MATMUL(aMatrixKm1,deltaQStar(:,i,j,k-1))

    END DO
   END DO

! and sweep...

   include 'EtaTestSweep3DViscous11.f90'

  END DO
