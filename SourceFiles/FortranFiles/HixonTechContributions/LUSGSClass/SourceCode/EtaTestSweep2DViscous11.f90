! + sweep 1 -- have deltaQ*, need RHSVec

  j = iMinUpdate(2)

  DO i=iMinUpdate(1),iMaxUpdate(1)
   
   DO nV=1,numberOfVariables
    ddJm1(nV,i) = 0.0_rDef
   END DO

  END DO

! and sweep...

  include 'XiTestSweep2DViscous11.f90'

  DO j=iMinUpdate(2)+1,iMaxUpdate(2)

   DO i=iMinUpdate(1),iMaxUpdate(1)
    DO jj = 1,numberOfVariables
     DO ii = 1,numberOfVariables
      aMatrixJm1(ii,jj) = deltaSigma(i,j)*(                &
                          0.5_rDef*aMatrixEta(ii,jj,i,j-1) &
                         +bMatrixEta(ii,jj,i,j-1))
     END DO
     aMatrixJm1(jj,jj) = aMatrixJm1(jj,jj) &
                       + deltaSigma(i,j)*(0.5_rDef*epsilonEta(i,j-1)) ! A^+

    END DO

    ddJm1(:,i) = MATMUL(aMatrixJm1,deltaQStar(:,i,j-1))

   END DO

! and sweep...

   include 'XiTestSweep2DViscous11.f90'

  END DO
