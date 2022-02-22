! - sweep 1 -- have deltaQ*, need RHSVec

  j = iMaxUpdate(2)

  DO i=iMinUpdate(1),iMaxUpdate(1)
   
   DO nV=1,numberOfVariables
    ddJp1(nV,i) = 0.0_rDef
   END DO

  END DO

! and sweep...

  include 'XiTestSweep2DInviscid12.f90'

  DO j=iMaxUpdate(2)-1,iMinUpdate(2),-1

   DO i=iMinUpdate(1),iMaxUpdate(1)
    DO jj = 1,numberOfVariables
     DO ii = 1,numberOfVariables
      aMatrixJp1(ii,jj) = deltaSigma(i,j)*(                &
                         -0.5_rDef*aMatrixEta(ii,jj,i,j+1)) 
     END DO
     aMatrixJp1(jj,jj) = aMatrixJp1(jj,jj) &
                       + deltaSigma(i,j)*(0.5_rDef*epsilonEta(i,j+1)) ! -A^-

    END DO

    ddJp1(:,i) = MATMUL(aMatrixJp1,deltaQStar(:,i,j+1))

   END DO

! and sweep...

   include 'XiTestSweep2DInviscid12.f90'

  END DO
