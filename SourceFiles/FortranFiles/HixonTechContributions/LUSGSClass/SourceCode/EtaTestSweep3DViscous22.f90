
! + sweep 2:  know deltaQ, need deltaQStar

  j = iMinUpdate(2)

  DO i=iMinUpdate(1),iMaxUpdate(1)
   DO nV=1,numberOfVariables
    ddJm1(nV,i) = 0.0_rDef
   END DO
  END DO

! sweep in xi

  include 'XiTestSweep3DViscous22.f90'

! and sweep in eta...

  DO j=iMinUpdate(2)+1,iMaxUpdate(2)
   DO i=iMinUpdate(1),iMaxUpdate(1)

    DO jj = 1,numberOfVariables
     DO ii = 1,numberOfVariables
      aMatrixJm1(ii,jj) = deltaSigma(i,j,k)*(                &
                           +0.5_rDef*aMatrixEta(ii,jj,i,j-1,k) & ! +A^+
                           +bMatrixEta(ii,jj,i,j-1,k))
     END DO
     aMatrixJm1(jj,jj) = aMatrixJm1(jj,jj) &
                       +( deltaSigma(i,j,k)*(0.5_rDef*epsilonEta(i,j-1,k))) ! +A^+

    END DO

    ddJm1(:,i) = MATMUL(aMatrixJm1,deltaQ(:,i,j-1,k))

   END DO

! sweep in xi

   include 'XiTestSweep3DViscous22.f90'

  END DO
