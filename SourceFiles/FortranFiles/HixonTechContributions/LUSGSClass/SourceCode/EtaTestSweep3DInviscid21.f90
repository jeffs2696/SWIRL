
! - sweep 2:  know deltaQ, need deltaQStar

  j = iMaxUpdate(2)

  DO i=iMinUpdate(1),iMaxUpdate(1)
   DO nV=1,numberOfVariables
    ddJp1(nV,i) = 0.0_rDef
   END DO
  END DO

! sweep in xi

  include 'XiTestSweep3DInviscid21.f90'

! and sweep in eta...

  DO j=iMaxUpdate(2)-1,iMinUpdate(2),-1
   DO i=iMinUpdate(1),iMaxUpdate(1)

    DO jj = 1,numberOfVariables
     DO ii = 1,numberOfVariables
      aMatrixJp1(ii,jj) = deltaSigma(i,j,k)*(                &
                           -0.5_rDef*aMatrixEta(ii,jj,i,j+1,k))  ! -A^-
     END DO
     aMatrixJp1(jj,jj) = aMatrixJp1(jj,jj) &
                       -(-deltaSigma(i,j,k)*(0.5_rDef*epsilonEta(i,j+1,k))) ! -A^-

    END DO

    ddJp1(:,i) = MATMUL(aMatrixJp1,deltaQ(:,i,j+1,k))

   END DO

! sweep in xi

   include 'XiTestSweep3DInviscid21.f90'

  END DO
