! + sweep 1 -- have deltaQ*, need RHSVec

  i = iMinUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 2.0_rDef*deltaSigma(i)*bMatrixXi(ii,jj,i)
    END DO
    aMatrix(jj,jj) = 1.0_rDef                   &
                   + deltaSigma(i)*epsilonXi(i) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQStar(:,i))

   DO j=1,numberOfVariables
    rhsVector(j,i) = -(ddDiag(j))/deltaSigma(i)
   END DO

! and sweep...

  DO i=iMinUpdate(1)+1,iMaxUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 2.0_rDef*deltaSigma(i)*bMatrixXi(ii,jj,i)
     aMatrixIm1(ii,jj) = deltaSigma(i)*(                &
                            0.5_rDef*aMatrixXi(ii,jj,i-1) &
                           +bMatrixXi(ii,jj,i-1))
    END DO
    aMatrixIm1(jj,jj) = aMatrixIm1(jj,jj) &
                      + deltaSigma(i)*(0.5_rDef*epsilonXi(i-1)) ! A^+

    aMatrix(jj,jj) = 1.0_rDef                   &
                   + deltaSigma(i)*epsilonXi(i) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQStar(:,i))

   ddiM1(:) = MATMUL(aMatrixiM1,deltaQStar(:,i-1))

   DO j=1,numberOfVariables
    rhsVector(j,i) = -(ddDiag(j) - ddiM1(j))/deltaSigma(i)
   END DO

  END DO
