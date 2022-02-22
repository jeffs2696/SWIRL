! - sweep 1 -- have deltaQ*, need RHSVec

  i = iMaxUpdate(1)

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

  DO i=iMaxUpdate(1)-1,iMinUpdate(1),-1

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 2.0_rDef*deltaSigma(i)*bMatrixXi(ii,jj,i)
     aMatrixIp1(ii,jj) = deltaSigma(i)*(                  &
                           -0.5_rDef*aMatrixXi(ii,jj,i+1) & ! -A^-
                           +bMatrixXi(ii,jj,i+1))
    END DO
    aMatrixIp1(jj,jj) = aMatrixIp1(jj,jj) &
                      + deltaSigma(i)*(0.5_rDef*epsilonXi(i+1)) ! -A^-

    aMatrix(jj,jj) = 1.0_rDef                   &
                   + deltaSigma(i)*epsilonXi(i) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQStar(:,i))

   ddIp1(:) = MATMUL(aMatrixIp1,deltaQStar(:,i+1))

   DO j=1,numberOfVariables
    rhsVector(j,i) = -(ddDiag(j) - ddIp1(j))/deltaSigma(i)
   END DO

  END DO
