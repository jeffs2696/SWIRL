! - sweep 1 -- have deltaQ*, need RHSVec

  i = iMaxUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 2.0_rDef*deltaSigma(i,j,k)*(bMatrixXi(ii,jj,i,j,k)    &
                                                    +bMatrixEta(ii,jj,i,j,k)   &
                                                    +bMatrixZeta(ii,jj,i,j,k))
    END DO
    aMatrix(jj,jj) = 1.0_rDef                               &
                   + deltaSigma(i,j,k)*(epsilonXi(i,j,k)    &
                                      + epsilonEta(i,j,k)   &
                                      + epsilonZeta(i,j,k)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQStar(:,i,j,k))

   DO nV=1,numberOfVariables
    rhsVector(nV,i,j,k) = -(ddDiag(nV)   &
                           -ddJp1(nV,i)  &
                           -ddKp1(nV,i,j))/deltaSigma(i,j,k)
   END DO

! and sweep...

  DO i=iMaxUpdate(1)-1,iMinUpdate(1),-1

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 2.0_rDef*deltaSigma(i,j,k)*(bMatrixXi(ii,jj,i,j,k)  &
                                                   + bMatrixEta(ii,jj,i,j,k) &
                                                   + bMatrixZeta(ii,jj,i,j,k))

     aMatrixIp1(ii,jj) = deltaSigma(i,j,k)*(               &
                        -0.5_rDef*aMatrixXi(ii,jj,i+1,j,k) &
                        +bMatrixXi(ii,jj,i+1,j,k))
    END DO
    aMatrixIp1(jj,jj) = aMatrixIp1(jj,jj) &
                      + deltaSigma(i,j,k)*(0.5_rDef*epsilonXi(i+1,j,k)) ! -A^-

    aMatrix(jj,jj) = 1.0_rDef                               &
                   + deltaSigma(i,j,k)*(epsilonXi(i,j,k)    &
                                      + epsilonEta(i,j,k)   &
                                      + epsilonZeta(i,j,k)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix(:,:),deltaQStar(:,i,j,k))

   ddIp1(:) = MATMUL(aMatrixIp1,deltaQStar(:,i+1,j,k))

   DO nV=1,numberOfVariables
    rhsVector(nV,i,j,k) = -(ddDiag(nV)   &
                          - ddIp1(nV)    &
                          - ddJp1(nV,i)  &
                          - ddKp1(nV,i,j))/deltaSigma(i,j,k)
   END DO

  END DO
