! + sweep 1 -- have deltaQ*, need RHSVec

  i = iMinUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 2.0_rDef*deltaSigma(i,j,k)*(bMatrixXi(ii,jj,i,j,k)   &
                                                    +bMatrixEta(ii,jj,i,j,k)  &
                                                    +bMatrixZeta(ii,jj,i,j,k))
    END DO
    aMatrix(jj,jj) = 1.0_rDef                           &
                   + deltaSigma(i,j,k)*(epsilonXi(i,j,k)    &
                                      + epsilonEta(i,j,k)   &
                                      + epsilonZeta(i,j,k)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQStar(:,i,j,k))

   DO nV=1,numberOfVariables
    rhsVector(nV,i,j,k) = -(ddDiag(nV)  &
                           -ddJm1(nV,i) &
                           -ddKm1(nV,i,j))/deltaSigma(i,j,k)
   END DO

! and sweep...

  DO i=iMinUpdate(1)+1,iMaxUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 2.0_rDef*deltaSigma(i,j,k)*(bMatrixXi(ii,jj,i,j,k)  &
                                                   + bMatrixEta(ii,jj,i,j,k) &
                                                   + bMatrixZeta(ii,jj,i,j,k))

     aMatrixIm1(ii,jj) = deltaSigma(i,j,k)*(               &
                         0.5_rDef*aMatrixXi(ii,jj,i-1,j,k) &
                        +bMatrixXi(ii,jj,i-1,j,k))
    END DO
    aMatrixIm1(jj,jj) = aMatrixIm1(jj,jj) &
                      + deltaSigma(i,j,k)*(0.5_rDef*epsilonXi(i-1,j,k)) ! A^+

    aMatrix(jj,jj) = 1.0_rDef                             &
                   + deltaSigma(i,j,k)*(epsilonXi(i,j,k)    &
                                      + epsilonEta(i,j,k)   &
                                      + epsilonZeta(i,j,k)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix(:,:),deltaQStar(:,i,j,k))

   ddIm1(:) = MATMUL(aMatrixIm1,deltaQStar(:,i-1,j,k))

   DO nV=1,numberOfVariables
    rhsVector(nV,i,j,k) = -(ddDiag(nV)   &
                          - ddIm1(nV)    &
                          - ddJm1(nV,i)  &
                          - ddKm1(nV,i,j))/deltaSigma(i,j,k)
   END DO

  END DO
