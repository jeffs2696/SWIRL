! + sweep 1 -- have deltaQ*, need RHSVec

  i = iMinUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 0.0_rDef
    END DO
    aMatrix(jj,jj) = 1.0_rDef                          &
                   + deltaSigma(i,j)*(epsilonXi(i,j)   &
                                    + epsilonEta(i,j)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQStar(:,i,j))

   DO nV=1,numberOfVariables
    rhsVector(nV,i,j) = -(ddDiag(nV)-ddJm1(nV,i))/deltaSigma(i,j)
   END DO

! and sweep...

  DO i=iMinUpdate(1)+1,iMaxUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 0.0_rDef

     aMatrixIm1(ii,jj) = deltaSigma(i,j)*(               &
                         0.5_rDef*aMatrixXi(ii,jj,i-1,j))
    END DO
    aMatrixIm1(jj,jj) = aMatrixIm1(jj,jj) &
                      + deltaSigma(i,j)*(0.5_rDef*epsilonXi(i-1,j)) ! A^+

    aMatrix(jj,jj) = 1.0_rDef                          &
                   + deltaSigma(i,j)*(epsilonXi(i,j)   &
                                    + epsilonEta(i,j)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix(:,:),deltaQStar(:,i,j))

   ddIm1(:) = MATMUL(aMatrixIm1,deltaQStar(:,i-1,j))

   DO nV=1,numberOfVariables
    rhsVector(nV,i,j) = -(ddDiag(nV)  &
                        - ddIm1(nV)   &
                        - ddJm1(nV,i))/deltaSigma(i,j)
   END DO

  END DO
