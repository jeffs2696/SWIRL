! - sweep 1 -- have deltaQ*, need RHSVec

  i = iMaxUpdate(1)

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
    rhsVector(nV,i,j) = -(ddDiag(nV)-ddJp1(nV,i))/deltaSigma(i,j)
   END DO

! and sweep...

  DO i=iMaxUpdate(1)-1,iMinUpdate(1),-1

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 0.0_rDef

     aMatrixIp1(ii,jj) = deltaSigma(i,j)*(               &
                        -0.5_rDef*aMatrixXi(ii,jj,i+1,j)) 
    END DO
    aMatrixIp1(jj,jj) = aMatrixIp1(jj,jj) &
                      + deltaSigma(i,j)*(0.5_rDef*epsilonXi(i+1,j)) ! -A^-

    aMatrix(jj,jj) = 1.0_rDef                          &
                   + deltaSigma(i,j)*(epsilonXi(i,j)   &
                                    + epsilonEta(i,j)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix(:,:),deltaQStar(:,i,j))

   ddIp1(:) = MATMUL(aMatrixIp1,deltaQStar(:,i+1,j))

   DO nV=1,numberOfVariables
    rhsVector(nV,i,j) = -(ddDiag(nV)  &
                        - ddIp1(nV)   &
                        - ddJp1(nV,i))/deltaSigma(i,j)
   END DO

  END DO
