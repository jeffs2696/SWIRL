
! + sweep 2:  know deltaQ, need deltaQStar

  i = iMinUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj) = 0.0_rDef
    END DO
    aMatrix(jj,jj) = 1.0_rDef                          &
                   + deltaSigma(i,j)*(epsilonXi(i,j)   &
                                    + epsilonEta(i,j)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQ(:,i,j))

   DO nV=1,numberOfVariables
    deltaQStar(nV,i,j) = (ddDiag(nV) - ddJm1(nV,i))
   END DO

! now invert

! LAPACK call to solve the equation A X = B and return the
!  solution in rhsVec

   CALL DGESV(numberOfVariables, & ! N
              NRHS,              & ! NRHS
              aMatrix,           & ! A 
              numberOfVariables, & ! LDA
              IPIV,              & ! IPIV
              deltaQStar(:,i,j), & ! B
              numberOfVariables, & ! LDB
              INFO)                ! INFO

   IF (INFO /= 0) THEN
    WRITE(charStringObject%charString,'(a,i5)') &
     'ERROR returned from DGESV (1) call!  INFO = ',INFO
     GO TO 100
   ELSE
   CONTINUE
   END IF
  
! and sweep...

  DO i=iMinUpdate(1)+1,iMaxUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj) = 0.0_rDef
     aMatrixIm1(ii,jj) = deltaSigma(i,j)*(                  &
                           +0.5_rDef*aMatrixXi(ii,jj,i-1,j))  ! +A^+
    END DO
    aMatrixIm1(jj,jj) = aMatrixIm1(jj,jj) &
                      +( deltaSigma(i,j)*(0.5_rDef*epsilonXi(i-1,j))) ! +A^+

    aMatrix(jj,jj) = 1.0_rDef                          &
                   + deltaSigma(i,j)*(epsilonXi(i,j)   &
                                    + epsilonEta(i,j)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQ(:,i,j))

   ddIm1(:) = MATMUL(aMatrixIm1,deltaQ(:,i-1,j))

   DO nV=1,numberOfVariables
    deltaQStar(nV,i,j) = ddDiag(nV)  &
                       - ddIm1(nV)   &
                       - ddJm1(nV,i)
   END DO

! now invert

! LAPACK call to solve the equation A X = B and return the
!  solution in rhsVec

   CALL DGESV(numberOfVariables, & ! N
              NRHS,              & ! NRHS
              aMatrix,           & ! A 
              numberOfVariables, & ! LDA
              IPIV,              & ! IPIV
              deltaQStar(:,i,j), & ! B
              numberOfVariables, & ! LDB
              INFO)                ! INFO

   IF (INFO /= 0) THEN
    WRITE(charStringObject%charString,'(a,i5)') &
     'ERROR returned from DGESV (1) call!  INFO = ',INFO
     GO TO 100
   ELSE
   CONTINUE
   END IF

  END DO
