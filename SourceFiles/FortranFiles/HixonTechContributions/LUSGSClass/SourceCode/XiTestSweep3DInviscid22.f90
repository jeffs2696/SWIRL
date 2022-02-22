
! + sweep 2:  know deltaQ, need deltaQStar

  i = iMinUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj) = 0.0_rDef
    END DO
    aMatrix(jj,jj) = 1.0_rDef                               &
                   + deltaSigma(i,j,k)*(epsilonXi(i,j,k)    &
                                      + epsilonEta(i,j,k)   &
                                      + epsilonZeta(i,j,k)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQ(:,i,j,k))

   DO nV=1,numberOfVariables
    deltaQStar(nV,i,j,k) = (ddDiag(nV)   &
                          - ddJm1(nV,i)  &
                          - ddKm1(nV,i,j))
   END DO

! now invert

! LAPACK call to solve the equation A X = B and return the
!  solution in rhsVec

   CALL DGESV(numberOfVariables,   & ! N
              NRHS,                & ! NRHS
              aMatrix,             & ! A 
              numberOfVariables,   & ! LDA
              IPIV,                & ! IPIV
              deltaQStar(:,i,j,k), & ! B
              numberOfVariables,   & ! LDB
              INFO)                  ! INFO

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

     aMatrixIm1(ii,jj) = deltaSigma(i,j,k)*(                  &
                           +0.5_rDef*aMatrixXi(ii,jj,i-1,j,k))  ! +A^+
    END DO
    aMatrixIm1(jj,jj) = aMatrixIm1(jj,jj) &
                      +( deltaSigma(i,j,k)*(0.5_rDef*epsilonXi(i-1,j,k))) ! +A^+

    aMatrix(jj,jj) = 1.0_rDef                               &
                   + deltaSigma(i,j,k)*(epsilonXi(i,j,k)    &
                                      + epsilonEta(i,j,k)   &
                                      + epsilonZeta(i,j,k)) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQ(:,i,j,k))

   ddIm1(:) = MATMUL(aMatrixIm1,deltaQ(:,i-1,j,k))

   DO nV=1,numberOfVariables
    deltaQStar(nV,i,j,k) = ddDiag(nV)  &
                         - ddIm1(nV)   &
                         - ddJm1(nV,i) &
                         - ddKm1(nV,i,j)
   END DO

! now invert

! LAPACK call to solve the equation A X = B and return the
!  solution in rhsVec

   CALL DGESV(numberOfVariables,   & ! N
              NRHS,                & ! NRHS
              aMatrix,             & ! A 
              numberOfVariables,   & ! LDA
              IPIV,                & ! IPIV
              deltaQStar(:,i,j,k), & ! B
              numberOfVariables,   & ! LDB
              INFO)                  ! INFO

   IF (INFO /= 0) THEN
    WRITE(charStringObject%charString,'(a,i5)') &
     'ERROR returned from DGESV (1) call!  INFO = ',INFO
     GO TO 100
   ELSE
   CONTINUE
   END IF

  END DO
