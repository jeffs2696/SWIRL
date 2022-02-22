! - sweep 2:  know deltaQ, need deltaQStar

  i = iMaxUpdate(1)

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 0.0_rDef
    END DO
    aMatrix(jj,jj) = 1.0_rDef                   &
                   + deltaSigma(i)*epsilonXi(i) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQ(:,i))

   DO j=1,numberOfVariables
    deltaQStar(j,i) = ddDiag(j)/aMatrix(1,1) 
   END DO

! now invert

! LAPACK call to solve the equation A X = B and return the
!  solution in rhsVec

!  CALL DGESV(numberOfVariables, & ! N
!             NRHS,              & ! NRHS
!             aMatrix,           & ! A 
!             numberOfVariables, & ! LDA
!             IPIV,              & ! IPIV
!             deltaQStar(:,i),   & ! B
!             numberOfVariables, & ! LDB
!             INFO)                ! INFO

!  IF (INFO /= 0) THEN
!   WRITE(charStringObject%charString,'(a,i5)') &
!    'ERROR returned from DGESV (1) call!  INFO = ',INFO
!    GO TO 100
!  ELSE
!  CONTINUE
!  END IF
  
! and sweep...

  DO i=iMaxUpdate(1)-1,iMinUpdate(1),-1

   DO jj = 1,numberOfVariables
    DO ii = 1,numberOfVariables
     aMatrix(ii,jj)    = 0.0_rDef
     aMatrixIp1(ii,jj) = deltaSigma(i)*(                &
                           -0.5_rDef*aMatrixXi(ii,jj,i+1))  ! -A^-
    END DO
    aMatrixIp1(jj,jj) = aMatrixIp1(jj,jj) &
                      -(-deltaSigma(i)*(0.5_rDef*epsilonXi(i+1))) ! -A^-

    aMatrix(jj,jj) = 1.0_rDef &
                   + deltaSigma(i)*epsilonXi(i) &
                   + aMatrix(jj,jj)
   END DO

   ddDiag(:) = MATMUL(aMatrix,deltaQ(:,i))

   ddIp1(:) = MATMUL(aMatrixIp1,deltaQ(:,i+1))

   DO j=1,numberOfVariables
    deltaQStar(j,i) = (ddDiag(j) - ddIp1(j))/aMatrix(1,1)
   END DO

! now invert

! LAPACK call to solve the equation A X = B and return the
!  solution in rhsVec

!  CALL DGESV(numberOfVariables, & ! N
!             NRHS,              & ! NRHS
!             aMatrix,           & ! A 
!             numberOfVariables, & ! LDA
!             IPIV,              & ! IPIV
!             deltaQStar(:,i),   & ! B
!             numberOfVariables, & ! LDB
!             INFO)                ! INFO

!  IF (INFO /= 0) THEN
!   WRITE(charStringObject%charString,'(a,i5)') &
!    'ERROR returned from DGESV (1) call!  INFO = ',INFO
!    GO TO 100
!  ELSE
!  CONTINUE
!  END IF

  END DO
