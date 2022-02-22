! no j contribution to dQLast at iMinSweep(2)

  j  = iMinSweep(2,2)
  jFacA = 0.5_rDef*REAL(dISweep(2),rDef)

! do the xi sweep at j = jMinSweep

  DO i=iMinUpdate(1),iMaxUpdate(1)
   DO n1=1,numberOfVariables
    dQLastEta(n1,i) = 0.0_rDef
   END DO
  END DO

  include 'XiSweep4DViscous2.f90'

  DO j = iMinSweep(2,2)+dISweep(2),iMaxSweep(2,2),dISweep(2)
   jj = j - dISweep(2)

! if dISweep(2) = -1, then the A- matrix is subtracted at j+1
! if dISweep(2) = +1, then the A+ matrix is added at j-1

   DO i=iMinUpdate(1),iMaxUpdate(1)
    DO n2=1,numberOfVariables
     DO n1=1,numberOfVariables
      abMatrixLast(n1,n2) = jFacA*aMatrixEta(n1,n2,i,jj,k,l) & ! +/-(1/2)*[A(j-/+1)]
                                 +bMatrixEta(n1,n2,i,jj,k,l)
     END DO
     abMatrixLast(n2,n2) = abMatrixLast(n2,n2) + 0.5_rDef*epsilonEta(i,jj,k,l) ! A+/- +/- (1/2)*epsilon(j-/+1)
!    abMatrixLast(n2,n2) = abMatrixLast(n2,n2) + jFacA*epsilonEta(i,jj,k,l) ! A+/- +/- (1/2)*epsilon(j-/+1)
    END DO

    dQLastEta(:,i) = MATMUL(abMatrixLast,deltaQ(:,i,jj,k,l))

   END DO

   include 'XiSweep4DViscous2.f90'

  END DO

