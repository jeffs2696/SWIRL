! no j contribution to dQLast at iMinSweep(1)

  j  = iMinSweep(2,1)
  jFacA = 0.5_rDef*REAL(dISweep(1),rDef)

! do the xi sweep at j = jMinSweep

  DO i=iMinUpdate(1),iMaxUpdate(1)
   DO n1=1,numberOfVariables
    dQLastEta(n1,i) = 0.0_rDef
   END DO
  END DO

  include 'XiSweep2DInviscid1.f90'

  DO j = iMinSweep(2,1)+dISweep(1),iMaxSweep(2,1),dISweep(1)
   jj = j - dISweep(1)

! if dISweep(1) = -1, then the A- matrix is subtracted at j+1
! if dISweep(1) = +1, then the A+ matrix is added at j-1

   DO i=iMinUpdate(1),iMaxUpdate(1)
    DO n2=1,numberOfVariables
     DO n1=1,numberOfVariables
      aMatrixLast(n1,n2) = jFacA*aMatrixEta(n1,n2,i,jj) ! +/-(1/2)*[A(j-/+1)]
     END DO
     aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + 0.5_rDef*epsilonEta(i,jj) ! A+/- +/- (1/2)*epsilon(j-/+1)
!    aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + jFacA*epsilonEta(i,jj) ! A+/- +/- (1/2)*epsilon(j-/+1)
    END DO

    dQLastEta(:,i) = MATMUL(aMatrixLast,dQ(:,i,jj))

   END DO

   include 'XiSweep2DInviscid1.f90'

  END DO

