! no l contribution to dQLast at iMinSweep(1)

  l  = iMinSweep(4,1)
  lFacA = 0.5_rDef*REAL(dISweep(1),rDef)

! do the tau sweep at l = lMinSweep

  DO k=iMinUpdate(3),iMaxUpdate(3)
   DO j=iMinUpdate(2),iMaxUpdate(2)
    DO i=iMinUpdate(1),iMaxUpdate(1)
     DO n1=1,numberOfVariables
      dQLastTau(n1,i,j,k) = 0.0_rDef
     END DO
    END DO
   END DO
  END DO

  include 'ZetaSweep4DInviscid1.f90'

  DO l = iMinSweep(4,1)+dISweep(1),iMaxSweep(4,1),dISweep(1)
   ll = l - dISweep(1)

! if dISweep(1) = -1, then the A- matrix is subtracted at l+1
! if dISweep(1) = +1, then the A+ matrix is added at l-1

   DO k=iMinUpdate(3),iMaxUpdate(3)
    DO j=iMinUpdate(2),iMaxUpdate(2)
     DO i=iMinUpdate(1),iMaxUpdate(1)
      DO n2=1,numberOfVariables
       DO n1=1,numberOfVariables
        aMatrixLast(n1,n2) = lFacA*aMatrixTau(n1,n2,i,j,k,ll) ! +/-(1/2)*[A(j-/+1)]
       END DO
       aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + 0.5_rDef*epsilonTau(i,j,k,ll) ! A+/- +/- (1/2)*epsilon(j-/+1)
!      aMatrixLast(n2,n2) = aMatrixLast(n2,n2) + lFacA*epsilonTau(i,j,k,ll) ! A+/- +/- (1/2)*epsilon(j-/+1)
      END DO

      dQLastTau(:,i,j,k) = MATMUL(aMatrixLast,dQ(:,i,j,k,ll))

     END DO
    END DO
   END DO

   include 'ZetaSweep4DInviscid1.f90'

  END DO

