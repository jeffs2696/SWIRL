! no l contribution to dQLast at iMinSweep(2)

  l  = iMinSweep(4,2)
  lFacA = 0.5_rDef*REAL(dISweep(2),rDef)

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

  include 'ZetaSweep4DViscous2.f90'

  DO l = iMinSweep(4,2)+dISweep(2),iMaxSweep(4,2),dISweep(2)
   ll = l - dISweep(2)

! if dISweep(2) = -1, then the A- matrix is subtracted at l+1
! if dISweep(2) = +1, then the A+ matrix is added at l-1

! A matrix portion...

   DO k=iMinUpdate(3),iMaxUpdate(3)
    DO j=iMinUpdate(2),iMaxUpdate(2)
     DO i=iMinUpdate(1),iMaxUpdate(1)
      DO n2=1,numberOfVariables
       DO n1=1,numberOfVariables
        abMatrixLast(n1,n2) = lFacA*aMatrixTau(n1,n2,i,j,k,ll) & ! +/-(1/2)*[A(j-/+1)]
                                   +bMatrixTau(n1,n2,i,j,k,ll) 
       END DO
       abMatrixLast(n2,n2) = abMatrixLast(n2,n2) + 0.5_rDef*epsilonTau(i,j,k,ll) ! A+/- +/- (1/2)*epsilon(j-/+1)
!      abMatrixLast(n2,n2) = abMatrixLast(n2,n2) + lFacA*epsilonTau(i,j,k,ll) ! A+/- +/- (1/2)*epsilon(j-/+1)
      END DO

      dQLastTau(:,i,j,k) = MATMUL(abMatrixLast,deltaQ(:,i,j,k,ll))

     END DO
    END DO
   END DO

   include 'ZetaSweep4DViscous2.f90'

  END DO

