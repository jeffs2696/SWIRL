MODULE globalModule
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: globalM

INTERFACE globalM
  MODULE PROCEDURE globalM1
END INTERFACE globalM

   INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine globalM1(np,np4,sig,mode,om,snd,dd, &
         rr,rx,dr,rt,dt,aa,bb)

!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!
!     parameter  (gm = 1.4)
!     real*8     dd(NMAX,NMAX),rx(NMAX),dr(NMAX),rt(NMAX)
!     real*8     dt(NMAX),snd(NMAX),rr(NMAX),r
!     complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4),ci,om
!
! note that, in F77, a 'complex*16' really means that the
!     two components of the complex number are real*8
!

      INTEGER, INTENT(IN) :: np,  &
                             np4, &
                             mode

      REAL(KIND=rDef), INTENT(IN) :: sig

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: dd

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: snd, &
                                                   rr,  &
                                                   rx,  &
                                                   dr,  &
                                                   dt
      COMPLEX(KIND=rDef), INTENT(IN) :: om

      COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(OUT) :: aa, &
                                                         bb

! define local variables

      REAL(KIND=rDef), PARAMETER :: gm = 1.4_rDef

      INTEGER :: j,  &
                 j1, &
                 j2, &
                 j3, &
                 k,  &
                 k1, &
                 k2, &
                 k3

      REAL(KIND=rDef) :: r

      REAL(KIND=rDef), DIMENSION(np) :: rt

      COMPLEX(KIND=rDef) :: ci

! zero out the matrices

      do j=1,np4
       do k=1,np4
        aa(k,j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
        bb(k,j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
       enddo
      enddo
!
      ci = CMPLX(0.0_rDef,1.0_rDef,rDef)
!
! Global matrices.
!
! np x np matrix, with 4x4 blocks in each entry
!
! In this one, the matrix is (4*np)x(4*np).
!
      do k=1,np        ! k  == v_r
       k1 =   np + k   ! k1 == v_{\theta}
       k2 = 2*np + k   ! k2 == v_x
       k3 = 3*np + k   ! k3 == p

       r   = rr(k)

       do j=1,np       ! j  == v_r
        j1 =   np + j  ! j1 == v_{\theta}
        j2 = 2*np + j  ! j2 == v_x
        j3 = 3*np + j  ! j3 == p

! dd is the derivative matrix for the radial direction

! derivatives appear in two locations in the [A] block matrix: 
!       (1,4) and (4,1)

        aa(k,j3) = dd(k,j)  ! (1,4) : v_r eqn, p entry     d/dr
        aa(k3,j) = dd(k,j)  ! (4,1) : p   eqn, v_r entry   d/dr

        if (k.eq.j) then
         aa(k,j)   = -ci*om/snd(k) ! (1,1) : v_r eqn, v_r entry:         -i k/A
         aa(k1,j1) = -ci*om/snd(k) ! (2,2) : v_theta eqn, v_theta entry: -i k/A
         aa(k2,j2) = -ci*om/snd(k) ! (3,3) : v_x eqn, v_x entry:         -i k/A
         aa(k3,j3) = -ci*om/snd(k) ! (4,4) : p eqn, p entry:             -i k/A
         aa(k1,j)  = dt(j)         ! (2,1) : v_theta eqn, v_r entry: dM_th/dr
         aa(k2,j)  = dr(j)         ! (3,1) : v_x eqn, v_r entry: dM_x/dr

         if (r.ne.0.0_rDef) then

          aa(k1,j3) = ci*REAL(mode,rDef)/r ! (2,4): v_theta eqn, p entry: i m/r

          aa(k3,j1) = ci*REAL(mode,rDef)/r ! (4,2): p eqn, v_theta entry: i m/r

          aa(k3,j)  = aa(k3,j)      & ! (4,1): p eqn, v_r entry:
                     +1.0_rDef/r    & !                          +1/r
                     +(gm +1.0_rDef)*rt(j)*rt(j)/(2.0_rDef*r) !  + ((gam+1)M_th^2)/(2r)

          aa(k,j)   = aa(k,j) &                  ! (1,1): v_r eqn, v_r entry:
                     +ci*REAL(mode,rDef)*rt(j)/r !    +(i m M_th)/r
          aa(k1,j1) = aa(k1,j1) &                ! (2,2): v_th eqn, v_th entry:
                     +ci*REAL(mode,rDef)*rt(j)/r !    +(i m M_th)/r
          aa(k2,j2) = aa(k2,j2) &                ! (3,3): v_x eqn, v_x entry:
                     +ci*REAL(mode,rDef)*rt(j)/r !    +(i m M_th)/r
          aa(k3,j3) = aa(k3,j3) &                ! (4,4): p eqn, p entry:
                     +ci*REAL(mode,rDef)*rt(j)/r !    +(i m M_th)/r
          aa(k,j1)  = -2.0_rDef*rt(j)/r          ! (1,2): v_r eqn, v_th entry:  -(2 M_th)/r
          aa(k,j3)  = aa(k,j3) &                 ! (1,4): v_r eqn, p entry:
                     +(gm -1.0_rDef)*rt(j)*rt(j)/r !  + (gam-1)*(M_th^2)/r
          aa(k1,j) = aa(k1,j) &                  ! (2,1): v_th eqn, v_r entry:
                     +rt(j)/r &                  !                   +(M_th)/r
                     +(gm -1.0_rDef)*(rt(j)**3)/(2.0_rDef*r) !       +((gam-1)*M_th^3)/(2 r)
          aa(k2,j) = aa(k2,j) &                  ! (3,1): v_x eqn, v_r entry:
                     +(gm -1.0_rDef)*rx(j)*rt(j)*rt(j)/(2.0_rDef*r) ! +((gam-1)*M_x*M_th^2)/(2 r)
         else ! r == 0
          aa(k1,j3) = CMPLX(0.0_rDef,0.0_rDef,rDef) ! (2,4): v_th eqn, p entry: 0
          aa(k3,j1) = CMPLX(0.0_rDef,0.0_rDef,rDef) ! (4,2): p eqn, v_th entry: 0
          aa(k,j)   = aa(k,j) &                     ! (1,1): v_r eqn, v_r entry:
                     +ci*REAL(mode,rDef)*dt(j)      !    + i m M_th
          aa(k1,j1) = aa(k1,j1) &                   ! (2,2): v_th eqn, v_th entry:
                     +ci*REAL(mode,rDef)*dt(j)      !    + i m M_th
          aa(k2,j2) = aa(k2,j2) &                   ! (3,3): v_x eqn, v_x entry:
                     +ci*REAL(mode,rDef)*dt(j)      !    + i m M_th
          aa(k3,j3) = aa(k3,j3) &                   ! (4,4): p eqn, p entry:
                     +ci*REAL(mode,rDef)*dt(j)      !    + i m M_th
          aa(k,j1)  = -2.0_rDef*dt(j)               ! (1,2): v_r eqn, v_th entry: - 2 M_th
          aa(k,j3)  = aa(k,j3) &                    ! (1,4): v_r eqn, p entry:
                     +2.0_rDef*(gm -1.0_rDef)*rt(j)*dt(j)  ! + 2 (gam-1)*M_th*dM_th/dr
          aa(k1,j)  = aa(k1,j) &                    ! (2,1): v_th eqn, v_r entry: 
                     +(1.0_rDef +3.0_rDef*(gm -1.0_rDef)*rt(j)*rt(j)/2.0_rDef)*dt(j)
                                                    !        +(1+3*((gam-1)/2)*Mth^2)*dM_th/dr
          aa(k2,j)  = aa(k2,j) &                    ! (3,1): v_x eqn, v_r entry:
                     +(gm -1.0_rDef)/2.0_rDef*rt(j)*(dr(j)*rt(j) +2.0_rDef*rx(j)*dt(j))
                                                    !  +((gam-1)/2)*Mth*(Mth*dMx/dr + 2*Mx*dMth/dr)
          aa(k3,j) = aa(k3,j) &                     ! (4,1): p eqn, v_r entry:
                     +(gm +1.0_rDef)*rt(j)*dt(j)    !    + (gam+1)*M_th*dMth/dr
         endif
         bb(k,j)   = rx(j)                          ! (1,1): v_r eqn, v_r entry: M_x
         bb(k1,j1) = rx(j)                          ! (2,2): v_th eqn, v_th entry: M_x
         bb(k2,j2) = rx(j)                          ! (3,3): v_x eqn, v_x entry: M_x
         bb(k3,j3) = rx(j)                          ! (4,4): p eqn, p entry: M_x
         bb(k2,j3) = CMPLX(1.0_rDef,0.0_rDef,rDef)  ! (3,4): v_x eqn, p entry: 1.0
         bb(k3,j2) = CMPLX(1.0_rDef,0.0_rDef,rDef)  ! (4,3): p eqn, v_x entry: 1.0
        endif
       enddo
      enddo
!
      return
      IF (sig > 0.0_rDef) CONTINUE ! sig is not actually used in this routine
      end

END MODULE globalModule
