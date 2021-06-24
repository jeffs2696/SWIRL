MODULE derivsModule

  USE, INTRINSIC :: ISO_FORTRAN_ENV

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: derivs

INTERFACE derivs
  MODULE PROCEDURE derivs1
END INTERFACE derivs

CONTAINS

      subroutine derivs1(np,sig,dl1,ed2,ed4)
!

!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128)
!     dimension  DL1(NMAX,NMAX),dl2(NMAX,NMAX),dl4(NMAX,NMAX)
!     dimension  trans(NMAX)

      INTEGER, PARAMETER :: rDef = REAL64

      INTEGER, INTENT(IN) :: np
      REAL(KIND=rDef), INTENT(IN) :: sig, &
                                     ed2, &
                                     ed4

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(INOUT) :: dl1

! local variables

      INTEGER :: n,     &
                 nhalf, &
                 i,     &
                 j,     &
                 k

      REAL(KIND=rDef) :: pi,    &
                         pinr,  &
                         pinh,  &
                         zero,  &
                         alpha, &
                         sina,  &
                         xj,    &
                         xk,    &
                         cbk,   &
                         cbj,   &
                         xdif,  &
                         tot,   &
                         fac,   &
                         s1,    &
                         s2,    &
                         gpr

      REAL(KIND=rDef), DIMENSION(np) :: trans
      REAL(KIND=rDef), DIMENSION(np,np) :: dl2, &
                                           dl4
!
!  COMPUTE ARRAY FOR CHEBYSHEV DIFFERENTIATION AT FACES.
!   See Canuto, p.69.
!
!  Modified to follow Wai Don and Solomonoff recommendations.
!
!      WRITE(6,*) 'Entering derivs'

      n     = np -1
      pi    = 4.0_rDef*ATAN(1.0_rDef)

      pinr  = pi/REAL(n,rDef)
      pinh  = 0.5_rDef*pinr

      zero  = 1.11e-16_rDef

      alpha = 1.0_rDef/COSH(ABS(LOG(zero))/REAL(n,rDef))
      sina  = ASIN(alpha)
!
!
!DRH: bugfix 9/23/17.  nhalf calculation was incorrect
!                      for odd number of input points.
!
!     if (MOD(n,2).eq.0) then
!      nhalf = np/2
!     else
!      nhalf = np/2 +1
!     endif

      nhalf = CEILING(0.5_rDef*REAL(np,rDef))

!     WRITE(6,*) 'derivs: n     = ',n
!     WRITE(6,*) 'derivs: nhalf = ',nhalf
!
      do k=1,nhalf
!
! calculate x_k using Eqn. (3.2)
!
       xk   = COS(REAL(k-1,rDef)*pinr)
!
! this is cBar_j in Eqn. (3.6)
!
       cbk  = 1.0_rDef
       if (k.eq.1 .or. k.eq.np) cbk = 2.0_rDef

       do j=1,np
        xj  = COS(REAL(j-1)*pinr)
        cbj = 1.0_rDef
        if (j.eq.1 .or. j.eq.np) cbj = 2.0_rDef

! Eqn. (3.8) 

        if (j.ne.k) then
         xdif       = -2.0_rDef*SIN(pinh*REAL(k+j-2,rDef))*SIN(pinh*REAL(k-j,rDef))
         dl1(k,j)   = (cbk/cbj)*(-1.0_rDef)**(k+j)/xdif
        elseif (k.eq.1) then
         dl1(1,1)   =  (2.0_rDef*REAL(n*n,rDef) +1.0_rDef)/6.0_rDef
        else
         dl1(k,j)   = -xj/(2.0_rDef*SIN(REAL(j-1,rDef)*pinr)**2)
        endif

       enddo
      enddo
      do k=nhalf+1,np
       do j=1,np
        dl1(k,j) = -dl1(np-k+1,np-j+1)
       enddo
      enddo
      do k=1,np
       xk = COS(REAL(k-1,rDef)*pinr)
       gpr = alpha/sina/SQRT(1.0_rDef -(alpha*xk)**2)
       trans(k) = 1.0_rDef/gpr
      enddo
      do k = 1,np
       fac = trans(k)
       do j = 1,np
        dl1(k,j) = fac*dl1(k,j)
       enddo
      enddo
!
! Switch signs on 1st derivative matrix due to mesh orientation
!  from -1 to 1.
      s1      = 1.0_rDef -sig
      s2      = 2.0_rDef/s1
      do k=1,np
       do j=1,np
        dl1(k,j) = -dl1(k,j)*s2
       enddo
      enddo
!
! Compute 2nd and 4th derivative matrices.
      do k=1,np
       do j=1,np
        tot = 0.0_rDef
        do i = 1,np
         tot = tot +dl1(k,i)*dl1(i,j)
        enddo
        dl2(k,j) = tot
       enddo
      enddo
      do k=1,np
       do j=1,np
        tot = 0.0_rDef
        do i = 1,np
         tot = tot +dl2(k,i)*dl2(i,j)
        enddo
        dl4(k,j) = tot
       enddo
      enddo
!
! Correct 1st derivative matrix using 2nd and 4th order smoothing.

      do k=1,np
       do j=1,np
        dl1(k,j) = dl1(k,j) +ed2*dl2(k,j) +ed4*dl4(k,j)
       enddo
      enddo
!
!      WRITE(6,*) 'Leaving derivs'
      return
      end

END MODULE derivsModule
