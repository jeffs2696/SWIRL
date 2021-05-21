      subroutine grid(np,sig,x,r)
c
C  COMPUTE THE CHEBYSHEV GAUSS-LOBATTO GRID, reversed.
c    See Canuto, p. 67.
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128)

      real*8 x(NMAX),r(NMAX),alpha,zero,sina
c
      print*,'Entering grid'
      PI     = 3.14159265358979324
      n      = np -1
      PIN    = PI/float(N)
      zero   = 1.11e-16
      alpha  = 1./cosh(abs(log(zero))/float(n))
      sina   = asin(alpha)
      sigbar = (1. +sig)/(1. -sig)
      coeff  = 0.5*(1. -sig)
      DO 10 j=1,NP
       x(j)  = -COS((j-1)*PIN)
       yj    =  asin(-x(j)*alpha)/sina
       r(j)  =  coeff*(-yj +sigbar)
 10   CONTINUE
      print*,'Leaving grid'
c
      return
      end

