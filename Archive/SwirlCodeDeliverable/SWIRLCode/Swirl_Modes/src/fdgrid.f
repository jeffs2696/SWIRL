      subroutine fdgrid(np,sig,x,r)
c
c Finite difference grid; equally spaced.
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
      dimension r(NMAX),x(NMAX)
c
      sigbar = (1. +sig)/(1. -sig)
      coeff  = 0.5*(1. -sig)
      dx = 2./float(np -1)
      do i = 1,np
       x(i) = -1. +(i -1)*dx
       r(i) = coeff*(x(i) +sigbar)
      enddo
c
      return
      end
