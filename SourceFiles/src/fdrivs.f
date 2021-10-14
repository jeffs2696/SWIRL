      subroutine fdrivs(np,sig,dl1,iorder,ed2,ed4)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
      dimension dl1(NMAX,NMAX),dl2(NMAX,NMAX),dl4(NMAX,NMAX)
c
c Compute array for finite differences.
c
      do i = 1,np
       if (iorder.eq.2) then
        if (i.eq.1) then
         dl1(i,1) = -25.
         dl1(i,2) =  48.
         dl1(i,3) = -36.
         dl1(i,4) =  16.
         dl1(i,5) =  -3.
        elseif (i.eq.2) then
         dl1(i,1) =  -3.
         dl1(i,2) = -10.
         dl1(i,3) =  18.
         dl1(i,4) =  -6.
         dl1(i,5) =   1.
        elseif (i.eq.np-1) then
         dl1(i,np-4) =  -1.
         dl1(i,np-3) =   6.
         dl1(i,np-2) = -18.
         dl1(i,np-1) =  10.
         dl1(i,np)   =   3.
        elseif (i.eq.np) then
         dl1(i,np-4) =   3.
         dl1(i,np-3) = -16.
         dl1(i,np-2) =  36.
         dl1(i,np-1) = -48.
         dl1(i,np)   =  25.
        else
         dl1(i,i-2) =  1.
         dl1(i,i-1) = -8.
         dl1(i,i)   =  0.
         dl1(i,i+1) =  8.
         dl1(i,i+2) = -1.
        endif
       else
        if (i.eq.1) then
         dl1(i,1) =  -3.
         dl1(i,2) =   4.
         dl1(i,3) =  -1.
        elseif (i.eq.np) then
         dl1(i,np-2) =  1.
         dl1(i,np-1) = -4.
         dl1(i,np)   =  3.
        else 
         dl1(i,i-1) = -1.
         dl1(i,i)   =  0.
         dl1(i,i+1) =  1.
        endif
       endif
      enddo
      open(unit=15,file='deriv.matrix',status='unknown')
      rewind 15
      do i = 1,np
       write(15,10) (dl1(i,j), j=1,np)
      enddo
 10   format(1x,16f7.1)
      close(15)
c
      coeff  = 0.5*(1. -sig)
      dx = 2./float(np -1)
      do j = 1,np
       do i = 1,np
        dr       = coeff*dx
        if (iorder.eq.2) then
         dl1(i,j) = dl1(i,j)/(12.*dr)
        else
         dl1(i,j) = dl1(i,j)/(2.*dr)
        endif
       enddo
      enddo
c
c Compute 2nd and 4th derivative matrices.
      do k=1,np
       do j=1,np
        sum = 0.
        do i = 1,np
         sum = sum +dl1(k,i)*dl1(i,j)
        enddo
        dl2(k,j) = sum
       enddo
      enddo
      do k=1,np
       do j=1,np
        sum = 0.
        do i = 1,np
         sum = sum +dl2(k,i)*dl2(i,j)
        enddo
        dl4(k,j) = sum
       enddo
      enddo
c
c Correct 1st derivative matrix using 2nd and 4th order smoothing.
      do k=1,np
       do j=1,np
        dl1(k,j) = dl1(k,j) +ed2*dl2(k,j) +ed4*dl4(k,j)
       enddo
      enddo
c
      return
      end
