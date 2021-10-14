      subroutine boundary(np,sig,ak,etah,etad,rmx,rmt,dd,aa,bb)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128)
c
      real*8     dd(NMAX,NMAX),rmx(NMAX),rmt(NMAX)
      complex*16 aa(4*NMAX,4*NMAX),bb(4*NMAX,4*NMAX),etah,etad,ci,ak
c
c Boundary conditions.
      ci      = (0.,1.)
      eps     = 1.e-4
c
      etamag = abs(etah) +abs(etad)
      if (etamag.gt.eps) then
       rmh    = sqrt(rmx(1)*rmx(1)   +rmt(1)*rmt(1))
       rmd    = sqrt(rmx(np)*rmx(np) +rmt(np)*rmt(np))
       aa(np,np)   = ci*ak
       aa(np,2*np) = 0.
       do j=1,np
        aa(np,3*np+j) = 0.
       enddo
       aa(np,4*np) = -ci*ak*etad
       bb(np,np)   = 0.
       bb(np,4*np) = etad*rmd
       if (sig.ne.0.) then
        aa(1,1)    = -ci*ak
        aa(1,np+1) = 0.
        do j=1,np
         aa(1,3*np+j) = 0.
        enddo
        aa(1,3*np+1) = -ci*ak*etah
        bb(1,1)      = 0.
        bb(1,3*np+1) = etah*rmh
       endif
      else
       if (sig.ne.0.) then
        do j = 1,np
         aa(1,  np+j) = 0.
         aa(1,3*np+j) = 0.
         bb(1,     j) = 0.
        enddo
       endif
       do j = 1,np
        aa(np,  np+j) = 0.
        aa(np,3*np+j) = 0.
        bb(np,     j) = 0.
       enddo
      endif
c
      return
      end
