      subroutine global(np,np4,sig,mode,om,snd,dd,
     &   rr,rx,dr,rt,dt,aa,bb)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
c
      parameter  (gm = 1.4)
      real*8     dd(NMAX,NMAX),rx(NMAX),dr(NMAX),rt(NMAX)
      real*8     dt(NMAX),snd(NMAX),rr(NMAX),r
      complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4),ci,om
c
      do j=1,np4
       do k=1,np4
        aa(k,j) = 0.
        bb(k,j) = 0.
       enddo
      enddo
c
      ci      = (0.,1.)
c
c Global matrices.
      do k=1,np
       r   = rr(k)
       do j=1,np
        aa(     k,3*np+j) = dd(k,j)
        aa(3*np+k,     j) = dd(k,j)
        if (k.eq.j) then
         aa(     k,     j) = -ci*om/snd(k)
         aa(  np+k,  np+j) = -ci*om/snd(k)
         aa(2*np+k,2*np+j) = -ci*om/snd(k)
         aa(3*np+k,3*np+j) = -ci*om/snd(k)
         aa(  np+k,     j) = dt(j)
         aa(2*np+k,     j) = dr(j)
         if (r.ne.0.) then
          aa(  np+k,3*np+j) = ci*float(mode)/r
          aa(3*np+k,  np+j) = ci*float(mode)/r
          aa(3*np+k,     j) = aa(3*np+k,j) +1.d0/r
     &       +(gm +1.)*rt(j)*rt(j)/(2.*r)
          aa(     k,     j) = aa(     k,     j) +ci*float(mode)*rt(j)/r
          aa(  np+k,  np+j) = aa(  np+k,  np+j) +ci*float(mode)*rt(j)/r
          aa(2*np+k,2*np+j) = aa(2*np+k,2*np+j) +ci*float(mode)*rt(j)/r
          aa(3*np+k,3*np+j) = aa(3*np+k,3*np+j) +ci*float(mode)*rt(j)/r
          aa(     k,  np+j) = -2.*rt(j)/r
          aa(     k,3*np+j) = aa(k,3*np+j) +(gm -1.d0)*rt(j)*rt(j)/r
          aa(  np+k,     j) = aa(np+k,j) +rt(j)/r
     &       +(gm -1.d0)*rt(j)**3/(2.*r)
          aa(2*np+k,     j) = aa(2*np+k,j)
     &       +(gm -1.d0)*rx(j)*rt(j)*rt(j)/(2.d0*r)
         else
          aa(  np+k,3*np+j) = 0.
          aa(3*np+k,  np+j) = 0.
          aa(     k,     j) = aa(     k,     j) +ci*float(mode)*dt(j)
          aa(  np+k,  np+j) = aa(  np+k,  np+j) +ci*float(mode)*dt(j)
          aa(2*np+k,2*np+j) = aa(2*np+k,2*np+j) +ci*float(mode)*dt(j)
          aa(3*np+k,3*np+j) = aa(3*np+k,3*np+j) +ci*float(mode)*dt(j)
          aa(     k,  np+j) = -2.*dt(j)
          aa(     k,3*np+j) = aa(k,3*np+j) +2.d0*(gm -1.d0)*rt(j)*dt(j)
          aa(  np+k,     j) = aa(np+k,j)
     &       +(1.d0 +3.d0*(gm -1.d0)*rt(j)*rt(j)/2.d0)*dt(j)
          aa(2*np+k,     j) = aa(2*np+k,j)
     &       +(gm -1.d0)/2.d0*rt(j)*(dr(j)*rt(j) +2.d0*rx(j)*dt(j))
          aa(3*np+k,     j) = aa(3*np+k,j)
     &       +(gm +1.d0)*rt(j)*dt(j)
         endif
         bb(     k,     j) = rx(j)
         bb(  np+k,  np+j) = rx(j)
         bb(2*np+k,2*np+j) = rx(j)
         bb(3*np+k,3*np+j) = rx(j)
         bb(2*np+k,3*np+j) = 1.
         bb(3*np+k,2*np+j) = 1.
        endif
       enddo
      enddo

c
      return
      end

