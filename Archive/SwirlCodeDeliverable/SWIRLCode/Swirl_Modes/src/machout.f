      subroutine machout(npts,rr,rmch,rmchp,rmsw,rmswp,snd,dsn,rhob)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
      dimension rr(NMAX),rmch(NMAX),rmchp(NMAX),rmsw(NMAX),rmswp(NMAX)
      dimension snd(NMAX),dsn(NMAX),rhob(NMAX)
c
c  Output Mach number, shear, and swirl distributions.
      open(unit=21,
     &     file='mach.dat',
     &     status='unknown')
      rewind 21
      write(21,5)
 5    format('#',5x,'r',8x,'M_x',5x,'dM_x/dr',5x,'M_th',3x,'dM_th/dr',
     &   4x,'M_tot',7x,'A',8x,'dA/dr',5x,'rhob',6x,'pbar')

      open(unit=22,
     &     file='machOut.dat',
     &     status='unknown')
      rewind(22)

      WRITE(22,*) nPts

      do i = 1,npts
       r    = rr(i)
       rm   = rmch(i)
       rt   = rmsw(i)
       rtot = sqrt(rm*rm +rt*rt)
       rhob(i) = snd(i)**5
       pbar = snd(i)**7
       write(21,10) r,rm,rmchp(i),rt,rmswp(i),rtot,snd(i),dsn(i),
     &    rhob(i),pbar
       write(22,*) r,rm,rt,rhob(i),pbar,snd(i)
      enddo
 10   format(10e15.4)
      close(21)
      close(22)
c
      return
      end
