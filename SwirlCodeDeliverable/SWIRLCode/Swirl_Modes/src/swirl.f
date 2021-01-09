      program swirl
c
      implicit real*8 (a-h,o-z)
c
c nmax = 4*nrad
c
      parameter (NMAX = 256, NMAX4 = NMAX*4)
c
      dimension dl1(NMAX,NMAX),y(NMAX),rwork(8*NMAX4),r(NMAX)
      dimension rmx(NMAX),drm(NMAX),rmt(NMAX),drt(NMAX)
      dimension snd(NMAX),dsn(NMAX),rho(NMAX)
      dimension akap(NMAX)
      character jobvl,jobvr
c
      complex*16 alpha(NMAX4),beta(NMAX4),work(2*NMAX4)
      complex*16 etah,etad,ak
      complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4),vph(NMAX4)
      complex*16 vl(NMAX4,NMAX4),vr(NMAX4,NMAX4),wvn(NMAX4)
c
c "sig" is the hub/duct ratio.
c "ak"  is the reduced frequency omega r_d/c.
c "mm"  is the circumferential mode number.
c "rmx" is the axial Mach number.
c "drm" is the derivative of the axial Mach number.
c "r" is the physical space vector, from sigma to 1.
c "y" is the mapping into -1 to 1.
c
 100  continue
c
c Get input quantities.
      call input(mm,np,np4,sig,ak,ix,nx,ir,rxmax,slope,is,angom,gam,
     &     jobvl,jobvr,itest,etah,etad,irepeat,ifdff,ed2,ed4,icomp)
c
c Set up Gauss-Lobatto grid and compute Chebyshev derivative matrix.
      if (is .ne. -1) then
        if (ifdff.eq.0) then
          call grid(np,sig,y,r)
          call derivs(np,sig,dl1,ed2,ed4)
        else
          call fdgrid(np,sig,y,r)
          call fdrivs(np,sig,dl1,ifdff,ed2,ed4)
        endif
c
c Speed of sound and Mach number distributions and output.
        call sndspd(np,r,rmt,snd,dsn,dl1,rho,angom,gam,sig,is)
        call smach(np,r,rmt,drt,snd,dsn,dl1,angom,gam,sig,is)
        call rmach(np,r,rmx,drm,snd,dsn,dl1,rxmax,slope,sig,ir)
      else
        call interp(np,sig,r,rmx,drm,rmt,drt,snd,dsn,dl1,ifdff,ed2,ed4)
      endif
      call machout(np,r,rmx,drm,rmt,drt,snd,dsn,rho)
c
c Set up global matrices.

      call global(np,np4,sig,mm,ak,snd,dl1,r,rmx,drm,rmt,drt,aa,bb)

      call boundary(np,sig,ak,etah,etad,rmx,rmt,dl1,aa,bb)

      call analysis(np,np4,ak,r,snd,rmx,rmt,aa,bb,alpha,beta,vl,vr,
     &     work,rwork,wvn,jobvl,jobvr,mm,ir,is,slope,vph,akap)

      call output(np,np4,mm,sig,ak,rxmax,slope,angom,gam,jobvr,
     &     etah,etad,rmx,drm,rmt,drt,snd,r,wvn,vr,vph,is,icomp)
c
      if (irepeat.eq.1) goto 100
c
      stop
      end
