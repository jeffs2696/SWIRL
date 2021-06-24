      subroutine sndspd (np,rr,rmsw,asnd,dsnd,dd,rhob,angom,gam,sig,is)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128)
      dimension rr(NMAX),rmsw(NMAX),asnd(NMAX),dsnd(NMAX),rhob(NMAX)
      dimension dd(NMAX,NMAX)
c
      gm  = 1.4
      gm1 = gm -1.
      do j = 1,np
        r  = rr(j)
        if (is.eq.0) then
          asnd(j) =  1.
          dsnd(j) =  0.
        elseif (is.eq.1) then
          asnd(j) =  1. -gm1/2.*angom*angom*(1. -r*r)
          asnd(j) =  sqrt(asnd(j))
          dsnd(j) =  gm1*angom*angom*r/(2.*asnd(j))
        elseif (is.eq.2) then
          asnd(j) =  1. -gm1/2.*gam*gam*(1./(r*r) -1.)
          asnd(j) =  sqrt(asnd(j))
          dsnd(j) =  gm1*gam*gam/(2.*r*r*r*asnd(j))
        elseif (is.eq.3) then
          ang     = -gm1/2.*angom*angom*(1. -r*r)
          agm     = -gm1/2.*gam*gam*(1./(r*r) -1.)
          alg     =  2*gm1*angom*gam*log(r)
          asnd(j) =  1. +ang +agm +alg
          asnd(j) =  sqrt(asnd(j))
          dsnd(j) =  gm1/2.*(angom*r +gam/r)**2/(r*asnd(j))
        elseif (is.eq.4) then
          asnd(j) =  1. -gm1/4.*gam*gam*(1./(r**4) -1.)
          asnd(j) =  sqrt(asnd(j))
          dsnd(j) =  gm1*gam*gam/(2.*r**5*asnd(j))
        elseif (is.eq.6) then
          asnd(j) = 1. +gm1*angom*angom*log(r)
          asnd(j) = sqrt(asnd(j))
          dsnd(j) = gm1*angom*angom/(2.*r*asnd(j))
c$$$       else 
c$$$        asnd(j) = 0.
c$$$        do i = np,j,-1
c$$$         if (i.eq.np) then
c$$$          asnd(i) = 0.
c$$$         else
c$$$          if (rr(i).ne.0.) then
c$$$           rswi    = rmsw(i)*rmsw(i)/rr(i)
c$$$           rsw1    = rmsw(i+1)*rmsw(i+1)/rr(i+1)
c$$$           xi      = rr(i)
c$$$           x1      = rr(i+1)
c$$$           asnd(i) = asnd(i+1) +0.5*(rswi +rsw1)*(xi -x1)
c$$$          else
c$$$           asnd(i) = 2.*rmsw(i)*(rmsw(i+1) -rmsw(i))/rr(i+1)
c$$$          endif
c$$$         endif
c$$$        enddo
c$$$        asnd(j) = exp(-gm1*asnd(j))
        endif
      enddo
      if (is.eq.5) then
        do k = 1,np
          sum = 0.
          do j = 1,np
            sum = sum +dd(k,j)*asnd(j)
          enddo
          dsnd(k) = sum
        enddo
      endif
c
      return
      end
