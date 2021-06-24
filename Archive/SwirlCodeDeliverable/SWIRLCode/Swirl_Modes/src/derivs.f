      subroutine derivs(np,sig,dl1,ed2,ed4)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128)
      dimension  DL1(NMAX,NMAX),dl2(NMAX,NMAX),dl4(NMAX,NMAX)
      dimension  trans(NMAX)
c
C  COMPUTE ARRAY FOR CHEBYSHEV DIFFERENTIATION AT FACES.
c   See Canuto, p.69.
c
c  Modified to follow Wai Don and Solomonoff recommendations.
c
      print*,'Entering derivs'
      n     = np -1
      PINR  = 3.1415926535898/N 
      PINH  = 0.5*PINr
      zero  = 1.11d-16
      alpha = 1./cosh(abs(log(zero))/n)
      sina  = asin(alpha)
c
      if (mod(n,2).eq.0) then
       nhalf = np/2
      else
       nhalf = np/2 +1
      endif
c
      do k=1,nhalf
       xk   = cos((k -1)*pinr)
       cbk  = 1.0
       if (k.eq.1 .or. k.eq.np) cbk = 2.0
       do j=1,np
        xj  = cos((j -1)*pinr)
        cbj = 1.0
        if (j.eq.1 .or. j.eq.np) cbj = 2.0
        if (j.ne.k) then
         xdif       = -2.*sin(PINH*(k +j -2))*sin(PINH*(k -j))
         dl1(k,j)   = (cbk/cbj)*(-1.0)**(k +j)/xdif
        elseif (k.eq.1) then
         dl1(1,1)   =  (2.*n*n +1.)/6.
        else
         dl1(k,j)   = -xj/(2.*sin((j -1)*PINR)**2)
        endif
       enddo
      enddo
      do k=nhalf+1,np
       do j=1,np
        dl1(k,j) = -dl1(np-k+1,np-j+1)
       enddo
      enddo
      do k=1,np
       xk = cos((k -1)*pinr)
       gpr = alpha/sina/sqrt(1. -(alpha*xk)**2)
       trans(k) = 1/gpr
      enddo
      do k = 1,np
       fac = trans(k)
       do j = 1,np
        dl1(k,j) = fac*dl1(k,j)
       enddo
      enddo
c
c Switch signs on 1st derivative matrix due to mesh orientation
c  from -1 to 1.
      s1      = 1. -sig
      s2      = 2./s1
      do k=1,np
       do j=1,np
        dl1(k,j) = -dl1(k,j)*s2
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
      print*,'Leaving derivs'
      return
      end
