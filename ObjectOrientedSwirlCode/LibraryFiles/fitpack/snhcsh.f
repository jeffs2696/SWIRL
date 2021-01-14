
      subroutine snhcsh (sinhm,coshm,x,isw)
c
      integer isw
      real sinhm,coshm,x
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine returns approximations to
c       sinhm(x) = sinh(x)/x-1
c       coshm(x) = cosh(x)-1
c and
c       coshmm(x) = (cosh(x)-1-x*x/2)/(x*x)
c with relative error less than 1.0e-6
c
c on input--
c
c   x contains the value of the independent variable.
c
c   isw indicates the function desired
c           = -1 if only sinhm is desired,
c           =  0 if both sinhm and coshm are desired,
c           =  1 if only coshm is desired,
c           =  2 if only coshmm is desired,
c           =  3 if both sinhm and coshmm are desired.
c
c on output--
c
c   sinhm contains the value of sinhm(x) if isw .le. 0 or
c   isw .eq. 3 (sinhm is unaltered if isw .eq.1 or isw .eq.
c   2).
c
c   coshm contains the value of coshm(x) if isw .eq. 0 or
c   isw .eq. 1 and contains the value of coshmm(x) if isw
c   .ge. 2 (coshm is unaltered if isw .eq. -1).
c
c and
c
c   x and isw are unaltered.
c
c-----------------------------------------------------------
c
      data sp13/.3029390e-5/,
     *     sp12/.1975135e-3/,
     *     sp11/.8334261e-2/,
     *     sp10/.1666665e0/
      data sp24/.3693467e-7/,
     *     sp23/.2459974e-5/,
     *     sp22/.2018107e-3/,
     *     sp21/.8315072e-2/,
     *     sp20/.1667035e0/
      data sp33/.6666558e-5/,
     *     sp32/.6646307e-3/,
     *     sp31/.4001477e-1/,
     *     sq32/.2037930e-3/,
     *     sq31/-.6372739e-1/,
     *     sq30/.6017497e1/
      data sp43/.2311816e-4/,
     *     sp42/.2729702e-3/,
     *     sp41/.9868757e-1/,
     *     sq42/.1776637e-3/,
     *     sq41/-.7549779e-1/,
     *     sq40/.9110034e1/
      data cp4/.2982628e-6/,
     *     cp3/.2472673e-4/,
     *     cp2/.1388967e-2/,
     *     cp1/.4166665e-1/,
     *     cp0/.5000000e0/
c
      ax = abs(x)
      if (isw .ge. 0) go to 5
c
c sinhm approximation
c
      if (ax .gt. 4.45) go to 2
      xs = ax*ax
      if (ax .gt. 2.3) go to 1
c
c sinhm approximation on (0.,2.3)
c
      sinhm = xs*(((sp13*xs+sp12)*xs+sp11)*xs+sp10)
      return
c
c sinhm approximation on (2.3,4.45)
c
    1 sinhm = xs*((((sp24*xs+sp23)*xs+sp22)*xs+sp21)
     .               *xs+sp20)
      return
    2 if (ax .gt. 7.65) go to 3
c
c sinhm approximation on (4.45,7.65)
c
      xs = ax*ax
      sinhm = xs*(((sp33*xs+sp32)*xs+sp31)*xs+1.)/
     .             ((sq32*xs+sq31)*xs+sq30)
      return
    3 if (ax .gt. 10.1) go to 4
c
c sinhm approximation on (7.65,10.1)
c
      xs = ax*ax
      sinhm = xs*(((sp43*xs+sp42)*xs+sp41)*xs+1.)/
     .             ((sq42*xs+sq41)*xs+sq40)
      return
c
c sinhm approximation above 10.1
c
    4 sinhm = exp(ax)/(ax+ax)-1.
      return
c
c coshm and (possibly) sinhm approximation
c
    5 if (isw .ge. 2) go to 7
      if (ax .gt. 2.3) go to 6
      xs = ax*ax
      coshm = xs*((((cp4*xs+cp3)*xs+cp2)*xs+cp1)*xs+cp0)
      if (isw .eq. 0) sinhm = xs*(((sp13*xs+sp12)*xs+sp11)
     .                              *xs+sp10)
      return
    6 expx = exp(ax)
      coshm = (expx+1./expx)/2.-1.
      if (isw .eq. 0) sinhm = (expx-1./expx)/(ax+ax)-1.
      return
c
c coshmm and (possibly) sinhm approximation
c
    7 xs = ax*ax
      if (ax .gt. 2.3) go to 8
      coshm = xs*(((cp4*xs+cp3)*xs+cp2)*xs+cp1)
      if (isw .eq. 3) sinhm = xs*(((sp13*xs+sp12)*xs+sp11)
     .                              *xs+sp10)
      return
    8 expx = exp(ax)
      coshm = ((expx+1./expx-xs)/2.-1.)/xs
      if (isw .eq. 3) sinhm = (expx-1./expx)/(ax+ax)-1.
      return
      end
