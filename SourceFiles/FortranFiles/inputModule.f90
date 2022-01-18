MODULE inputModule
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: input

INTERFACE input
  MODULE PROCEDURE input1
END INTERFACE input

   INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine input1(mode,np,np4,rho,freq,ixp,nxp,irr,rxmax,slp, &
         iss,ang,gm,vlchar,vrchar,itst,etah,etad,irpt,ifdff,        &
         eps2,eps4,icomp)
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!     complex*16 etah,etad,freq
!     character  vlchar,vrchar
!
      INTEGER, INTENT(OUT) :: mode, &
                                np, &
                               np4, &
                               ixp, &
                               nxp, &
                               irr, &
                               iss, &
                              itst, &
                              irpt, &
                             ifdff, &
                             icomp

      REAL(KIND=rDef), INTENT(OUT) :: rho, &
                                    rxmax, &
                                      slp, &
                                      ang, &
                                       gm, &
                                     eps2, &
                                     eps4

      COMPLEX(KIND=rDef), INTENT(OUT) :: etah, &
                                         etad, &
                                         freq

      CHARACTER, INTENT(OUT) :: vlchar, &
                                vrchar

!
! local variables
!

      INTEGER :: mm = 0, &
               npts = 0, &
                 ix = 0, &
                 nx = 0, &
                 ir = 0, &
                 is = 0, &
              itest = 0, &
            irepeat = 0, &
                ifd = 0, &
              icmpr = 0, &
            ichange = 0
 
      REAL(KIND=rDef) :: sig = 0.0_rDef, &
                        akre = 0.0_rDef, &
                        akim = 0.0_rDef, &
                        rmax = 0.0_rDef, &
                       slope = 0.0_rDef, &
                       angom = 0.0_rDef, &
                         gam = 0.0_rDef, &
                       etahr = 0.0_rDef, &
                       etahi = 0.0_rDef, &
                       etadr = 0.0_rDef, &
                       etadi = 0.0_rDef, &
                         ed2 = 0.0_rDef, &
                         ed4 = 0.0_rDef, &
                        etar = 0.0_rDef, &
                        etai = 0.0_rDef
 
!
! define the NAMELIST for input (keeping from the original SWIRL code)
!
! adding a zero initialization in, which was NOT in the original
!
      NAMELIST /inputs/ mm, & ! circumferential mode number
                      npts, & ! number of radial mesh points
                       sig, & ! hub-to-duct radius ratio
                      akre, & ! real part of frequency
                      akim, & ! imag part of frequency
                        ix, & ! ? not in paper, and not actually used.
                        nx, & ! ? not in paper, and not actually used.
                        ir, & ! axial Mach number distribution flag
                      rmax, & ! max axial Mach number
                     slope, & ! slope of linear Mach distribution
                        is, & ! swirl Mach number distribution flag
                     angom, & ! magnitude of solid body swirl
                       gam, & ! magnitude of free vortex swirl
                     itest, & ! perform consistency test on selected modes?
                     etahr, & ! real part of hub liner admittance
                     etahi, & ! imag part of hub liner admittance
                     etadr, & ! real part of duct liner admittance
                     etadi, & ! imag part of duct liner admittance
                   irepeat, & ! ? not in paper, and is not actually used.
                       ifd, & ! use finite differences for derivatives flag
                       ed2, & ! second order smoothing for derivatives
                       ed4, & ! fourth order smoothing for derivatives
                     icmpr    ! compares result using another method (which doesn't work).
!
      OPEN(UNIT  = 16,           &
           FILE  = 'input.data', &
           FORM  = 'FORMATTED',  &
           STATUS= 'UNKNOWN')

      read(16,NML=INPUTS)
!
      mode  = mm
      np    = npts
      rho   = sig
      freq  = CMPLX(akre,akim,rDef)
      ixp   = ix
      nxp   = nx
      irr   = ir
      rxmax = rmax
      slp   = slope
      iss   = is
      ang   = angom
      gm    = gam
      itst  = itest
      irpt  = irepeat
      ifdff = ifd
      etah  = CMPLX(etahr,etahi,rDef)
      etad  = CMPLX(etadr,etadi,rDef)
      eps2  = ed2
      eps4  = ed4
      icomp = icmpr
!
      close(16)
!
      vlchar = 'N'
      vrchar = 'V'
      np4  = 4*np
!
      goto 1000
!
      write(6,10)
 10   format(1x,'Please input mode, number of points, hub/duct,', &
         ' and red. freq.')
      read(5,*) mode,np,rho,freq
      vlchar = 'N'
      vrchar = 'V'
      np4  = 4*np
      write(6,20)
 20   format(1x,'Please input ir.'/,5x,'ir = 0: uniform',/,5x, &
         'ir = 1: linear shear',/,5x,                          &
         'ir = 2: read from mach.input',/,5x,                  &
         'ir = 3: uniform core + bl. of thickness delta',/,5x, &
         'ir = 4: uniform + linear b.l.s',/,5x,                &
         'ir = 5: uniform + 1/7th power law b.l.s',/,5x,       &
         'ir = 6: hyperbolic secant',/,5x,                     &
         'ir = 7: laminar mean flow, M(r) = M_0 (1-r^2)',/,5x, &
         'ir = 8: wavy sinusoid; input M_max and M_min.')
      read(5,*) ir
      if (ir.eq.0) then
       write(6,35)
       read(5,*) rxmax
       slope = 0.
      elseif(ir.eq.1) then
       write(6,40)
       read(5,*) rxmax,slope
      elseif(ir.eq.3) then
       write(6,45)
       read(5,*) rxmax,slope
      elseif(ir.eq.4) then
       write(6,45)
       read(5,*) rxmax,slope
      elseif(ir.eq.5) then
       write(6,45)
       read(5,*) rxmax
      elseif(ir.eq.6) then
       write(6,55)
       read(5,*) rxmax,slope
      elseif(ir.eq.7) then
       write(6,50)
       read(5,*) rxmax
      elseif(ir.eq.8) then
       write(6,60)
       read(5,*) rxmax,slope
      endif
      write(6,100)
 100  format(1x,'Please input is.'/,5x,'is = 0: none',/,5x, &
         'is = 1: solid body swirl only',/,5x,              &
         'is = 2: free vortex swirl only',/,5x,             &
         'is = 3: both solid body and free vortex',/,5x,    &
         'is = 4: read from swrl.input')
      read(5,*) is
      if (is.eq.1) then
       write(6,110)
       read(5,*) angom
      elseif(is.eq.2) then
       write(6,120)
       read(5,*) gam
      elseif(is.eq.3) then
       write(6,130)
       read(5,*) angom,gam
      endif
      write(6,25)
 25   format(1x,'Please input real and imag parts of admittance.')
      read(5,*) etar,etai
 35   format(1x,'Please input the axial Mach number.')
 40   format(1x,'Please input max M and slope.')
 45   format(1x,'Please input core M.')
 50   format(1x,'Please input max axial M.')
 55   format(1x,'Please input max M and half-width.')
 60   format(1x,'Please input max M and min M.')
 110  format(1x,'Please input angular velocity.')
 120  format(1x,'Please input free vortex strength.')
 130  format(1x,'Please input ang.vel. and vortex strength.')

 1000 continue
!
! Echo input.
      write(6,30) np,mode,rho,freq,ir,rxmax,slope,is,angom,gam, &
         itest,etah,irepeat,etad,ifd,eps2,eps4,ixp,nxp,icomp
 30   format(/,1x,'npts  = ',i4,/                                        &
         ,1x,'mm    = ',i4,3x,'r_H/r_D = ',f10.6,3x,'k     = ',          &
         2f10.6,/,1x,'ir    = ',i4,3x,'rmax    = ',                      &
         f10.6,3x,'slope = ',f10.6,/,1x,'is    = ',i4,3x,'angom   = ',   &
         f10.6,3x,'gam   = ',f10.6,/,1x,'it    = ',i4,3x,'etah    =  (', &
         f8.6,',',f8.6,')',/,1x,'irpt  = ',i4,3x,                        &
         'etad    =  (',f8.6,',',f8.6,')',/,1x,'ifdff = ',i4,3x,         &
         'ed2     = ',f10.6,3x,'ed4   = ',f10.6,/,1x,'ix    = ',i4,3x,   &
         'nx      = ',i4,3x,'icmpr = ',i4)
!
! the user _was_ able to change things; no more.
      ichange = 0
!     write(6,140)

!140  format(1x,'Input 0 to continue,',/,               &
!        1x,'      1 to change namelist entries, or',/, &
!        1x,'      2 to show namelist entries.')
 150  format(1x,'Input via &inputs &end.')

! the user _was_ able to change things; no more.
!     read(5,*) ichange


      if (ichange.eq.0) then
       write(6,200)
      elseif (ichange.eq.2) then
       write(6,NML=inputs)
       goto 1000
      elseif(ichange.eq.1) then
       write(6,150)
       read(5,NML=inputs)
       mode  = mm
       np    = npts
       rho   = sig
       freq  = CMPLX(akre,akim,rDef)
       ixp   = ix
       nxp   = nx
       irr   = ir
       rxmax = rmax
       slp   = slope
       iss   = is
       ang   = angom
       gm    = gam
       itst  = itest
       irpt  = irepeat
       ifdff = ifd
       etah  = CMPLX(etahr,etahi,rDef)
       etad  = CMPLX(etadr,etadi,rDef)
       eps2  = ed2
       eps4  = ed4
       icomp = icmpr
       vlchar = 'N'
       vrchar = 'V'
       np4  = 4*np
       goto 1000
      endif
 200  format(1x,'Continuing ...')
!
      return
      end

END MODULE inputModule
