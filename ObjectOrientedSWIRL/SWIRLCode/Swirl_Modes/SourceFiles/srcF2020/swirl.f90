      program swirl
!
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      USE analysisModule
      USE boundaryModule
      USE derivsModule
      USE fdgridModule
      USE fdrivsModule
      USE globalModule
      USE gridModule
      USE inputModule
      USE interpModule
      USE machoutModule
      USE outputModule
      USE rmachModule
      USE smachAndSndspdModule

      IMPLICIT NONE

!     implicit real*8 (a-h,o-z)
!
! nmax = 4*nrad
!
!     PARAMETER (NMAX = 256, NMAX4 = NMAX*4)
!

!     dimension dl1(NMAX,NMAX),y(NMAX),rwork(8*NMAX4),r(NMAX)
!     dimension rmx(NMAX),drm(NMAX),rmt(NMAX),drt(NMAX)
!     dimension snd(NMAX),dsn(NMAX),rho(NMAX)
!     dimension akap(NMAX)
! TYPE Name of type that i want 
      REAL(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: dl1
      REAL(KIND=REAL64), DIMENSION(:), ALLOCATABLE :: y,     &
                                                      rwork, &
                                                      r,     &
                                                      rmx,   &
                                                      drm,   &
                                                      rmt,   &
                                                      drt,   &
                                                      snd,   &
                                                      dsn,   &
                                                      rho,   &
                                                      akap     

      character :: jobvl,jobvr
!
!     complex*16 alpha(NMAX4),beta(NMAX4),work(2*NMAX4)
!     complex*16 etah,etad,ak
!     complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4),vph(NMAX4)
!     complex*16 vl(NMAX4,NMAX4),vr(NMAX4,NMAX4),wvn(NMAX4)
!
! note that, in F77, 'complex*16' actually means that the two
!   arguments of the complex number are real*8
!
      complex(KIND=REAL64), DIMENSION(:), ALLOCATABLE :: alpha, &
                                                         beta,  &
                                                         work,  &
                                                         vph,   &
                                                         wvn

      complex(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: aa, &
                                                           bb, &
                                                           vl, &
                                                           vr

      complex(KIND=REAL64) etah,etad,ak

! additional variables, implicitly defined in the original code

      INTEGER :: icomp,   &
                 ifdff,   &
                 ir,      &
                 irepeat, &
                 is,      &
                 itest,   &
                 ix,      &
                 mm,      &
                 np,      &
                 np4,     &
                 nx

      REAL(KIND=REAL64) :: angom, &
                           ed2,   &
                           ed4,   &
                           gam,   &
                           rxmax, &
                           sig,   &
                           slope
      !END TYPE
! 
! "sig" is the hub/duct ratio.
! "ak"  is the reduced frequency omega r_d/c.
! "mm"  is the circumferential mode number.
! "rmx" is the axial Mach number.
! "drm" is the derivative of the axial Mach number.
! "r" is the physical space vector, from sigma to 1.
! "y" is the mapping into -1 to 1.
!
 100  continue
!
! Get input quantities.
      CALL input(mode   = mm,      &
                 np     = np,      &
                 np4    = np4,     &
                 rho    = sig,     &
                 freq   = ak,      &
                 ixp    = ix,      &
                 nxp    = nx,      &
                 irr    = ir,      &
                 rxmax  = rxmax,   &
                 slp    = slope,   &
                 iss    = is,      &
                 ang    = angom,   &
                 gm     = gam,     &
                 vlchar = jobvl,   &
                 vrchar = jobvr,   &
                 itst   = itest,   &
                 etah   = etah,    &
                 etad   = etad,    &
                 irpt   = irepeat, &
                 ifdff  = ifdff,   &
                 eps2   = ed2,     &
                 eps4   = ed4,     &
                 icomp  = icomp)
!
! NEW: allocate data arrays
!

      ALLOCATE(dl1(np,np),   &
               y(np),        &
               rwork(8*np4), &
               r(np),        &
               rmx(np),      &
               drm(np),      &
               rmt(np),      &
               drt(np),      &
               snd(np),      &
               dsn(np),      &
               rho(np),      &
               akap(np4),    & ! was np in code, but may go to np4 in analysisModule
               alpha(np4),   &
               beta(np4),    &
               work(2*np4),  &
               vph(np4),     &
               wvn(np4),     &
               aa(np4,np4),  &
               bb(np4,np4),  &
               vl(np4,np4),  &
               vr(np4,np4))

!
! Set up Gauss-Lobatto grid and compute Chebyshev derivative matrix.
      if (is .ne. -1) then

        if (ifdff.eq.0) then
          CALL grid(np  = np,  &
                    sig = sig, &
                    x   = y,   &
                    r   = r)

          CALL derivs(np  = np,  &
                      sig = sig, &
                      dl1 = dl1, &
                      ed2 = ed2, &
                      ed4 = ed4)
        else

          CALL fdgrid(np  = np,  &
                      sig = sig, &
                      x   = y,   &
                      r   = r)

          CALL fdrivs(np     = np,    &
                      sig    = sig,   &
                      dl1    = dl1,   &
                      iorder = ifdff, &
                      ed2    = ed2,   &
                      ed4    = ed4)

        endif
!
! Speed of sound and Mach number distributions and output.

!       CALL sndspd(np    = np,    &
!                   rr    = r,     &
!                   rmsw  = rmt,   &
!                   asnd  = snd,   &
!                   dsnd  = dsn,   &
!                   dd    = dl1,   &
!                   rhob  = rho,   &
!                   angom = angom, &
!                   gam   = gam,   &
!                   sig   = sig,   &
!                   is    = is)

!       CALL smach(npts  = np,    &
!                  rr    = r,     &
!                  rmsw  = rmt,   &
!                  rmswp = drt,   &
!                  snd   = snd,   &
!                  dsn   = dsn,   &
!                  dd    = dl1,   &
!                  angom = angom, &
!                  gam   = gam,   &
!                  rro   = sig,   &
!                  is    = is)

        CALL smachAndSndspd(npts  = np,    &
                            rr    = r,     &
                            rmsw  = rmt,   &
                            rmswp = drt,   &
                            snd   = snd,   &
                            dsn   = dsn,   &
                            dd    = dl1,   &
                            rhob  = rho,   &
                            angom = angom, &
                            gam   = gam,   &
                            sig   = sig,   &
                            is    = is)

        CALL rmach(npts  = np,    &
                   rr    = r,     &
                   rmch  = rmx,   &
                   drm   = drm,   &
                   snd   = snd,   &
                   dsn   = dsn,   &
                   dd    = dl1,   &
                   rxmax = rxmax, &
                   slope = slope, &
                   rro   = sig,   &
                   ir    = ir)

      else
        CALL interp(np    = np,    &
                    sig   = sig,   &
                    rr    = r,     &
                    rmx   = rmx,   &
                    drm   = drm,   &
                    rmt   = rmt,   &
                    drt   = drt,   &
                    snd   = snd,   &
                    dsn   = dsn,   &
                    dd    = dl1,   &
                    ifdff = ifdff, &
                    ed2   = ed2,   &
                    ed4   = ed4)
      endif

! output the mean flow data.

      CALL machout(npts  = np,  &
                   rr    = r,   &
                   rmch  = rmx, &
                   rmchp = drm, &
                   rmsw  = rmt, &
                   rmswp = drt, &
                   snd   = snd, &
                   dsn   = dsn, &
                   rhob  = rho)
!
! Set up global matrices.

      CALL globalM(np   = np,  &
                   np4  = np4, &
                   sig  = sig, &
                   mode = mm,  &
                   om   = ak,  &
                   snd  = snd, &
                   dd   = dl1, &
                   rr   = r,   &
                   rx   = rmx, &
                   dr   = drm, &
                   rt   = rmt, &
                   dt   = drt, &
                   aa   = aa,  &
                   bb   = bb)

      CALL boundary(np   = np,   &
                    sig  = sig,  &
                    ak   = ak,   &
                    etah = etah, &
                    etad = etad, &
                    rmx  = rmx,  &
                    rmt  = rmt,  &
                    dd   = dl1,  &
                    aa   = aa,   &
                    bb   = bb)

      CALL analysis(np    = np,    &
                    np4   = np4,   &
                    ak    = ak,    &
                    rr    = r,     &
                    snd   = snd,   &
                    rmx   = rmx,   &
                    rmt   = rmt,   &
                    aa    = aa,    &
                    bb    = bb,    &
                    alpha = alpha, &
                    beta  = beta,  &
                    vl    = vl,    &
                    vr    = vr,    &
                    work  = work,  &
                    rwork = rwork, &
                    gam   = wvn,   &
                    jobvl = jobvl, &
                    jobvr = jobvr, &
                    mm    = mm,    &
                    ir    = ir,    &
                    is    = is,    &
                    slp   = slope, &
                    vphi  = vph,   &
                    akap  = akap)

      CALL output(np     = np,    &
                  np4    = np4,   &
                  mode   = mm,    &
                  rho    = sig,   &
                  omega  = ak,    &
                  rmax   = rxmax, &
                  slp    = slope, &
                  ang    = angom, &
                  gam    = gam,   &
                  egv    = jobvr, &
                  attenh = etah,  &
                  attend = etad,  &
                  rmx    = rmx,   &
                  drm    = drm,   &
                  rmt    = rmt,   &
                  drt    = drt,   &
                  snd    = snd,   &
                  rr     = r,     &
                  wvn    = wvn,   &
                  vrm    = vr,    &
                  vphi   = vph,   &
                  is     = is,    &
                  icomp  = icomp)
!
      if (irepeat.eq.1) goto 100
!
!
! NEW: deallocate data arrays
!

      DEALLOCATE(dl1,   &
                 y,     &
                 rwork, &
                 r,     &
                 rmx,   &
                 drm,   &
                 rmt,   &
                 drt,   &
                 snd,   &
                 dsn,   &
                 rho,   &
                 akap,  &
                 alpha, &
                 beta,  &
                 work,  &
                 vph,   &
                 wvn,   &
                 aa,    &
                 bb,    &
                 vl,    &
                 vr)

!
      stop
      end
