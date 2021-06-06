! MACHOUTMODULE.f90-Gathers the fluid properties used and exports to file
!
 
MODULE machoutModule
      USE, INTRINSIC:: ISO_FORTRAN_ENV
      IMPLICIT NONE
      PRIVATE
      PUBLIC:: machout

INTERFACE machout
      MODULE PROCEDURE machout1
END INTERFACE machout

      INTEGER, PARAMETER:: rDef = REAL64

CONTAINS

      subroutine machout1(npts, rr, rmch, rmchp, rmsw, rmswp, snd, dsn, rhob)

      INTEGER, INTENT(IN):: npts
      REAL(KIND = rDef), DIMENSION(:), INTENT(IN)  :: rr, &
                                                  rmch, &
                                                 rmchp, &
                                                  rmsw, &
                                                 rmswp, &
                                                   snd, &
                                                   dsn
                                           
      REAL(KIND = rDef), DIMENSION(:), INTENT(OUT)  :: rhob
!
! local variables
!
      INTEGER:: i
      REAL(KIND = rDef):: r, &
                        rm, &
                        rt, &
                      rtot, &
                      pbar
!
!
!  Output Mach number, shear, and swirl distributions.
      open(unit = 21,         &
           file='mach.dat', &
           status='unknown')
      rewind 21
      write(21, 5)
 5    format('#',5x, 'r',8x, 'M_x',5x, 'dM_x/dr',5x, 'M_th',3x, 'dM_th/dr', &
         4x, 'M_tot',7x, 'A',8x, 'dA/dr',5x, 'rhob',6x, 'pbar')

      open(unit = 22,            &
           file='machOut.dat', &
           status='unknown')
      rewind(22)

      WRITE(22, *) nPts

      do i = 1, npts
       r    = rr(i)
       rm   = rmch(i)
       rt   = rmsw(i)
       rtot = sqrt(rm*rm+rt*rt)
       rhob(i) = snd(i)**5
       pbar = snd(i)**7
       write(21, 10) r, rm, rmchp(i), rt, rmswp(i), rtot, snd(i), dsn(i), &
          rhob(i), pbar
       write(22, *) r, rm, rt, rhob(i), pbar, snd(i)
      enddo
 10   format(10e15.4)
      close(21)
      close(22)
!
      return
      end
END MODULE machoutModule
