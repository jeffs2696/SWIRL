PROGRAM MAIN

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE

  INTEGER, PARAMETER :: rDef = REAL64
  
!
! this routine generates the inputs that are identical
!  to the Ken Kousen test case (just to check!).
!

  INTEGER, PARAMETER :: nPts = 201
  REAL(KIND=rDef), PARAMETER :: radMin  = 0.5_rDef,  &
                                radMax  = 1.0_rDef,  &
                                rVelMax = 0.30_rDef, &
                                slope   = 0.0_rDef,  &
                                angom   = 0.50_rDef
  REAL(KIND=rDef), DIMENSION(nPts) :: r,     &
                                      snd,   &
                                      rmach, &
                                      smach, &
                                      rvel,  &
                                      svel

  REAL(KIND=rDef) :: dr, &
                    gam, &
                    gm1, &
                    sig

  INTEGER :: i

! generate the grid

  sig = radMin/radMax

  dr = (radMax-radMin)/REAL(nPts-1,rDef)

  DO i=1,nPts
   r(i) = (radMin + REAL(i-1,rDef)*dr)/radMax
  END DO

! solid-body swirl

  gam = 1.4_rDef
  gm1 = gam - 1.0_rDef

  DO i=1,nPts
   snd(i)  =  1.0_rDef -gm1/2.0_rDef*angom*angom*(1.0_rDef -r(i)*r(i))
   snd(i)  =  sqrt(snd(i))

   svel(i)   =  angom*r(i)
   smach(i)  =  svel(i)/snd(i)

  END DO

! linear shear in the axial flow

  DO i=1,nPts
   if (slope.ge.0.0_rDef) then
    rvel(i) = slope*(r(i) -1.0_rDef)+rVelMax
   else
    rvel(i) = slope*(r(i) -sig)     +rVelMax
   endif
  enddo

  do i = 1,npts
   rmach(i) = rvel(i)/snd(i)
  enddo

! write the data files

! axial mach number

  OPEN(22,FILE='mach.input',FORM='FORMATTED')
  WRITE(22,*) nPts
  DO i=1,nPts
   WRITE(22,*) r(i),rmach(i)
  END DO
 
  CLOSE(22)

! swirl mach number

  OPEN(24,FILE='swrl.input',FORM='FORMATTED')
  WRITE(24,*) nPts
  DO i=1,nPts
   WRITE(24,*) r(i),smach(i)
  END DO
 
  CLOSE(24)

  STOP
END PROGRAM MAIN
