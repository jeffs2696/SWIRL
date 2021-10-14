PROGRAM GetModeData

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE

  INTEGER, PARAMETER :: outputMode =  40, &
                        nTime      =  51, &
                        rDef       = REAL64

  REAL(KIND=rDef), PARAMETER :: gam = 1.4_rDef, &
                                amp = 1.0e-5_rDef

  CHARACTER(LEN=40) :: outFile

  INTEGER :: np,  &
            np4,  &
           mode,  &
            i,j,  &
             nr,  &
             nt,  &
             npTest

  REAL(KIND=rDef), DIMENSION(nTime) :: time

  REAL(KIND=rDef) :: sigma,   &
                     rMax,    &
                     slp,     &
                     ang,     &
                     gamSWIRL,     &
                     gustVel, &
                     time0,   &
                     theta0,  &
                     period,  &
                     pi,      &
                     dt,      &
                     norm,    &
                     area,    &
                     normFac, &
                     dr,      &
                     rho,     &
                     p,       &
                     rDum

  REAL(KIND=rDef) :: rhoRefSWIRL,lRefSWIRL,aRefSWIRL,pRefSWIRL, &
                     rhoRefBASS, lRefBASS, aRefBASS, pRefBASS


  REAL(KIND=rDef), DIMENSION(:), ALLOCATABLE :: r, &
                                                a

  COMPLEX(KIND=rDef) :: omega, &
                       attenh, &
                       attend, &
                          fac, &
                           ai

  COMPLEX(KIND=rDef), DIMENSION(:), ALLOCATABLE :: wavenumber
  COMPLEX(KIND=rDef), DIMENSION(:,:), ALLOCATABLE :: eigenVector
  COMPLEX(KIND=rDef), DIMENSION(:), ALLOCATABLE :: rhoP,vXP,vRP,vThP,pP
  COMPLEX(KIND=rDef), DIMENSION(:,:), ALLOCATABLE :: rhoPG,vXPG,vRPG,vThPG,pPG

  CHARACTER(LEN=*), PARAMETER :: baseFileName = 'eigenVector'

  CHARACTER(LEN=20) :: fileName

  OPEN(12,FILE='output.data',FORM='UNFORMATTED')

  READ(12) np,np4,mode,sigma,omega,rMax,slp,ang,gamSWIRL,attenh,attend

  WRITE(6,*) 'np     = ',np
  WRITE(6,*) 'np4    = ',np4
  WRITE(6,*) 'mode   = ',mode
  WRITE(6,*) 'sigma  = ',sigma
  WRITE(6,*) 'omega  = ',omega
  WRITE(6,*) 'rMax   = ',rMax
  WRITE(6,*) 'slp    = ',slp
  WRITE(6,*) 'ang    = ',ang
  WRITE(6,*) 'gamSWIRL    = ',gamSWIRL
  WRITE(6,*) 'attenh = ',attenh
  WRITE(6,*) 'attend = ',attend

  ALLOCATE(wavenumber(np4),  &
                     r(np),  &
                     a(np),  &
      eigenvector(np4,np4),  &
                  rhoP(np),  &
                   vXP(np),  &
                   vRP(np),  &
                  vThP(np),  &
                    pP(np),  &
           rhoPG(np,nTime),  &
            vXPG(np,nTime),  &
            vRPG(np,nTime),  &
           vThPG(np,nTime),  &
             pPG(np,nTime))

  READ(12) (wavenumber(i),i=1,np4)

  READ(12) (r(i),i=1,np)

  OPEN(15,FILE='wavenumber.dat',FORM = 'FORMATTED')
  OPEN(25,FILE='wavenumberOverOmega.dat',FORM = 'FORMATTED')
 
  DO i=1,np4
!  WRITE(15,*) i,REAL(wavenumber(i)), AIMAG(wavenumber(i))
   WRITE(15,*) REAL(wavenumber(i)), AIMAG(wavenumber(i))
   WRITE(25,*) REAL(wavenumber(i)/omega), AIMAG(wavenumber(i)/omega)
  END DO
 
  CLOSE(15)
  CLOSE(25)

  READ(12) ((eigenVector(i,j),i=1,np4),j=1,np4)

  DO j=1,np4
   WRITE(fileName,'(a,i4.4,a)') baseFileName,j,'.dat'
   OPEN(21,FILE=fileName,FORM='FORMATTED')

   DO i=1,np
    WRITE(21,*) r(i),REAL(eigenVector(i,j)),      & ! vR
                    AIMAG(eigenVector(i,j)),      &
                     REAL(eigenVector(np+i,j)),   & ! vTh
                    AIMAG(eigenVector(np+i,j)),   &
                     REAL(eigenVector(2*np+i,j)), & ! vX
                    AIMAG(eigenVector(2*np+i,j)), &
                     REAL(eigenVector(3*np+i,j)), & ! p
                    AIMAG(eigenVector(3*np+i,j))
   END DO
   
   CLOSE(21)
  END DO

  CLOSE(12)

! read in the speed of sound (from machOut.dat)

  OPEN(12,FILE='machOut.dat',FORM='FORMATTED')
  READ(12,*) npTest

  IF (npTest /= np) THEN
   WRITE(6,*) 'ERROR: machOut has a different number of radial points.'
   STOP
  ELSE
   CONTINUE
  END IF

  DO i=1,np
   READ(12,*) rDum, & ! r(i)
              rDum, & ! mX(i)
              rDum, & ! mTh(i)
              rho,  & ! rho(i)
              p,    & ! p(i)
              a(i) 
  END DO  

  CLOSE(12)

! SWIRL nondimensionalization

  lRefSWIRL   = r(np)
  rhoRefSWIRL = rho
  aRefSWIRL   = a(np)
  pRefSWIRL   = p

! BASS nondimensionalization

  lRefBASS   = r(np)
  rhoRefBASS = rho
  aRefBASS   = a(np)
  pRefBASS   = gam*p

! now write out the desired gust.

  gustVel = omega/REAL(mode,rDef)
  time0  = 0.0_rDef
  theta0 = 0.0_rDef

  pi = 4.0_rDef*ATAN(1.0_rDef)
  ai = CMPLX(0.0_rDef,1.0_rDef,rDef)

  WRITE(outfile,'(a,i3.3,a)') 'SwirlingGustMode',outputMode,'.dat'

  OPEN(16,FILE = TRIM(outfile),FORM = 'FORMATTED')

! write out the file header

! start with the upper input keyword data

   WRITE(16,*) ' InputGustData    SWIRL code verification test'
   WRITE(16,*) ' InputGustDataFileVersion  1'

! header data

   WRITE(16,*) ' InputGustHeaderData '
   WRITE(16,*) ' NumberOfGusts             1 '
   WRITE(16,*) ' IncludeSyntheticTurb      No '
   WRITE(16,*) ' ThirdOrderInterpolation   Yes '
   WRITE(16,*) ' UseVorticalGustBC         No '
   WRITE(16,*) ' end InputGustHeaderData '

! data for deterministic gust

   WRITE(16,*) ' GustData  1'
   WRITE(16,*) ' GustDataFileVersion  1'

! gust header data

   WRITE(16,*) ' GustHeaderData '
   WRITE(16,*) '  PolarCoordinateGust   Yes'
   WRITE(16,*) '  AcousticGust          Yes'
   WRITE(16,*) '  PeriodicGust          Yes'
   IF (mode == 0) THEN
    WRITE(16,*) '  UseGustVelocity       No'
   ELSE
    WRITE(16,*) '  UseGustVelocity       Yes'
   END IF
   WRITE(16,*) '  NumberOfTimeEntries  ',nTime
   WRITE(16,*) '  NumberOfRadialPoints ',np
   WRITE(16,*) '  NumberOfRealizations  1 '
   WRITE(16,*) '  omega                ',REAL(omega,rDef)
   IF (mode == 0) THEN
    CONTINUE
   ELSE
    WRITE(16,*) '  gustVelocity         ',gustVel
   END IF
   WRITE(16,*) '  time0                ',time0
   WRITE(16,*) '  theta0               ',theta0
   WRITE(16,*) '  thetaAveraging         No'
   WRITE(16,*) ' end GustHeaderData'

! space point locations

   WRITE(16,*) ' SpacePointLocations '
   WRITE(16,*) (r(nr),nr = 1,np)
   WRITE(16,*) ' end SpacePointLocations '

   WRITE(6,*) 'm = ',mode,'sigma = ', sigma

   period = 2.0_rDef*pi/omega
   dt = period/REAL(nTime-1,rDef)

   DO nt = 1,nTime
    time(nt) = time0 + REAL(nt-1,rDef)*dt
   END DO

   WRITE(16,*) ' TimeLevels'
   WRITE(16,*) (time(nt),nt=1,nTime)
   WRITE(16,*) ' end TimeLevels'

! assemble the gust data

   DO nr = 1,np
    vRP(nr)  = eigenVector(nr,outputMode)
    vThP(nr) = eigenVector(np+nr,outputMode)
    vXP(nr)  = eigenVector((2*np)+nr,outputMode)
    pP(nr)   = eigenVector((3*np)+nr,outputMode)
    rhoP(nr) = a(nr)*a(nr)*pP(nr)
   END DO

! nondimensionalize properly

   DO nr = 1,np
    vRP(nr)  = vRP(nr)*(aRefSWIRL/aRefBASS)
    vThP(nr) = vThP(nr)*(aRefSWIRL/aRefBASS)
    vXP(nr)  = vXP(nr)*(aRefSWIRL/aRefBASS)
    pP(nr)   = pP(nr)*(pRefSWIRL/pRefBASS)
    rhoP(nr) = rhoP(nr)*(rhoRefSWIRL/rhoRefBASS)
   END DO

! normalize the pressure component of the gust.

! integrate using Trapezoidal rule

   norm = 0.5_rDef*((pP(1)*CONJG(pP(1)))*r(1))
   DO nr = 2,np-1
    norm = norm + (pP(nr)*CONJG(pP(nr)))*r(nr)
   END DO
   norm = norm + 0.5_rDef*(pP(np)*CONJG(pP(np)))*r(np)

   dr = (r(np)-r(1))/REAL(np-1,rDef)

   norm = norm*dr

   area = 0.5_rDef*(r(np)**2 - r(1)**2)

   normFac = 1.0_rDef/SQRT(norm/area)

   WRITE(6,*) outputMode,'norm = ',norm,' normFac = ',normFac

   DO nr=1,np
    pP(nr)   = pP(nr)*normFac
    rhoP(nr) = rhoP(nr)*normFac
    vXP(nr)  = vXP(nr)*normFac
    vRP(nr)  = vRP(nr)*normFac
    vThP(nr) = vThP(nr)*normFac
   END DO

! sanity check

   norm = 0.5_rDef*((pP(1)*CONJG(pP(1)))*r(1))
   DO nr = 2,np-1
    norm = norm + (pP(nr)*CONJG(pP(nr)))*r(nr)
   END DO
   norm = norm + 0.5_rDef*(pP(np)*CONJG(pP(np)))*r(np)
   norm = norm*dr

   area = 0.5_rDef*(r(np)**2 - r(1)**2)

   normFac = 1.0_rDef/SQRT(norm/area)

   WRITE(6,*) outputMode,'norm = ',norm,' normFac = ',normFac

! output

   OPEN(28,FILE='gust.dat',FORM = 'FORMATTED')
   DO nr = 1,np
    WRITE(28,*) r(nr),                          &
                REAL(rhoP(nr)),AIMAG(rhoP(nr)), &
                REAL(vXP(nr)),AIMAG(vXP(nr)),   &
                REAL(vRP(nr)),AIMAG(vRP(nr)),   &
                REAL(vThP(nr)),AIMAG(vThP(nr)), &
                REAL(pP(nr)),AIMAG(pP(nr))
   END DO

   CLOSE(28)

   OPEN(28,FILE='gustPAmp.dat',FORM = 'FORMATTED')
   DO nr = 1,np
    WRITE(28,*) r(nr),                          &
                SQRT(REAL(pP(nr))*REAL(pP(nr))+AIMAG(pP(nr))*AIMAG(pP(nr)))
   END DO

   CLOSE(28)

! construct the gust data

   DO nt = 1,nTime
    fac = amp*EXP(-ai*omega*time(nt))
    DO nr = 1,np
     rhoPG(nr,nt) = fac*rhoP(nr)
     vXPG(nr,nt)  = fac*vXP(nr)
     vRPG(nr,nt)  = fac*vRP(nr)
     vThPG(nr,nt) = fac*vThP(nr)
     pPG(nr,nt)   = fac*pP(nr)
    END DO
!   WRITE(28,*) time(nt),REAL(pPG(np,nt)),AIMAG(pPG(np,nt))
   END DO

! and write it out

   WRITE(16,*) ' GustRealization 1'
   WRITE(16,*) ((REAL(rhoPG(nr,nt),rDef),nr=1,np),   &
                                        nt=1,nTime), &
               ((REAL(vXPG(nr,nt),rDef),nr=1,np),    &
                                        nt=1,nTime), &
               ((REAL(vRPG(nr,nt),rDef),nr=1,np),    &
                                        nt=1,nTime), &
               ((REAL(vThPG(nr,nt),rDef),nr=1,np),   &
                                        nt=1,nTime), &
               ((REAL(pPG(nr,nt),rDef),nr=1,np),     &
                                        nt=1,nTime)
   WRITE(16,*) ' end GustRealization 1'
   WRITE(16,*) ' end GustData  1'
   WRITE(16,*) ' end InputGustData'

  CLOSE(16)

  STOP

END PROGRAM GetModeData
