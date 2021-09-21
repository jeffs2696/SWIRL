PROGRAM GetMeanData

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE Akima1D
  IMPLICIT NONE

  INTEGER :: npBASS = 11

  INTEGER :: np, &
              i

  REAL(KIND=REAL64), PARAMETER :: gam = 1.4_REAL64 

  REAL(KIND=REAL64) :: rhoRefSWIRL,lRefSWIRL,aRefSWIRL,pRefSWIRL, &
                       rhoRefBASS, lRefBASS, aRefBASS, pRefBASS,  &
                       dr

  REAL(KIND=REAL64), DIMENSION(:), ALLOCATABLE :: rIn, &
                                                 mXIn, &
                                                mThIn, &
                                                rhoIn, &
                                                  pIn, &
                                                  aIn, &
                                              rhoVXIn, &
                                             rhoVThIn, &
                                              rhoVRIn, &
                                               eTotIn, &
                                                 rOut, &
                                               rhoOut, &
                                             rhoVXOut, &
                                            rhoVThOut, &
                                             rhoVROut, &
                                              eTotOut

  OPEN(12,FILE='machOut.dat',FORM='FORMATTED')

  READ(12,*) np

  ALLOCATE(rIn(np),     &
          mXIn(np),     &
         mThIn(np),     &
         rhoIn(np),     &
           pIn(np),     &
           aIn(np),     &
       rhoVXIn(np),     &
      rhoVThIn(np),     &
       rhoVRIn(np),     &
        eTotIn(np),     &
          rOut(npBASS), &
        rhoOut(npBASS), &
      rhoVXOut(npBASS), &
     rhoVThOut(npBASS), &
      rhoVROut(npBASS), &
       eTotOut(npBASS))

  DO i=1,np
   READ(12,*) rIn(i),mXIn(i),mThIn(i),rhoIn(i),pIn(i),aIn(i)
  END DO

  CLOSE(12)

! SWIRL nondimensionalization

  lRefSWIRL   = rIn(np)
  rhoRefSWIRL = rhoIn(np)
  aRefSWIRL   = aIn(np)
  pRefSWIRL   = pIn(np)

! BASS nondimensionalization

  lRefBASS   = rIn(np)
  rhoRefBASS = rhoIn(np)
  aRefBASS   = aIn(np)
  pRefBASS   = gam*pIn(np)

  DO i=1,np
   rhoVXIn(i)  = (rhoIn(i)*mXIn(i))*(rhoRefSWIRL*aRefSWIRL)/(rhoRefBASS*aRefBASS)
   rhoVThIn(i) = (rhoIn(i)*mThIn(i))*(rhoRefSWIRL*aRefSWIRL)/(rhoRefBASS*aRefBASS)
   rhoVRIn(i)  = 0.0_REAL64
   rIn(i)      = rIn(i)*(lRefSWIRL/lRefBASS)
   rhoIn(i)    = rhoIn(i)*(rhoRefSWIRL/rhoRefBASS)
   pIn(i)      = pIn(i)*(pRefSWIRL/pRefBASS)
   eTotIn(i)   = (pIn(i)/(gam-1.0_REAL64)) + 0.5_REAL64*((rhoVXIn(i)*rhoVXIn(i)   &
                                                         +rhoVThIn(i)*rhoVThIn(i) &
                                                         +rhoVRIn(i)*rhoVRIn(i))/rhoIn(i))
  END DO

! use Akima spline to generate BASS data

  dr = (rIn(np)-rIn(1))/REAL(npBASS-1,REAL64)

  DO i=1,npBASS
   rOut(i) = rIn(1) + dr*REAL(i-1,REAL64)
  END DO

! and spline it

! rho

  CALL Akima433Interpolation(inputDataLength  = np,     &
                             xInputData       = rIn,    &
                             yInputData       = rhoIn,  &
                             outputDataLength = npBASS, &
                             xOutputData      = rOut,   &
                             yOutputData      = rhoOut)

! rhoVX

  CALL Akima433Interpolation(inputDataLength  = np,      &
                             xInputData       = rIn,     &
                             yInputData       = rhoVXIn, &
                             outputDataLength = npBASS,  &
                             xOutputData      = rOut,    &
                             yOutputData      = rhoVXOut)

! rhoVR

  CALL Akima433Interpolation(inputDataLength  = np,      &
                             xInputData       = rIn,     &
                             yInputData       = rhoVRIn, &
                             outputDataLength = npBASS,  &
                             xOutputData      = rOut,    &
                             yOutputData      = rhoVROut)

! rhoVTh

  CALL Akima433Interpolation(inputDataLength  = np,       &
                             xInputData       = rIn,      &
                             yInputData       = rhoVThIn, &
                             outputDataLength = npBASS,   &
                             xOutputData      = rOut,     &
                             yOutputData      = rhoVThOut)

! eTot

  CALL Akima433Interpolation(inputDataLength  = np,       &
                             xInputData       = rIn,      &
                             yInputData       = eTotIn,   &
                             outputDataLength = npBASS,   &
                             xOutputData      = rOut,     &
                             yOutputData      = eTotOut)


! write out the file

  OPEN(13,FILE = 'meanBASS.dat',FORM = 'FORMATTED')
  WRITE(13,*) npBASS
  WRITE(13,*) (rOut(i),      i=1,npBASS), &
              (rhoOut(i),    i=1,npBASS), &
              (rhoVXOut(i),  i=1,npBASS), &
              (rhoVROut(i),  i=1,npBASS), &
              (rhoVThOut(i), i=1,npBASS), &
              (eTotOut(i),   i=1,npBASS)


  CLOSE(13)

  STOP

END PROGRAM GetMeanData
