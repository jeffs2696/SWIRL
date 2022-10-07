PROGRAM testzeroCrossingModule
    USE, INTRINSIC :: ISO_FORTRAN_ENV
   USE zeroCrossingModule
    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64


      INTEGER :: &
          UNIT, &
          i , &
          numberOfPoints 

      REAL(KIND = REAL64) :: &
          min_bound,&
          max_bound,&
          delta_domain

      REAL(KIND = REAL64), DIMENSION(:), ALLOCATABLE :: domain

      COMPLEX(KIND = REAL64), DIMENSION(:), ALLOCATABLE :: waveData

      min_bound = 0
      max_bound = 1 
      numberOfPoints  = 100
      delta_domain = (max_bound - min_bound)/REAL(numberOfPoints - 1, rDef)

      ALLOCATE(&
          domain(numberOfPoints), &
          waveData(numberOfPoints))

CONTINUE

      OPEN( NEWUNIT = UNIT, FILE = 'testZCM.dat')

      WRITE(UNIT,*) 'x', ' y' 

      DO i = 1,numberOfPoints

      domain(i) = (min_bound + REAL(i-1, rDef)*delta_domain)/max_bound

      waveData(i) = SIN(5*domain(i))

      WRITE(UNIT,*) domain(i), REAL(waveData(i))

      ENDDO 
      CLOSE(UNIT)

      CALL zeroCrossing(&
          domain = domain, &
          dataSet = waveData)

END PROGRAM testzeroCrossingModule

