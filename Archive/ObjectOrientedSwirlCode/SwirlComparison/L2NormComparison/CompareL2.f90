PROGRAM MAIN
USE, INTRINSIC :: ISO_FORTRAN_ENV
USE L2NormModule 
IMPLICIT NONE

 INTEGER, PARAMETER :: rDef = REAL64

!REAL(KIND=rDef),DIMENSION(:),ALLOCATABLE :: xOld,xNew,xErr
!REAL(KIND=rDef),DIMENSION(:,:),ALLOCATABLE :: twoDimX 

REAL(KIND=rDef),DIMENSION(:),ALLOCATABLE :: realFreqObj,imagFreqObj,&
                                            realFreqF08,imagFreqF08 

!REAL(KIND=rDef) :: L2,&
!                   L2twoDim

COMPLEX(KIND=rDef) ::  L2Freq  ,MaxFreqSqSum,MinFreqSqSum,FreqSqSum
                                       
COMPLEX(KIND=rDef),DIMENSION(:),ALLOCATABLE :: FreqObj, FreqF08, FreqErr,FreqErrSquared


INTEGER :: i,j,numPoints

CHARACTER(17) :: fileName1 = 'gammasOnlyObj.dat'
CHARACTER(19) :: fileName2 = 'gammasOnlyOrig.dat'


! Allocate Data

!ALLOCATE( xOld(5), xNew(5) , xErr(5) )
!ALLOCATE( twoDimX(5,5) )

numPoints = 64 
ALLOCATE(realFreqObj(numPoints),&
         imagFreqObj(numPoints),&
         realFreqF08(numPoints),&
         imagFreqF08(numPoints),&
         FreqObj(numPoints),&
       FreqF08(numPoints) ,&
        FreqErr(numPoints),&
        FreqErrSquared(numPoints))
! Fill in arrays

! 1D Test
!DO i=1,5
! xOld(i) = i
! xNew(i) = i*3
! xErr(i) = xOld(i) - xNew(i)
!ENDDO
!! 2D Test
!DO i = 1,5
! DO j = 1,5
!  twoDimX(i,j) = i
! ENDDO
!ENDDO
!
!CALL getL2Norm(L2 = L2  ,&
!               dataSet =xErr)




!CALL getL2Norm(L2      =L2twoDim  ,&
!               dataSet =twoDimX)

! Swirl Comparison

OPEN(UNIT = 11, FILE = fileName1)! ,FORMAT = 'FORMATTED')
OPEN(UNIT = 12, FILE = fileName2)! ,FORMAT = 'FORMATTED')


DO i = 1,numPoints
READ(11,*) realFreqObj(i),imagFreqObj(i)
READ(12,*) realFreqF08(i),imagFreqF08(i)


FreqErr(i) = ABS(realFreqObj(i) - realFreqF08(i))
FreqErrSquared(i) = FreqErr(i)**2
FreqSqSum = FreqSqSum + FreqErrSquared(i)


ENDDO

CLOSE(11)
CLOSE(12)

!L2Freq = SQRT(FreqSqSum/numPoints )
!
CALL getL2Norm(L2 = L2Freq,&
               dataSet1 = realFreqObj,&
               dataSet2 = realFreqF08 ,&
               numPoints = numPoints)



WRITE(6,*) 'L2:', L2Freq
END PROGRAM MAIN
