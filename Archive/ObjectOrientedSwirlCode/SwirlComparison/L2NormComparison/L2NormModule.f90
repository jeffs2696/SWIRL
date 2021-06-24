MODULE L2NormModule

USE, INTRINSIC :: ISO_FORTRAN_ENV
IMPLICIT NONE
PRIVATE
PUBLIC :: getL2Norm 

INTERFACE getL2Norm
   MODULE PROCEDURE L2N
END INTERFACE

INTEGER,PARAMETER :: rDef = REAL64
INTEGER :: i
!,numPoints

CONTAINS

SUBROUTINE L2N(L2,&
              dataSet1,&
              dataSet2,&
              numPoints)
!lenDataSet = SIZE(dataSet)


INTEGER, INTENT(INOUT) :: numPoints
COMPLEX(KIND=rDef), INTENT(INOUT) :: L2
REAL(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet1,&
                                             dataSet2

!Local variables within submodule only

REAL(KIND=rDef) :: dataSum
REAL(KIND=rDef), DIMENSION(numPoints) :: dataError,&
                                dataErrorSquared                 
 

dataSum = 0.0_rDef
DO i = 1,numPoints
dataError(i) = ABS(dataSet1(i) - dataSet2(i)) 
dataErrorSquared(i) = dataError(i)**2
dataSum = dataSum + dataErrorSquared(i)
ENDDO
L2 = SQRT(dataSum/numPoints)
END SUBROUTINE L2N
END MODULE L2NormModule
