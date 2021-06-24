! L2NormModule.f90 - Calculates the L2_Norm =
!                       sqrt( sum (dataError)/numberofPoints)
! 
MODULE L2NormModule

USE, INTRINSIC :: ISO_FORTRAN_ENV
IMPLICIT NONE
PRIVATE
PUBLIC :: getL2Norm 

INTERFACE getL2Norm
   MODULE PROCEDURE L2N
   MODULE PROCEDURE L2N_COMPLEX
   MODULE PROCEDURE L2N_2D
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
REAL(KIND=rDef), INTENT(INOUT) :: L2
REAL(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet1,&
                                             dataSet2

!Local variables within submodule only

REAL(KIND=rDef) :: dataSum
REAL(KIND=rDef), DIMENSION(:),ALLOCATABLE :: dataError,&
                                dataErrorSquared                 
                            
ALLOCATE(dataError(numPoints), &
         dataErrorSquared(numPoints))

dataSum = 0.0_rDef

DO i = 1,numPoints
dataError(i) = ABS(dataSet1(i) - dataSet2(i)) 
dataErrorSquared(i) = dataError(i)**2
dataSum = dataSum + dataErrorSquared(i)
ENDDO

L2 = SQRT(dataSum/numPoints)

DEALLOCATE(dataError,dataErrorSquared)

!WRITE(6,*) L2
END SUBROUTINE L2N
SUBROUTINE L2N_COMPLEX(L2,&
              dataSet1,&
              dataSet2,&
              numPoints)
!lenDataSet = SIZE(dataSet)


INTEGER, INTENT(INOUT) :: numPoints
COMPLEX(KIND=rDef), INTENT(INOUT) :: L2
COMPLEX(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet1,&
                                             dataSet2

!Local variables within submodule only

COMPLEX(KIND=rDef) :: dataSum
COMPLEX(KIND=rDef), DIMENSION(numPoints) :: dataError,&
                                dataErrorSquared                 
 

dataSum = 0.0_rDef
DO i = 1,numPoints
dataError(i) = ABS(dataSet1(i) - dataSet2(i)) 
dataErrorSquared(i) = dataError(i)**2
dataSum = dataSum + dataErrorSquared(i)
ENDDO
L2 = SQRT(dataSum/numPoints)
END SUBROUTINE L2N_COMPLEX
SUBROUTINE L2N_2D(L2    ,&
                  dataSet1,&
                  dataSet2,&
                  numPoints)
! This will calculare the L2 Norm of a 2D array that
! has (numPoints,numPoints) dimension

INTEGER :: j ! Index for the second dimension
INTEGER, INTENT(INOUT) :: numPoints
COMPLEX(KIND=rDef), INTENT(INOUT) :: L2
COMPLEX(KIND=rDef),DIMENSION(:,:), INTENT(IN)  :: dataSet1,&
                                             dataSet2

!Local variables within submodule only

REAL(KIND=rDef) :: dataSum
REAL(KIND=rDef), DIMENSION(numPoints,numPoints) :: dataError,&
                                dataErrorSquared                 
 

dataSum = 0.0_rDef
DO i = 1,numPoints
DO j =1,numPoints
dataError(i,j) = ABS(dataSet1(i,j) - dataSet2(i,j)) 
dataErrorSquared(i,j) = dataError(i,j)**2
dataSum = dataSum + dataErrorSquared(i,j)
ENDDO
ENDDO
L2 = SQRT(dataSum/numPoints)
WRITE(6,*) L2
END SUBROUTINE L2N_2D
END MODULE L2NormModule
