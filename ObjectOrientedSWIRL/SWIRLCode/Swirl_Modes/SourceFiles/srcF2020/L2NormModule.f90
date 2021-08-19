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
        MODULE PROCEDURE L2N_One_Dataset
        MODULE PROCEDURE L2N_One_Dataset_Complex
        MODULE PROCEDURE L2N_COMPLEX
        MODULE PROCEDURE L2N_2D
    END INTERFACE

    INTERFACE getLMax
        MODULE PROCEDURE LMax
    END INTERFACE getLMax

    INTEGER,PARAMETER :: rDef = REAL64
    INTEGER :: i
!,numPoints

CONTAINS


    SUBROUTINE L2N(L2,&
        dataSet1,&
        dataSet2)
!lenDataSet = SIZE(dataSet)


! INTEGER, INTENT(INOUT) :: numPoints
        REAL(KIND=rDef), INTENT(INOUT) :: L2
        REAL(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet1,&
            dataSet2

!Local variables within submodule only
        INTEGER :: numPoints
        REAL(KIND=rDef) :: dataSum
        REAL(KIND=rDef), DIMENSION(:),ALLOCATABLE :: dataError,&
            dataErrorSquared


        numPoints = SIZE(dataSet1)
        ALLOCATE(dataError(numPoints), &
            dataErrorSquared(numPoints))

        dataSum = 0.0_rDef

        DO i = 1,numPoints
            dataError(i) = ABS(dataSet1(i) - dataSet2(i))
            dataErrorSquared(i) = dataError(i)**2
            dataSum = dataSum + dataErrorSquared(i)
        ENDDO

        L2 = SQRT(dataSum/REAL(numPoints,rDef))

        DEALLOCATE(dataError,dataErrorSquared)

!WRITE(6,*) L2
    END SUBROUTINE L2N

    SUBROUTINE L2N_One_Dataset(L2,&
        dataSet)
!lenDataSet = SIZE(dataSet)


! INTEGER, INTENT(INOUT) :: numPoints
        REAL(KIND=rDef), INTENT(INOUT) :: &
            L2
        REAL(KIND=rDef),DIMENSION(:), INTENT(IN)  :: &
            dataSet

!Local variables within submodule only
        INTEGER :: &
            numPoints

        REAL(KIND=rDef) :: &
            dataSum

        REAL(KIND=rDef), DIMENSION(:),ALLOCATABLE :: &
            dataError,&
            dataErrorSquared

        numPoints = SIZE(dataSet)

        ALLOCATE(&
            dataError(numPoints), &
            dataErrorSquared(numPoints))

        dataSum = 0.0_rDef

        DO i = 1,numPoints
            dataError(i) = ABS(dataSet(i) )
            dataErrorSquared(i) = dataError(i)**2
            dataSum = dataSum + dataErrorSquared(i)
        ENDDO

        L2 = SQRT(dataSum/REAL(numPoints,rDef))

        DEALLOCATE(dataError,dataErrorSquared)

!WRITE(6,*) L2
    END SUBROUTINE L2N_One_Dataset


    SUBROUTINE L2N_One_Dataset_Complex(L2,&
        dataSet)
!lenDataSet = SIZE(dataSet)


! INTEGER, INTENT(INOUT) :: numPoints
        COMPLEX(KIND=rDef), INTENT(INOUT) :: &
            L2

        COMPLEX(KIND=rDef),DIMENSION(:), INTENT(IN)  :: &
            dataSet

!Local variables within submodule only
        INTEGER :: &
            numPoints

        COMPLEX(KIND=rDef) :: &
            dataSum

        COMPLEX(KIND=rDef), DIMENSION(:),ALLOCATABLE :: &
            dataError,&
            dataErrorSquared

        numPoints = SIZE(dataSet)

        ALLOCATE(&
            dataError(numPoints), &
            dataErrorSquared(numPoints))

        dataSum = CMPLX(0.0_rDef,0.0_rDef,rDef)

        DO i = 1,numPoints
            dataError(i) = (CMPLX(dataSet(i) , KIND=rDef))
            dataErrorSquared(i) = dataError(i)**2
            dataSum = dataSum + dataErrorSquared(i)
        ENDDO

        L2 = SQRT(dataSum/CMPLX(numPoints,KIND=rDef))

        DEALLOCATE(dataError,dataErrorSquared)

!WRITE(6,*) L2
    END SUBROUTINE L2N_One_Dataset_Complex
    SUBROUTINE L2N_COMPLEX(L2,&
        dataSet1,&
        dataSet2)!,& numPoints)


        COMPLEX(KIND=rDef), INTENT(INOUT) :: L2
        COMPLEX(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet1,&
            dataSet2
!Local variables within submodule only

        INTEGER  :: numPoints
        COMPLEX(KIND=rDef) :: dataSum
        COMPLEX(KIND=rDef), DIMENSION(:), ALLOCATABLE :: dataError,&
            dataErrorSquared
        numPoints = SIZE(dataSet1)

        ALLOCATE(&
            dataError(numPoints) , &
            dataErrorSquared(numPoints))

        dataSum = CMPLX(0.0,0.0,rDef)

        DO i = 1,numPoints
            dataError(i) = CMPLX(ABS(dataSet1(i) - dataSet2(i)),KIND=rDef)
            dataErrorSquared(i) = dataError(i)**2
            dataSum = dataSum + dataErrorSquared(i)
        ENDDO

        L2 = SQRT(dataSum/CMPLX(numPoints,KIND=rDef))

        DEALLOCATE(dataError,dataErrorSquared)

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
        L2 = SQRT(CMPLX(dataSum,KIND=rDef) &
            /CMPLX(numPoints,KIND=rDef))
        ! WRITE(6,*) L2
    END SUBROUTINE L2N_2D
    SUBROUTINE LMax( &
        LM , &
        dataSet)

        REAL(KIND=rDef), INTENT(INOUT) :: LM
        REAL(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet

        LM = MAXVAL(dataSet)

    END SUBROUTINE LMax
END MODULE L2NormModule
