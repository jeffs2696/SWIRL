MODULE mmsClassObject

    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: getRateOfConvergence, getL2Norm, mmsClassType


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

    INTERFACE getRateOfConvergence
        MODULE PROCEDURE getROC
        MODULE PROCEDURE getROC_Complex
    END INTERFACE getRateOfConvergence
!    INTERFACE getLMax
!        MODULE PROCEDURE LMax
!    END INTERFACE getLMax

    INTEGER,PARAMETER :: rDef = REAL64
    INTEGER :: i

    TYPE mmsClassType

        PRIVATE 

        LOGICAL :: &
            isInitialized = .FALSE.        ! flag to identify if object exists

        REAL(KIND=rDef) ::& 
            L2
        REAL(KIND=rDef),DIMENSION(:) , ALLOCATABLE :: &
            dataSet , &
            dataSet1,&
            dataSet2,&
            RateOfConvergence, L2Array

        REAL(KIND=rDef),DIMENSION(:,:) , ALLOCATABLE :: &
            TwoDdataSet1 , &
            TwoDdataSet2

    END TYPE mmsClassType
CONTAINS 


    SUBROUTINE L2N(&
        object    ,&
        L2        ,&
        dataSet1  ,&
        dataSet2)

        TYPE(mmsClassType) , INTENT(INOUT) :: &
            object

        REAL(KIND=rDef), INTENT(INOUT) :: L2

        REAL(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet1,&
            dataSet2

! INTEGER, INTENT(INOUT) :: numPoints
!Local variables within submodule only

        INTEGER :: numPoints

        REAL(KIND=rDef) :: dataSum

        REAL(KIND=rDef), DIMENSION(:),ALLOCATABLE :: &
            dataError,&
            dataErrorSquared


        ! Sending INTENT(IN)'s to Derived Data TYPE . . .
        object%dataSet1 = dataSet1
        object%dataSet2 = dataSet2
        numPoints = SIZE(object%dataSet1)

        ALLOCATE(dataError(numPoints), &
            dataErrorSquared(numPoints))

        dataSum = 0.0_rDef

        DO i = 1,numPoints
            dataError(i)        = ABS(object%dataSet1(i) - object%dataSet2(i))
            dataErrorSquared(i) = dataError(i)**2
            dataSum = dataSum + dataErrorSquared(i)
        ENDDO

        object%L2 = SQRT(dataSum/REAL(numPoints,rDef))
        L2 = object%L2


        DEALLOCATE(dataError,dataErrorSquared)

!WRITE(6,*) L2
    END SUBROUTINE L2N

    SUBROUTINE L2N_One_Dataset( &
        object ,&
        L2,&
        dataSet)

        TYPE(mmsClassType) , INTENT(INOUT) :: &
            object

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

        object%dataSet = dataSet

        numPoints = SIZE(object%dataSet)

        ALLOCATE(&
            dataError(numPoints), &
            dataErrorSquared(numPoints))

        dataSum = 0.0_rDef

        DO i = 1,numPoints
            dataError(i) = ABS(object%dataSet(i) )
            dataErrorSquared(i) = dataError(i)**2
            dataSum = dataSum + dataErrorSquared(i)
        ENDDO

        object%L2 = SQRT(dataSum/REAL(numPoints,rDef))


        L2 = object%L2
       
        DEALLOCATE(dataError,dataErrorSquared)

!WRITE(6,*) L2
    END SUBROUTINE L2N_One_Dataset


    SUBROUTINE L2N_One_Dataset_Complex(&
        object ,&
        L2,&
        dataSet)
!lenDataSet = SIZE(dataSet)

        TYPE(mmsClassType) , INTENT(INOUT) :: &
            object

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


        object%dataSet = dataSet

        numPoints = SIZE(object%dataSet)

        ALLOCATE(&
            dataError(numPoints), &
            dataErrorSquared(numPoints))

        dataSum = CMPLX(0.0_rDef,0.0_rDef,rDef)

        DO i = 1,numPoints
            dataError(i) = (CMPLX(object%dataSet(i) , KIND=rDef))
            dataErrorSquared(i) = dataError(i)**2
            dataSum = dataSum + dataErrorSquared(i)
        ENDDO


        object%L2 = SQRT(dataSum/CMPLX(numPoints,KIND=rDef))
        L2 = object%L2 

        DEALLOCATE(dataError,dataErrorSquared)

!WRITE(6,*) L2
    END SUBROUTINE L2N_One_Dataset_Complex
    SUBROUTINE L2N_COMPLEX( &
        object   ,&
        L2,&
        dataSet1,&
        dataSet2)!,& numPoints)

        TYPE(mmsClassType) , INTENT(INOUT) :: &
            object

        INTEGER :: &
            numPoints
        COMPLEX(KIND=rDef), INTENT(INOUT) :: L2
        COMPLEX(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet1,&
            dataSet2
!Local variables within submodule only

        COMPLEX(KIND=rDef) :: dataSum
        COMPLEX(KIND=rDef), DIMENSION(:), ALLOCATABLE :: dataError,&
            dataErrorSquared

        object%dataSet1 = dataSet1
        object%dataSet2 = dataSet2

        numPoints = SIZE(object%dataSet1)

        ALLOCATE(&
            dataError(numPoints) , &
            dataErrorSquared(numPoints))

        dataSum = CMPLX(0.0,0.0,rDef)

        DO i = 1,numPoints
            dataError(i) = CMPLX(ABS(object%dataSet1(i) - object%dataSet2(i)),KIND=rDef)
            dataErrorSquared(i) = dataError(i)**2
            dataSum = dataSum + dataErrorSquared(i)
        ENDDO

        L2 = SQRT(dataSum/CMPLX(numPoints,KIND=rDef))

        object%L2 =L2 
        DEALLOCATE(dataError,dataErrorSquared)

    END SUBROUTINE L2N_COMPLEX

    SUBROUTINE L2N_2D( &
        object ,&
        L2    ,&
        dataSet1,&
        dataSet2,&
        numPoints)
! This will calculare the L2 Norm of a 2D array that
! has (numPoints,numPoints) dimension

        TYPE(mmsClassType) , INTENT(INOUT) :: &
            object

        INTEGER :: j ! Index for the second dimension

        INTEGER, INTENT(INOUT) :: numPoints

        COMPLEX(KIND=rDef), INTENT(INOUT) :: &
            L2

        COMPLEX(KIND=rDef),DIMENSION(:,:), INTENT(IN)  :: &
            dataSet1,&
            dataSet2

!Local variables within submodule only


        REAL(KIND=rDef) :: dataSum
        REAL(KIND=rDef), DIMENSION(numPoints,numPoints) :: dataError,&
            dataErrorSquared

        object%TwoDdataSet1 = dataSet1
        object%TwoDdataSet2 = dataSet2

        dataSum = 0.0_rDef
        DO i = 1,numPoints
            DO j =1,numPoints
                dataError(i,j) = ABS(object%TwoDdataSet1(i,j) - object%TwoDdataSet2(i,j))
                dataErrorSquared(i,j) = dataError(i,j)**2
                dataSum = dataSum + dataErrorSquared(i,j)
            ENDDO
        ENDDO
        L2 = SQRT(CMPLX(dataSum,KIND=rDef) &
            /CMPLX(numPoints,KIND=rDef))
        object%L2 = L2

        ! WRITE(6,*) L2
    END SUBROUTINE L2N_2D
    SUBROUTINE LMax( &
        object , &
        LM , &
        dataSet)

        TYPE(mmsClassType) , INTENT(INOUT) :: &
            object

        REAL(KIND=rDef), INTENT(INOUT) :: LM
        REAL(KIND=rDef),DIMENSION(:), INTENT(IN)  :: dataSet

        object%dataSet = dataSet
        LM = MAXVAL(dataSet)


    END SUBROUTINE LMax
    SUBROUTINE getROC(&
        object           ,&
        RateOfConvergence,&
        L2Array          )

        TYPE(mmsClassType) , INTENT(INOUT) :: &
            object

        REAL(KIND = rDef), DIMENSION(:), INTENT(OUT) :: &
            RateOfConvergence

        REAL(KIND = rDef), DIMENSION(:), INTENT(IN) :: &
            L2Array

        INTEGER ::  numberOfIterations, i

        numberOfIterations = SIZE(RateOfConvergence)

        WRITE(6,*)  SIZE(RateOfConvergence)

        object%L2Array = L2Array
        object%RateOfConvergence = RateOfConvergence

        DO i = 1,numberOfIterations 
            IF (REAL(L2Array(i),KIND=rDef)< 1e-13) THEN 
            ! WRITE(6,*) L2Array(i)
                WRITE(6,*) 'WARNING: The L2 at',i,'=' ,L2Array(i) ,'is really small, double check if trivial'
                EXIT
            ELSE
            ENDIF
             object%RateOfConvergence(i) = &
                (&
                LOG((object%L2Array(i+1))) -&
                LOG((object%L2Array(i  )))&
                )&
                /&
                LOG(0.50_rDef) ! change 0.5 so that way the grid spacing doesnt have to half as big between iterations JS

        ENDDO
        RateOfConvergence = object%RateOfConvergence 

END SUBROUTINE getROC 

SUBROUTINE getROC_Complex(&
    object           ,&
    RateOfConvergence,&
    L2Array          )

    TYPE(mmsClassType) , INTENT(INOUT) :: &
        object

    REAL(KIND = rDef), DIMENSION(:), INTENT(OUT) :: &
        RateOfConvergence

    COMPLEX(KIND = rDef), DIMENSION(:), INTENT(IN) :: &
        L2Array

        INTEGER ::  numberOfIterations, i

        numberOfIterations = SIZE(RateOfConvergence)

        WRITE(6,*)  SIZE(RateOfConvergence)

        object%L2Array = L2Array
        object%RateOfConvergence = RateOfConvergence

        DO i = 1,numberOfIterations 
            IF (REAL(L2Array(i),KIND=rDef) < 1e-13) THEN
                WRITE(6,*) 'WARNING: The L2 at',i,'=' ,L2Array(i) ,'is really small, double check if trivial'
                
                EXIT
            ELSE
            ENDIF
             object%RateOfConvergence(i) = &
                (&
                 LOG((object%L2Array(i+1))) -&
                 LOG((object%L2Array(i  )))&
                )&
                /&
                LOG(0.50_rDef) ! change 0.5 so that way the grid spacing doesnt have to half as big between iterations JS

        ENDDO
        RateOfConvergence = object%RateOfConvergence 

    END SUBROUTINE getROC_Complex

END MODULE mmsClassObject
