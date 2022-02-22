MODULE DataVolume5DClassCreateAndDestroyTest

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE MPI2DataClassNoErrClass
  USE ErrorInformationClass
  USE DataVolume5DClass
  USE Check1DData
  USE Check2DData
  USE Check3DData
  USE Check4DData
  USE Check5DData

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: CreateAndDestroyTest

  TYPE(CharacterStringType) :: charStringObject

  CHARACTER(LEN=*), PARAMETER :: moduleLocation = &
    ' in MODULE DataVolume5DClassCreateAndDestroyTest: '

INTEGER, PARAMETER :: int4Def  = INT32,  &
                      int8Def  = INT64,  &
                      real4Def = REAL32, &
                      real8Def = REAL64

INTERFACE CreateAndDestroyTest
  MODULE PROCEDURE CreateAndDestroyDataVolume5DTest
END INTERFACE CreateAndDestroyTest

CONTAINS

SUBROUTINE CreateAndDestroyDataVolume5DTest(mpiData,        &
                                            verbose,        &
                                            enableChecking, &
                                            passedTest,     &
                                            errorInfoObject)

  TYPE(mpiDataObjectType), INTENT(INOUT) :: mpiData

  LOGICAL, INTENT(IN) :: verbose, &
                         enableChecking
  LOGICAL, INTENT(INOUT) :: passedTest

  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  LOGICAL :: masterNode = .TRUE.

  LOGICAL :: errorFound,              &
             errorExpected

  LOGICAL :: correctValue

  LOGICAL :: foundPointer, &
             pointerIsExpected

! dataVolume5DClass objects

  TYPE(dataVolume5DType) :: int4DataVolume5DObject, &
                            int8DataVolume5DObject, &
                           real4DataVolume5DObject, &
                           real8DataVolume5DObject, &
                         logicalDataVolume5DObject

  INTEGER(KIND=int4Def), PARAMETER :: int4Lo = -2101_int4Def, &
                                      int4Hi = 38109_int4Def

  INTEGER(KIND=int8Def), PARAMETER :: int8Lo = -129131_int8Def, &
                                      int8Hi =  13093145_int8Def

  REAL(KIND=real4Def), PARAMETER :: real4Lo = -445931.0_real4Def, &
                                    real4Hi =  348631.0_real4Def

  REAL(KIND=real8Def), PARAMETER :: real8Lo = -78434168.0_real8Def, &
                                    real8Hi =  84901325.0_real8Def

! several types of data

! 5D

  INTEGER, DIMENSION(5,2) :: int4BufferVolumeDataTotalBounds5D,  &
                             int8BufferVolumeDataTotalBounds5D,  &
                             real4BufferVolumeDataTotalBounds5D, &
                             real8BufferVolumeDataTotalBounds5D, &
                           logicalBufferVolumeDataTotalBounds5D

  INTEGER, DIMENSION(5,2) :: int4BufferVolumeDataUpdateBounds5D,  &
                             int8BufferVolumeDataUpdateBounds5D,  &
                             real4BufferVolumeDataUpdateBounds5D, &
                             real8BufferVolumeDataUpdateBounds5D, &
                           logicalBufferVolumeDataUpdateBounds5D

  INTEGER, DIMENSION(5,2) :: int4PointerVolumeDataTotalBounds5D,  &
                             int8PointerVolumeDataTotalBounds5D,  &
                             real4PointerVolumeDataTotalBounds5D, &
                             real8PointerVolumeDataTotalBounds5D, &
                           logicalPointerVolumeDataTotalBounds5D

  INTEGER, DIMENSION(5,2) :: int4PointerVolumeDataUpdateBounds5D,  &
                             int8PointerVolumeDataUpdateBounds5D,  &
                             real4PointerVolumeDataUpdateBounds5D, &
                             real8PointerVolumeDataUpdateBounds5D, &
                           logicalPointerVolumeDataUpdateBounds5D

  INTEGER(KIND=int4Def), DIMENSION(1:3,2:4,4:6,6:8,3:5) :: int4Data5D
  INTEGER(KIND=int8Def), DIMENSION(-1:15,2:31,1:11,12:48,-1:4) :: int8Data5D
  REAL(KIND=real4Def), DIMENSION(1:10,5:17,-3:13,3:31,5:11) :: real4Data5D
  REAL(KIND=real8Def), DIMENSION(3:7,51:72,4:5,-1:15,-3:1) :: real8Data5D
  LOGICAL, DIMENSION(8:10,-53:-51,21:31,-2:8,1:6) :: logicalData5D

  INTEGER(KIND=int4Def), DIMENSION(:,:,:,:,:), POINTER :: int4Ptr5D => NULL()
  INTEGER(KIND=int8Def), DIMENSION(:,:,:,:,:), POINTER :: int8Ptr5D => NULL()
  REAL(KIND=real4Def), DIMENSION(:,:,:,:,:), POINTER :: real4Ptr5D => NULL()
  REAL(KIND=real8Def), DIMENSION(:,:,:,:,:), POINTER :: real8Ptr5D => NULL()
  LOGICAL, DIMENSION(:,:,:,:,:), POINTER :: logicalPtr5D => NULL()

  INTEGER, DIMENSION(5,2) :: int4BufferVolumeDataTotalBounds5DTest,  &
                             int8BufferVolumeDataTotalBounds5DTest,  &
                             real4BufferVolumeDataTotalBounds5DTest, &
                             real8BufferVolumeDataTotalBounds5DTest, &
                           logicalBufferVolumeDataTotalBounds5DTest

  INTEGER, DIMENSION(5,2) :: int4BufferVolumeDataUpdateBounds5DTest,  &
                             int8BufferVolumeDataUpdateBounds5DTest,  &
                             real4BufferVolumeDataUpdateBounds5DTest, &
                             real8BufferVolumeDataUpdateBounds5DTest, &
                           logicalBufferVolumeDataUpdateBounds5DTest

! 4D

  INTEGER, DIMENSION(4,2) :: int4BufferVolumeDataTotalBounds4D,  &
                             int8BufferVolumeDataTotalBounds4D,  &
                             real4BufferVolumeDataTotalBounds4D, &
                             real8BufferVolumeDataTotalBounds4D, &
                           logicalBufferVolumeDataTotalBounds4D

  INTEGER, DIMENSION(4,2) :: int4BufferVolumeDataUpdateBounds4D,  &
                             int8BufferVolumeDataUpdateBounds4D,  &
                             real4BufferVolumeDataUpdateBounds4D, &
                             real8BufferVolumeDataUpdateBounds4D, &
                           logicalBufferVolumeDataUpdateBounds4D

  INTEGER, DIMENSION(4,2) :: int4PointerVolumeDataTotalBounds4D,  &
                             int8PointerVolumeDataTotalBounds4D,  &
                             real4PointerVolumeDataTotalBounds4D, &
                             real8PointerVolumeDataTotalBounds4D, &
                           logicalPointerVolumeDataTotalBounds4D

  INTEGER, DIMENSION(4,2) :: int4PointerVolumeDataUpdateBounds4D,  &
                             int8PointerVolumeDataUpdateBounds4D,  &
                             real4PointerVolumeDataUpdateBounds4D, &
                             real8PointerVolumeDataUpdateBounds4D, &
                           logicalPointerVolumeDataUpdateBounds4D

  INTEGER(KIND=int4Def), DIMENSION(1:3,2:4,4:6,6:8) :: int4Data4D
  INTEGER(KIND=int8Def), DIMENSION(-1:15,2:31,1:11,12:48) :: int8Data4D
  REAL(KIND=real4Def), DIMENSION(1:10,5:17,-3:13,3:31) :: real4Data4D
  REAL(KIND=real8Def), DIMENSION(3:7,51:72,4:5,-1:15) :: real8Data4D
  LOGICAL, DIMENSION(8:10,-53:-51,21:31,-2:8) :: logicalData4D

  INTEGER(KIND=int4Def), DIMENSION(:,:,:,:), POINTER :: int4Ptr4D => NULL()
  INTEGER(KIND=int8Def), DIMENSION(:,:,:,:), POINTER :: int8Ptr4D => NULL()
  REAL(KIND=real4Def), DIMENSION(:,:,:,:), POINTER :: real4Ptr4D => NULL()
  REAL(KIND=real8Def), DIMENSION(:,:,:,:), POINTER :: real8Ptr4D => NULL()
  LOGICAL, DIMENSION(:,:,:,:), POINTER :: logicalPtr4D => NULL()

  INTEGER, DIMENSION(4,2) :: int4BufferVolumeDataTotalBounds4DTest,  &
                             int8BufferVolumeDataTotalBounds4DTest,  &
                             real4BufferVolumeDataTotalBounds4DTest, &
                             real8BufferVolumeDataTotalBounds4DTest, &
                           logicalBufferVolumeDataTotalBounds4DTest

  INTEGER, DIMENSION(4,2) :: int4BufferVolumeDataUpdateBounds4DTest,  &
                             int8BufferVolumeDataUpdateBounds4DTest,  &
                             real4BufferVolumeDataUpdateBounds4DTest, &
                             real8BufferVolumeDataUpdateBounds4DTest, &
                           logicalBufferVolumeDataUpdateBounds4DTest

! 3D

  INTEGER, DIMENSION(3,2) :: int4BufferVolumeDataTotalBounds3D,  &
                             int8BufferVolumeDataTotalBounds3D,  &
                             real4BufferVolumeDataTotalBounds3D, &
                             real8BufferVolumeDataTotalBounds3D, &
                           logicalBufferVolumeDataTotalBounds3D

  INTEGER, DIMENSION(3,2) :: int4BufferVolumeDataUpdateBounds3D,  &
                             int8BufferVolumeDataUpdateBounds3D,  &
                             real4BufferVolumeDataUpdateBounds3D, &
                             real8BufferVolumeDataUpdateBounds3D, &
                           logicalBufferVolumeDataUpdateBounds3D

  INTEGER, DIMENSION(3,2) :: int4PointerVolumeDataTotalBounds3D,  &
                             int8PointerVolumeDataTotalBounds3D,  &
                             real4PointerVolumeDataTotalBounds3D, &
                             real8PointerVolumeDataTotalBounds3D, &
                           logicalPointerVolumeDataTotalBounds3D

  INTEGER, DIMENSION(3,2) :: int4PointerVolumeDataUpdateBounds3D,  &
                             int8PointerVolumeDataUpdateBounds3D,  &
                             real4PointerVolumeDataUpdateBounds3D, &
                             real8PointerVolumeDataUpdateBounds3D, &
                           logicalPointerVolumeDataUpdateBounds3D

  INTEGER(KIND=int4Def), DIMENSION(2:4,4:6,6:8) :: int4Data3D
  INTEGER(KIND=int8Def), DIMENSION(2:31,1:11,12:48) :: int8Data3D
  REAL(KIND=real4Def), DIMENSION(5:17,-3:13,3:31) :: real4Data3D
  REAL(KIND=real8Def), DIMENSION(51:72,4:5,-1:15) :: real8Data3D
  LOGICAL, DIMENSION(-53:-51,21:31,-2:8) :: logicalData3D

  INTEGER(KIND=int4Def), DIMENSION(:,:,:), POINTER :: int4Ptr3D => NULL()
  INTEGER(KIND=int8Def), DIMENSION(:,:,:), POINTER :: int8Ptr3D => NULL()
  REAL(KIND=real4Def), DIMENSION(:,:,:), POINTER :: real4Ptr3D => NULL()
  REAL(KIND=real8Def), DIMENSION(:,:,:), POINTER :: real8Ptr3D => NULL()
  LOGICAL, DIMENSION(:,:,:), POINTER :: logicalPtr3D => NULL()

  INTEGER, DIMENSION(3,2) :: int4BufferVolumeDataTotalBounds3DTest,  &
                             int8BufferVolumeDataTotalBounds3DTest,  &
                             real4BufferVolumeDataTotalBounds3DTest, &
                             real8BufferVolumeDataTotalBounds3DTest, &
                           logicalBufferVolumeDataTotalBounds3DTest

  INTEGER, DIMENSION(3,2) :: int4BufferVolumeDataUpdateBounds3DTest,  &
                             int8BufferVolumeDataUpdateBounds3DTest,  &
                             real4BufferVolumeDataUpdateBounds3DTest, &
                             real8BufferVolumeDataUpdateBounds3DTest, &
                           logicalBufferVolumeDataUpdateBounds3DTest

! 2D

  INTEGER, DIMENSION(2,2) :: int4BufferVolumeDataTotalBounds2D,  &
                             int8BufferVolumeDataTotalBounds2D,  &
                             real4BufferVolumeDataTotalBounds2D, &
                             real8BufferVolumeDataTotalBounds2D, &
                           logicalBufferVolumeDataTotalBounds2D

  INTEGER, DIMENSION(2,2) :: int4BufferVolumeDataUpdateBounds2D,  &
                             int8BufferVolumeDataUpdateBounds2D,  &
                             real4BufferVolumeDataUpdateBounds2D, &
                             real8BufferVolumeDataUpdateBounds2D, &
                           logicalBufferVolumeDataUpdateBounds2D

  INTEGER, DIMENSION(2,2) :: int4PointerVolumeDataTotalBounds2D,  &
                             int8PointerVolumeDataTotalBounds2D,  &
                             real4PointerVolumeDataTotalBounds2D, &
                             real8PointerVolumeDataTotalBounds2D, &
                           logicalPointerVolumeDataTotalBounds2D

  INTEGER, DIMENSION(2,2) :: int4PointerVolumeDataUpdateBounds2D,  &
                             int8PointerVolumeDataUpdateBounds2D,  &
                             real4PointerVolumeDataUpdateBounds2D, &
                             real8PointerVolumeDataUpdateBounds2D, &
                           logicalPointerVolumeDataUpdateBounds2D

  INTEGER(KIND=int4Def), DIMENSION(4:6,6:8) :: int4Data2D
  INTEGER(KIND=int8Def), DIMENSION(1:11,12:48) :: int8Data2D
  REAL(KIND=real4Def), DIMENSION(-3:13,3:31) :: real4Data2D
  REAL(KIND=real8Def), DIMENSION(4:5,-1:15) :: real8Data2D
  LOGICAL, DIMENSION(21:31,-2:8) :: logicalData2D

  INTEGER(KIND=int4Def), DIMENSION(:,:), POINTER :: int4Ptr2D => NULL()
  INTEGER(KIND=int8Def), DIMENSION(:,:), POINTER :: int8Ptr2D => NULL()
  REAL(KIND=real4Def), DIMENSION(:,:), POINTER :: real4Ptr2D => NULL()
  REAL(KIND=real8Def), DIMENSION(:,:), POINTER :: real8Ptr2D => NULL()
  LOGICAL, DIMENSION(:,:), POINTER :: logicalPtr2D => NULL()

  INTEGER, DIMENSION(2,2) :: int4BufferVolumeDataTotalBounds2DTest,  &
                             int8BufferVolumeDataTotalBounds2DTest,  &
                             real4BufferVolumeDataTotalBounds2DTest, &
                             real8BufferVolumeDataTotalBounds2DTest, &
                           logicalBufferVolumeDataTotalBounds2DTest

  INTEGER, DIMENSION(2,2) :: int4BufferVolumeDataUpdateBounds2DTest,  &
                             int8BufferVolumeDataUpdateBounds2DTest,  &
                             real4BufferVolumeDataUpdateBounds2DTest, &
                             real8BufferVolumeDataUpdateBounds2DTest, &
                           logicalBufferVolumeDataUpdateBounds2DTest

! 1D

  INTEGER, DIMENSION(1,2) :: int4BufferVolumeDataTotalBounds1D,  &
                             int8BufferVolumeDataTotalBounds1D,  &
                             real4BufferVolumeDataTotalBounds1D, &
                             real8BufferVolumeDataTotalBounds1D, &
                           logicalBufferVolumeDataTotalBounds1D

  INTEGER, DIMENSION(1,2) :: int4BufferVolumeDataUpdateBounds1D,  &
                             int8BufferVolumeDataUpdateBounds1D,  &
                             real4BufferVolumeDataUpdateBounds1D, &
                             real8BufferVolumeDataUpdateBounds1D, &
                           logicalBufferVolumeDataUpdateBounds1D

  INTEGER, DIMENSION(1,2) :: int4PointerVolumeDataTotalBounds1D,  &
                             int8PointerVolumeDataTotalBounds1D,  &
                             real4PointerVolumeDataTotalBounds1D, &
                             real8PointerVolumeDataTotalBounds1D, &
                           logicalPointerVolumeDataTotalBounds1D

  INTEGER, DIMENSION(1,2) :: int4PointerVolumeDataUpdateBounds1D,  &
                             int8PointerVolumeDataUpdateBounds1D,  &
                             real4PointerVolumeDataUpdateBounds1D, &
                             real8PointerVolumeDataUpdateBounds1D, &
                           logicalPointerVolumeDataUpdateBounds1D

  INTEGER(KIND=int4Def), DIMENSION(6:8) :: int4Data1D
  INTEGER(KIND=int8Def), DIMENSION(12:48) :: int8Data1D
  REAL(KIND=real4Def), DIMENSION(3:31) :: real4Data1D
  REAL(KIND=real8Def), DIMENSION(-1:15) :: real8Data1D
  LOGICAL, DIMENSION(-2:8) :: logicalData1D

  INTEGER(KIND=int4Def), DIMENSION(:), POINTER :: int4Ptr1D => NULL()
  INTEGER(KIND=int8Def), DIMENSION(:), POINTER :: int8Ptr1D => NULL()
  REAL(KIND=real4Def), DIMENSION(:), POINTER :: real4Ptr1D => NULL()
  REAL(KIND=real8Def), DIMENSION(:), POINTER :: real8Ptr1D => NULL()
  LOGICAL, DIMENSION(:), POINTER :: logicalPtr1D => NULL()

  INTEGER, DIMENSION(1,2) :: int4BufferVolumeDataTotalBounds1DTest,  &
                             int8BufferVolumeDataTotalBounds1DTest,  &
                             real4BufferVolumeDataTotalBounds1DTest, &
                             real8BufferVolumeDataTotalBounds1DTest, &
                           logicalBufferVolumeDataTotalBounds1DTest

  INTEGER, DIMENSION(1,2) :: int4BufferVolumeDataUpdateBounds1DTest,  &
                             int8BufferVolumeDataUpdateBounds1DTest,  &
                             real4BufferVolumeDataUpdateBounds1DTest, &
                             real8BufferVolumeDataUpdateBounds1DTest, &
                           logicalBufferVolumeDataUpdateBounds1DTest

  REAL(KIND=real8Def) :: rDum, &
                         rFac

  INTEGER :: i,j,k,l,m
  
  INTEGER, DIMENSION(5) :: errorLocation

  CHARACTER(LEN=*), PARAMETER :: location = &
    'SUBROUTINE CreateAndDestroyDataVolume5DTest'

!------end of variable definition----------------------------

  CONTINUE ! execution begins here

  passedTest = .TRUE.
  errorExpected = .FALSE.

  errorFound = .FALSE.

  masterNode = IsMasterNode(object = mpiData)

  IF (masterNode) THEN
   WRITE(0,*) '--------------------------------'
   WRITE(0,*) 'Testing object create and destroy routines.'
   WRITE(0,*) '--------------------------------'
  END IF
!
!-------generate random data for the various arrays
!
! initialize

! 5D

  DO k=1,5
   int4BufferVolumeDataTotalBounds5D(k,1)     = 1
   int8BufferVolumeDataTotalBounds5D(k,1)     = 1
   real4BufferVolumeDataTotalBounds5D(k,1)    = 1
   real8BufferVolumeDataTotalBounds5D(k,1)    = 1
   logicalBufferVolumeDataTotalBounds5D(k,1)  = 1
   int4BufferVolumeDataTotalBounds5D(k,2)     = 0
   int8BufferVolumeDataTotalBounds5D(k,2)     = 0
   real4BufferVolumeDataTotalBounds5D(k,2)    = 0
   real8BufferVolumeDataTotalBounds5D(k,2)    = 0
   logicalBufferVolumeDataTotalBounds5D(k,2)  = 0
   int4BufferVolumeDataUpdateBounds5D(k,1)    = 1
   int8BufferVolumeDataUpdateBounds5D(k,1)    = 1
   real4BufferVolumeDataUpdateBounds5D(k,1)   = 1
   real8BufferVolumeDataUpdateBounds5D(k,1)   = 1
   logicalBufferVolumeDataUpdateBounds5D(k,1) = 1
   int4BufferVolumeDataUpdateBounds5D(k,2)    = 0
   int8BufferVolumeDataUpdateBounds5D(k,2)    = 0
   real4BufferVolumeDataUpdateBounds5D(k,2)   = 0
   real8BufferVolumeDataUpdateBounds5D(k,2)   = 0
   logicalBufferVolumeDataUpdateBounds5D(k,2) = 0
  END DO

  DO k=1,5
   int4BufferVolumeDataTotalBounds5D(k,1)     = LBOUND(int4Data5D,k)
   int4BufferVolumeDataTotalBounds5D(k,2)     = UBOUND(int4Data5D,k)
   int8BufferVolumeDataTotalBounds5D(k,1)     = LBOUND(int8Data5D,k)
   int8BufferVolumeDataTotalBounds5D(k,2)     = UBOUND(int8Data5D,k)
   real4BufferVolumeDataTotalBounds5D(k,1)    = LBOUND(real4Data5D,k)
   real4BufferVolumeDataTotalBounds5D(k,2)    = UBOUND(real4Data5D,k)
   real8BufferVolumeDataTotalBounds5D(k,1)    = LBOUND(real8Data5D,k)
   real8BufferVolumeDataTotalBounds5D(k,2)    = UBOUND(real8Data5D,k)
   logicalBufferVolumeDataTotalBounds5D(k,1)  = LBOUND(logicalData5D,k)
   logicalBufferVolumeDataTotalBounds5D(k,2)  = UBOUND(logicalData5D,k)
   int4BufferVolumeDataUpdateBounds5D(k,1)    = LBOUND(int4Data5D,k)+1
   int4BufferVolumeDataUpdateBounds5D(k,2)    = UBOUND(int4Data5D,k)-1
   int8BufferVolumeDataUpdateBounds5D(k,1)    = LBOUND(int8Data5D,k)+1
   int8BufferVolumeDataUpdateBounds5D(k,2)    = UBOUND(int8Data5D,k)-1
   real4BufferVolumeDataUpdateBounds5D(k,1)   = LBOUND(real4Data5D,k)+1
   real4BufferVolumeDataUpdateBounds5D(k,2)   = UBOUND(real4Data5D,k)-1
   real8BufferVolumeDataUpdateBounds5D(k,1)   = LBOUND(real8Data5D,k)+1
   real8BufferVolumeDataUpdateBounds5D(k,2)   = UBOUND(real8Data5D,k)-1
   logicalBufferVolumeDataUpdateBounds5D(k,1) = LBOUND(logicalData5D,k)+1
   logicalBufferVolumeDataUpdateBounds5D(k,2) = UBOUND(logicalData5D,k)-1
  END DO
  
! now fill in the data

! int4

  rFac = REAL((int4Hi+1-int4Lo),real8Def)

  DO m=int4BufferVolumeDataTotalBounds5D(5,1), &
        int4BufferVolumeDataTotalBounds5D(5,2)
   DO l=int4BufferVolumeDataTotalBounds5D(4,1), &
         int4BufferVolumeDataTotalBounds5D(4,2)
    DO k=int4BufferVolumeDataTotalBounds5D(3,1), &
          int4BufferVolumeDataTotalBounds5D(3,2)
     DO j=int4BufferVolumeDataTotalBounds5D(2,1), &
           int4BufferVolumeDataTotalBounds5D(2,2)
      DO i=int4BufferVolumeDataTotalBounds5D(1,1), &
            int4BufferVolumeDataTotalBounds5D(1,2)
       CALL RANDOM_NUMBER(rDum)
       int4Data5D(i,j,k,l,m) = int4Lo + FLOOR(rFac*rDum)
      END DO
     END DO
    END DO
   END DO
  END DO

!int8

  rFac = REAL((int8Hi+1-int8Lo),real8Def)

  DO m=int8BufferVolumeDataTotalBounds5D(5,1), &
       int8BufferVolumeDataTotalBounds5D(5,2)
   DO l=int8BufferVolumeDataTotalBounds5D(4,1), &
        int8BufferVolumeDataTotalBounds5D(4,2)
    DO k=int8BufferVolumeDataTotalBounds5D(3,1), &
          int8BufferVolumeDataTotalBounds5D(3,2)
     DO j=int8BufferVolumeDataTotalBounds5D(2,1), &
           int8BufferVolumeDataTotalBounds5D(2,2)
      DO i=int8BufferVolumeDataTotalBounds5D(1,1), &
            int8BufferVolumeDataTotalBounds5D(1,2)
       CALL RANDOM_NUMBER(rDum)
       int8Data5D(i,j,k,l,m) = int8Lo + FLOOR(rFac*rDum)
      END DO
     END DO
    END DO
   END DO
  END DO

!real4

  rFac = (real4Hi-real4Lo)

  DO m=real4BufferVolumeDataTotalBounds5D(5,1), &
        real4BufferVolumeDataTotalBounds5D(5,2)
   DO l=real4BufferVolumeDataTotalBounds5D(4,1), &
         real4BufferVolumeDataTotalBounds5D(4,2)
    DO k=real4BufferVolumeDataTotalBounds5D(3,1), &
          real4BufferVolumeDataTotalBounds5D(3,2)
     DO j=real4BufferVolumeDataTotalBounds5D(2,1), &
           real4BufferVolumeDataTotalBounds5D(2,2)
      DO i=real4BufferVolumeDataTotalBounds5D(1,1), &
            real4BufferVolumeDataTotalBounds5D(1,2)
       CALL RANDOM_NUMBER(rDum)
       real4Data5D(i,j,k,l,m) = real4Lo + REAL(rFac*rDum,real4Def)
      END DO
     END DO
    END DO
   END DO
  END DO

!real8

  rFac = (real8Hi-real8Lo)

  DO m=real8BufferVolumeDataTotalBounds5D(5,1), &
        real8BufferVolumeDataTotalBounds5D(5,2)
   DO l=real8BufferVolumeDataTotalBounds5D(4,1), &
         real8BufferVolumeDataTotalBounds5D(4,2)
    DO k=real8BufferVolumeDataTotalBounds5D(3,1), &
          real8BufferVolumeDataTotalBounds5D(3,2)
     DO j=real8BufferVolumeDataTotalBounds5D(2,1), &
           real8BufferVolumeDataTotalBounds5D(2,2)
      DO i=real8BufferVolumeDataTotalBounds5D(1,1), &
            real8BufferVolumeDataTotalBounds5D(1,2)
       CALL RANDOM_NUMBER(rDum)
       real8Data5D(i,j,k,l,m) = real8Lo + (rFac*rDum)
      END DO
     END DO
    END DO
   END DO
  END DO

!logical

  DO m=logicalBufferVolumeDataTotalBounds5D(5,1), &
        logicalBufferVolumeDataTotalBounds5D(5,2)
   DO l=logicalBufferVolumeDataTotalBounds5D(4,1), &
         logicalBufferVolumeDataTotalBounds5D(4,2)
    DO k=logicalBufferVolumeDataTotalBounds5D(3,1), &
          logicalBufferVolumeDataTotalBounds5D(3,2)
     DO j=logicalBufferVolumeDataTotalBounds5D(2,1), &
           logicalBufferVolumeDataTotalBounds5D(2,2)
      DO i=logicalBufferVolumeDataTotalBounds5D(1,1), &
            logicalBufferVolumeDataTotalBounds5D(1,2)
       CALL RANDOM_NUMBER(rDum)
       IF (rDum < 0.0_real8Def) THEN
        logicalData5D(i,j,k,l,m) = .FALSE.
       ELSE
        logicalData5D(i,j,k,l,m) = .TRUE.
       END IF
      END DO
     END DO
    END DO
   END DO
  END DO

! 4D

  DO k=1,4
   int4BufferVolumeDataTotalBounds4D(k,1)    = 1
   int8BufferVolumeDataTotalBounds4D(k,1)    = 1
   real4BufferVolumeDataTotalBounds4D(k,1)   = 1
   real8BufferVolumeDataTotalBounds4D(k,1)   = 1
   logicalBufferVolumeDataTotalBounds4D(k,1) = 1
   int4BufferVolumeDataTotalBounds4D(k,2)    = 0
   int8BufferVolumeDataTotalBounds4D(k,2)    = 0
   real4BufferVolumeDataTotalBounds4D(k,2)   = 0
   real8BufferVolumeDataTotalBounds4D(k,2)   = 0
   logicalBufferVolumeDataTotalBounds4D(k,2) = 0
   int4BufferVolumeDataUpdateBounds4D(k,1)    = 1
   int8BufferVolumeDataUpdateBounds4D(k,1)    = 1
   real4BufferVolumeDataUpdateBounds4D(k,1)   = 1
   real8BufferVolumeDataUpdateBounds4D(k,1)   = 1
   logicalBufferVolumeDataUpdateBounds4D(k,1) = 1
   int4BufferVolumeDataUpdateBounds4D(k,2)    = 0
   int8BufferVolumeDataUpdateBounds4D(k,2)    = 0
   real4BufferVolumeDataUpdateBounds4D(k,2)   = 0
   real8BufferVolumeDataUpdateBounds4D(k,2)   = 0
   logicalBufferVolumeDataUpdateBounds4D(k,2) = 0
  END DO

  DO k=1,4
   int4BufferVolumeDataTotalBounds4D(k,1)    = LBOUND(int4Data4D,k)
   int4BufferVolumeDataTotalBounds4D(k,2)    = UBOUND(int4Data4D,k)
   int8BufferVolumeDataTotalBounds4D(k,1)    = LBOUND(int8Data4D,k)
   int8BufferVolumeDataTotalBounds4D(k,2)    = UBOUND(int8Data4D,k)
   real4BufferVolumeDataTotalBounds4D(k,1)   = LBOUND(real4Data4D,k)
   real4BufferVolumeDataTotalBounds4D(k,2)   = UBOUND(real4Data4D,k)
   real8BufferVolumeDataTotalBounds4D(k,1)   = LBOUND(real8Data4D,k)
   real8BufferVolumeDataTotalBounds4D(k,2)   = UBOUND(real8Data4D,k)
   logicalBufferVolumeDataTotalBounds4D(k,1) = LBOUND(logicalData4D,k)
   logicalBufferVolumeDataTotalBounds4D(k,2) = UBOUND(logicalData4D,k)
   int4BufferVolumeDataUpdateBounds4D(k,1)    = LBOUND(int4Data4D,k)+1
   int4BufferVolumeDataUpdateBounds4D(k,2)    = UBOUND(int4Data4D,k)-1
   int8BufferVolumeDataUpdateBounds4D(k,1)    = LBOUND(int8Data4D,k)+1
   int8BufferVolumeDataUpdateBounds4D(k,2)    = UBOUND(int8Data4D,k)-1
   real4BufferVolumeDataUpdateBounds4D(k,1)   = LBOUND(real4Data4D,k)+1
   real4BufferVolumeDataUpdateBounds4D(k,2)   = UBOUND(real4Data4D,k)-1
   real8BufferVolumeDataUpdateBounds4D(k,1)   = LBOUND(real8Data4D,k)+1
   real8BufferVolumeDataUpdateBounds4D(k,2)   = UBOUND(real8Data4D,k)-1
   logicalBufferVolumeDataUpdateBounds4D(k,1) = LBOUND(logicalData4D,k)+1
   logicalBufferVolumeDataUpdateBounds4D(k,2) = UBOUND(logicalData4D,k)-1
  END DO
  
! now fill in the data

! int4

  rFac = REAL((int4Hi+1-int4Lo),real8Def)

  DO l=int4BufferVolumeDataTotalBounds4D(4,1), &
        int4BufferVolumeDataTotalBounds4D(4,2)
   DO k=int4BufferVolumeDataTotalBounds4D(3,1), &
         int4BufferVolumeDataTotalBounds4D(3,2)
    DO j=int4BufferVolumeDataTotalBounds4D(2,1), &
          int4BufferVolumeDataTotalBounds4D(2,2)
     DO i=int4BufferVolumeDataTotalBounds4D(1,1), &
           int4BufferVolumeDataTotalBounds4D(1,2)
      CALL RANDOM_NUMBER(rDum)
      int4Data4D(i,j,k,l) = int4Lo + FLOOR(rFac*rDum)
     END DO
    END DO
   END DO
  END DO

!int8

  rFac = REAL((int8Hi+1-int8Lo),real8Def)

  DO l=int8BufferVolumeDataTotalBounds4D(4,1), &
        int8BufferVolumeDataTotalBounds4D(4,2)
   DO k=int8BufferVolumeDataTotalBounds4D(3,1), &
         int8BufferVolumeDataTotalBounds4D(3,2)
    DO j=int8BufferVolumeDataTotalBounds4D(2,1), &
          int8BufferVolumeDataTotalBounds4D(2,2)
     DO i=int8BufferVolumeDataTotalBounds4D(1,1), &
           int8BufferVolumeDataTotalBounds4D(1,2)
      CALL RANDOM_NUMBER(rDum)
      int8Data4D(i,j,k,l) = int8Lo + FLOOR(rFac*rDum)
     END DO
    END DO
   END DO
  END DO

!real4

  rFac = (real4Hi-real4Lo)

  DO l=real4BufferVolumeDataTotalBounds4D(4,1), &
        real4BufferVolumeDataTotalBounds4D(4,2)
   DO k=real4BufferVolumeDataTotalBounds4D(3,1), &
         real4BufferVolumeDataTotalBounds4D(3,2)
    DO j=real4BufferVolumeDataTotalBounds4D(2,1), &
          real4BufferVolumeDataTotalBounds4D(2,2)
     DO i=real4BufferVolumeDataTotalBounds4D(1,1), &
           real4BufferVolumeDataTotalBounds4D(1,2)
      CALL RANDOM_NUMBER(rDum)
      real4Data4D(i,j,k,l) = real4Lo + REAL(rFac*rDum,real4Def)
     END DO
    END DO
   END DO
  END DO

!real8

  rFac = (real8Hi-real8Lo)

  DO l=real8BufferVolumeDataTotalBounds4D(4,1), &
        real8BufferVolumeDataTotalBounds4D(4,2)
   DO k=real8BufferVolumeDataTotalBounds4D(3,1), &
         real8BufferVolumeDataTotalBounds4D(3,2)
    DO j=real8BufferVolumeDataTotalBounds4D(2,1), &
          real8BufferVolumeDataTotalBounds4D(2,2)
     DO i=real8BufferVolumeDataTotalBounds4D(1,1), &
           real8BufferVolumeDataTotalBounds4D(1,2)
      CALL RANDOM_NUMBER(rDum)
      real8Data4D(i,j,k,l) = real8Lo + (rFac*rDum)
     END DO
    END DO
   END DO
  END DO

!logical

  DO l=logicalBufferVolumeDataTotalBounds4D(4,1), &
        logicalBufferVolumeDataTotalBounds4D(4,2)
   DO k=logicalBufferVolumeDataTotalBounds4D(3,1), &
         logicalBufferVolumeDataTotalBounds4D(3,2)
    DO j=logicalBufferVolumeDataTotalBounds4D(2,1), &
          logicalBufferVolumeDataTotalBounds4D(2,2)
     DO i=logicalBufferVolumeDataTotalBounds4D(1,1), &
           logicalBufferVolumeDataTotalBounds4D(1,2)
      CALL RANDOM_NUMBER(rDum)
      IF (rDum < 0.0_real8Def) THEN
       logicalData4D(i,j,k,l) = .FALSE.
      ELSE
       logicalData4D(i,j,k,l) = .TRUE.
      END IF
     END DO
    END DO
   END DO
  END DO

! 3D

  DO k=1,3
   int4BufferVolumeDataTotalBounds3D(k,1)     = 1
   int8BufferVolumeDataTotalBounds3D(k,1)     = 1
   real4BufferVolumeDataTotalBounds3D(k,1)    = 1
   real8BufferVolumeDataTotalBounds3D(k,1)    = 1
   logicalBufferVolumeDataTotalBounds3D(k,1)  = 1
   int4BufferVolumeDataTotalBounds3D(k,2)     = 0
   int8BufferVolumeDataTotalBounds3D(k,2)     = 0
   real4BufferVolumeDataTotalBounds3D(k,2)    = 0
   real8BufferVolumeDataTotalBounds3D(k,2)    = 0
   logicalBufferVolumeDataTotalBounds3D(k,2)  = 0
   int4BufferVolumeDataUpdateBounds3D(k,1)    = 1
   int8BufferVolumeDataUpdateBounds3D(k,1)    = 1
   real4BufferVolumeDataUpdateBounds3D(k,1)   = 1
   real8BufferVolumeDataUpdateBounds3D(k,1)   = 1
   logicalBufferVolumeDataUpdateBounds3D(k,1) = 1
   int4BufferVolumeDataUpdateBounds3D(k,2)    = 0
   int8BufferVolumeDataUpdateBounds3D(k,2)    = 0
   real4BufferVolumeDataUpdateBounds3D(k,2)   = 0
   real8BufferVolumeDataUpdateBounds3D(k,2)   = 0
   logicalBufferVolumeDataUpdateBounds3D(k,2) = 0
  END DO

  DO k=1,3
   int4BufferVolumeDataTotalBounds3D(k,1)     = LBOUND(int4Data3D,k)
   int4BufferVolumeDataTotalBounds3D(k,2)     = UBOUND(int4Data3D,k)
   int8BufferVolumeDataTotalBounds3D(k,1)     = LBOUND(int8Data3D,k)
   int8BufferVolumeDataTotalBounds3D(k,2)     = UBOUND(int8Data3D,k)
   real4BufferVolumeDataTotalBounds3D(k,1)    = LBOUND(real4Data3D,k)
   real4BufferVolumeDataTotalBounds3D(k,2)    = UBOUND(real4Data3D,k)
   real8BufferVolumeDataTotalBounds3D(k,1)    = LBOUND(real8Data3D,k)
   real8BufferVolumeDataTotalBounds3D(k,2)    = UBOUND(real8Data3D,k)
   logicalBufferVolumeDataTotalBounds3D(k,1)  = LBOUND(logicalData3D,k)
   logicalBufferVolumeDataTotalBounds3D(k,2)  = UBOUND(logicalData3D,k)
   int4BufferVolumeDataUpdateBounds3D(k,1)    = LBOUND(int4Data3D,k)+1
   int4BufferVolumeDataUpdateBounds3D(k,2)    = UBOUND(int4Data3D,k)-1
   int8BufferVolumeDataUpdateBounds3D(k,1)    = LBOUND(int8Data3D,k)+1
   int8BufferVolumeDataUpdateBounds3D(k,2)    = UBOUND(int8Data3D,k)-1
   real4BufferVolumeDataUpdateBounds3D(k,1)   = LBOUND(real4Data3D,k)+1
   real4BufferVolumeDataUpdateBounds3D(k,2)   = UBOUND(real4Data3D,k)-1
   real8BufferVolumeDataUpdateBounds3D(k,1)   = LBOUND(real8Data3D,k)+1
   real8BufferVolumeDataUpdateBounds3D(k,2)   = UBOUND(real8Data3D,k)-1
   logicalBufferVolumeDataUpdateBounds3D(k,1) = LBOUND(logicalData3D,k)+1
   logicalBufferVolumeDataUpdateBounds3D(k,2) = UBOUND(logicalData3D,k)-1
  END DO
  
! now fill in the data

! int4

  rFac = REAL((int4Hi+1-int4Lo),real8Def)

   DO k=int4BufferVolumeDataTotalBounds3D(3,1), &
         int4BufferVolumeDataTotalBounds3D(3,2)
    DO j=int4BufferVolumeDataTotalBounds3D(2,1), &
          int4BufferVolumeDataTotalBounds3D(2,2)
     DO i=int4BufferVolumeDataTotalBounds3D(1,1), &
           int4BufferVolumeDataTotalBounds3D(1,2)
      CALL RANDOM_NUMBER(rDum)
      int4Data3D(i,j,k) = int4Lo + FLOOR(rFac*rDum)
     END DO
    END DO
   END DO

!int8

  rFac = REAL((int8Hi+1-int8Lo),real8Def)

   DO k=int8BufferVolumeDataTotalBounds3D(3,1), &
         int8BufferVolumeDataTotalBounds3D(3,2)
    DO j=int8BufferVolumeDataTotalBounds3D(2,1), &
          int8BufferVolumeDataTotalBounds3D(2,2)
     DO i=int8BufferVolumeDataTotalBounds3D(1,1), &
           int8BufferVolumeDataTotalBounds3D(1,2)
      CALL RANDOM_NUMBER(rDum)
      int8Data3D(i,j,k) = int8Lo + FLOOR(rFac*rDum)
     END DO
    END DO
   END DO

!real4

  rFac = (real4Hi-real4Lo)

   DO k=real4BufferVolumeDataTotalBounds3D(3,1), &
         real4BufferVolumeDataTotalBounds3D(3,2)
    DO j=real4BufferVolumeDataTotalBounds3D(2,1), &
          real4BufferVolumeDataTotalBounds3D(2,2)
     DO i=real4BufferVolumeDataTotalBounds3D(1,1), &
           real4BufferVolumeDataTotalBounds3D(1,2)
      CALL RANDOM_NUMBER(rDum)
      real4Data3D(i,j,k) = real4Lo + REAL(rFac*rDum,real4Def)
     END DO
    END DO
   END DO

!real8

  rFac = (real8Hi-real8Lo)

   DO k=real8BufferVolumeDataTotalBounds3D(3,1), &
         real8BufferVolumeDataTotalBounds3D(3,2)
    DO j=real8BufferVolumeDataTotalBounds3D(2,1), &
          real8BufferVolumeDataTotalBounds3D(2,2)
     DO i=real8BufferVolumeDataTotalBounds3D(1,1), &
           real8BufferVolumeDataTotalBounds3D(1,2)
      CALL RANDOM_NUMBER(rDum)
      real8Data3D(i,j,k) = real8Lo + (rFac*rDum)
     END DO
    END DO
   END DO

!logical

   DO k=logicalBufferVolumeDataTotalBounds3D(3,1), &
         logicalBufferVolumeDataTotalBounds3D(3,2)
    DO j=logicalBufferVolumeDataTotalBounds3D(2,1), &
          logicalBufferVolumeDataTotalBounds3D(2,2)
     DO i=logicalBufferVolumeDataTotalBounds3D(1,1), &
           logicalBufferVolumeDataTotalBounds3D(1,2)
      CALL RANDOM_NUMBER(rDum)
      IF (rDum < 0.0_real8Def) THEN
       logicalData3D(i,j,k) = .FALSE.
      ELSE
       logicalData3D(i,j,k) = .TRUE.
      END IF
     END DO
    END DO
   END DO

! 2D

  DO k=1,2
   int4BufferVolumeDataTotalBounds2D(k,1)     = 1
   int8BufferVolumeDataTotalBounds2D(k,1)     = 1
   real4BufferVolumeDataTotalBounds2D(k,1)    = 1
   real8BufferVolumeDataTotalBounds2D(k,1)    = 1
   logicalBufferVolumeDataTotalBounds2D(k,1)  = 1
   int4BufferVolumeDataTotalBounds2D(k,2)     = 0
   int8BufferVolumeDataTotalBounds2D(k,2)     = 0
   real4BufferVolumeDataTotalBounds2D(k,2)    = 0
   real8BufferVolumeDataTotalBounds2D(k,2)    = 0
   logicalBufferVolumeDataTotalBounds2D(k,2)  = 0
   int4BufferVolumeDataUpdateBounds2D(k,1)    = 1
   int8BufferVolumeDataUpdateBounds2D(k,1)    = 1
   real4BufferVolumeDataUpdateBounds2D(k,1)   = 1
   real8BufferVolumeDataUpdateBounds2D(k,1)   = 1
   logicalBufferVolumeDataUpdateBounds2D(k,1) = 1
   int4BufferVolumeDataUpdateBounds2D(k,2)    = 0
   int8BufferVolumeDataUpdateBounds2D(k,2)    = 0
   real4BufferVolumeDataUpdateBounds2D(k,2)   = 0
   real8BufferVolumeDataUpdateBounds2D(k,2)   = 0
   logicalBufferVolumeDataUpdateBounds2D(k,2) = 0
  END DO

  DO k=1,2
   int4BufferVolumeDataTotalBounds2D(k,1)     = LBOUND(int4Data2D,k)
   int4BufferVolumeDataTotalBounds2D(k,2)     = UBOUND(int4Data2D,k)
   int8BufferVolumeDataTotalBounds2D(k,1)     = LBOUND(int8Data2D,k)
   int8BufferVolumeDataTotalBounds2D(k,2)     = UBOUND(int8Data2D,k)
   real4BufferVolumeDataTotalBounds2D(k,1)    = LBOUND(real4Data2D,k)
   real4BufferVolumeDataTotalBounds2D(k,2)    = UBOUND(real4Data2D,k)
   real8BufferVolumeDataTotalBounds2D(k,1)    = LBOUND(real8Data2D,k)
   real8BufferVolumeDataTotalBounds2D(k,2)    = UBOUND(real8Data2D,k)
   logicalBufferVolumeDataTotalBounds2D(k,1)  = LBOUND(logicalData2D,k)
   logicalBufferVolumeDataTotalBounds2D(k,2)  = UBOUND(logicalData2D,k)
   int4BufferVolumeDataUpdateBounds2D(k,1)    = LBOUND(int4Data2D,k)+1
   int4BufferVolumeDataUpdateBounds2D(k,2)    = UBOUND(int4Data2D,k)-1
   int8BufferVolumeDataUpdateBounds2D(k,1)    = LBOUND(int8Data2D,k)+1
   int8BufferVolumeDataUpdateBounds2D(k,2)    = UBOUND(int8Data2D,k)-1
   real4BufferVolumeDataUpdateBounds2D(k,1)   = LBOUND(real4Data2D,k)+1
   real4BufferVolumeDataUpdateBounds2D(k,2)   = UBOUND(real4Data2D,k)-1
   real8BufferVolumeDataUpdateBounds2D(k,1)   = LBOUND(real8Data2D,k)+1
   real8BufferVolumeDataUpdateBounds2D(k,2)   = UBOUND(real8Data2D,k)-1
   logicalBufferVolumeDataUpdateBounds2D(k,1) = LBOUND(logicalData2D,k)+1
   logicalBufferVolumeDataUpdateBounds2D(k,2) = UBOUND(logicalData2D,k)-1
  END DO
  
! now fill in the data

! int4

  rFac = REAL((int4Hi+1-int4Lo),real8Def)

    DO j=int4BufferVolumeDataTotalBounds2D(2,1), &
          int4BufferVolumeDataTotalBounds2D(2,2)
     DO i=int4BufferVolumeDataTotalBounds2D(1,1), &
           int4BufferVolumeDataTotalBounds2D(1,2)
      CALL RANDOM_NUMBER(rDum)
      int4Data2D(i,j) = int4Lo + FLOOR(rFac*rDum)
     END DO
    END DO

!int8

  rFac = REAL((int8Hi+1-int8Lo),real8Def)

    DO j=int8BufferVolumeDataTotalBounds2D(2,1), &
          int8BufferVolumeDataTotalBounds2D(2,2)
     DO i=int8BufferVolumeDataTotalBounds2D(1,1), &
           int8BufferVolumeDataTotalBounds2D(1,2)
      CALL RANDOM_NUMBER(rDum)
      int8Data2D(i,j) = int8Lo + FLOOR(rFac*rDum)
     END DO
    END DO

!real4

  rFac = (real4Hi-real4Lo)

    DO j=real4BufferVolumeDataTotalBounds2D(2,1), &
          real4BufferVolumeDataTotalBounds2D(2,2)
     DO i=real4BufferVolumeDataTotalBounds2D(1,1), &
           real4BufferVolumeDataTotalBounds2D(1,2)
      CALL RANDOM_NUMBER(rDum)
      real4Data2D(i,j) = real4Lo + REAL(rFac*rDum,real4Def)
     END DO
    END DO

!real8

  rFac = (real8Hi-real8Lo)

    DO j=real8BufferVolumeDataTotalBounds2D(2,1), &
          real8BufferVolumeDataTotalBounds2D(2,2)
     DO i=real8BufferVolumeDataTotalBounds2D(1,1), &
           real8BufferVolumeDataTotalBounds2D(1,2)
      CALL RANDOM_NUMBER(rDum)
      real8Data2D(i,j) = real8Lo + (rFac*rDum)
     END DO
    END DO

!logical

    DO j=logicalBufferVolumeDataTotalBounds2D(2,1), &
          logicalBufferVolumeDataTotalBounds2D(2,2)
     DO i=logicalBufferVolumeDataTotalBounds2D(1,1), &
           logicalBufferVolumeDataTotalBounds2D(1,2)
      CALL RANDOM_NUMBER(rDum)
      IF (rDum < 0.0_real8Def) THEN
       logicalData2D(i,j) = .FALSE.
      ELSE
       logicalData2D(i,j) = .TRUE.
      END IF
     END DO
    END DO

! 1D

  DO k=1,1
   int4BufferVolumeDataTotalBounds1D(k,1)     = 1
   int8BufferVolumeDataTotalBounds1D(k,1)     = 1
   real4BufferVolumeDataTotalBounds1D(k,1)    = 1
   real8BufferVolumeDataTotalBounds1D(k,1)    = 1
   logicalBufferVolumeDataTotalBounds1D(k,1)  = 1
   int4BufferVolumeDataTotalBounds1D(k,2)     = 0
   int8BufferVolumeDataTotalBounds1D(k,2)     = 0
   real4BufferVolumeDataTotalBounds1D(k,2)    = 0
   real8BufferVolumeDataTotalBounds1D(k,2)    = 0
   logicalBufferVolumeDataTotalBounds1D(k,2)  = 0
   int4BufferVolumeDataUpdateBounds1D(k,1)    = 1
   int8BufferVolumeDataUpdateBounds1D(k,1)    = 1
   real4BufferVolumeDataUpdateBounds1D(k,1)   = 1
   real8BufferVolumeDataUpdateBounds1D(k,1)   = 1
   logicalBufferVolumeDataUpdateBounds1D(k,1) = 1
   int4BufferVolumeDataUpdateBounds1D(k,2)    = 0
   int8BufferVolumeDataUpdateBounds1D(k,2)    = 0
   real4BufferVolumeDataUpdateBounds1D(k,2)   = 0
   real8BufferVolumeDataUpdateBounds1D(k,2)   = 0
   logicalBufferVolumeDataUpdateBounds1D(k,2) = 0
  END DO

  DO k=1,1
   int4BufferVolumeDataTotalBounds1D(k,1)     = LBOUND(int4Data1D,k)
   int4BufferVolumeDataTotalBounds1D(k,2)     = UBOUND(int4Data1D,k)
   int8BufferVolumeDataTotalBounds1D(k,1)     = LBOUND(int8Data1D,k)
   int8BufferVolumeDataTotalBounds1D(k,2)     = UBOUND(int8Data1D,k)
   real4BufferVolumeDataTotalBounds1D(k,1)    = LBOUND(real4Data1D,k)
   real4BufferVolumeDataTotalBounds1D(k,2)    = UBOUND(real4Data1D,k)
   real8BufferVolumeDataTotalBounds1D(k,1)    = LBOUND(real8Data1D,k)
   real8BufferVolumeDataTotalBounds1D(k,2)    = UBOUND(real8Data1D,k)
   logicalBufferVolumeDataTotalBounds1D(k,1)  = LBOUND(logicalData1D,k)
   logicalBufferVolumeDataTotalBounds1D(k,2)  = UBOUND(logicalData1D,k)
   int4BufferVolumeDataUpdateBounds1D(k,1)    = LBOUND(int4Data1D,k)+1
   int4BufferVolumeDataUpdateBounds1D(k,2)    = UBOUND(int4Data1D,k)-1
   int8BufferVolumeDataUpdateBounds1D(k,1)    = LBOUND(int8Data1D,k)+1
   int8BufferVolumeDataUpdateBounds1D(k,2)    = UBOUND(int8Data1D,k)-1
   real4BufferVolumeDataUpdateBounds1D(k,1)   = LBOUND(real4Data1D,k)+1
   real4BufferVolumeDataUpdateBounds1D(k,2)   = UBOUND(real4Data1D,k)-1
   real8BufferVolumeDataUpdateBounds1D(k,1)   = LBOUND(real8Data1D,k)+1
   real8BufferVolumeDataUpdateBounds1D(k,2)   = UBOUND(real8Data1D,k)-1
   logicalBufferVolumeDataUpdateBounds1D(k,1) = LBOUND(logicalData1D,k)+1
   logicalBufferVolumeDataUpdateBounds1D(k,2) = UBOUND(logicalData1D,k)-1
  END DO
  
! now fill in the data

! int4

  rFac = REAL((int4Hi+1-int4Lo),real8Def)

     DO i=int4BufferVolumeDataTotalBounds1D(1,1), &
           int4BufferVolumeDataTotalBounds1D(1,2)
      CALL RANDOM_NUMBER(rDum)
      int4Data1D(i) = int4Lo + FLOOR(rFac*rDum)
     END DO

!int8

  rFac = REAL((int8Hi+1-int8Lo),real8Def)

     DO i=int8BufferVolumeDataTotalBounds1D(1,1), &
           int8BufferVolumeDataTotalBounds1D(1,2)
      CALL RANDOM_NUMBER(rDum)
      int8Data1D(i) = int8Lo + FLOOR(rFac*rDum)
     END DO

!real4

  rFac = (real4Hi-real4Lo)

     DO i=real4BufferVolumeDataTotalBounds1D(1,1), &
           real4BufferVolumeDataTotalBounds1D(1,2)
      CALL RANDOM_NUMBER(rDum)
      real4Data1D(i) = real4Lo + REAL(rFac*rDum,real4Def)
     END DO

!real8

  rFac = (real8Hi-real8Lo)

     DO i=real8BufferVolumeDataTotalBounds1D(1,1), &
           real8BufferVolumeDataTotalBounds1D(1,2)
      CALL RANDOM_NUMBER(rDum)
      real8Data1D(i) = real8Lo + (rFac*rDum)
     END DO

!logical

     DO i=logicalBufferVolumeDataTotalBounds1D(1,1), &
           logicalBufferVolumeDataTotalBounds1D(1,2)
      CALL RANDOM_NUMBER(rDum)
      IF (rDum < 0.0_real8Def) THEN
       logicalData1D(i) = .FALSE.
      ELSE
       logicalData1D(i) = .TRUE.
      END IF
     END DO

!----------include the actual testing routines

include 'DataVolume5DClassCreateAndDestroyTest5DInt4.f90'
include 'DataVolume5DClassCreateAndDestroyTest5DInt8.f90'
include 'DataVolume5DClassCreateAndDestroyTest5DReal4.f90'
include 'DataVolume5DClassCreateAndDestroyTest5DReal8.f90'
include 'DataVolume5DClassCreateAndDestroyTest5DLogical.f90'

include 'DataVolume5DClassCreateAndDestroyTest4DInt4.f90'
include 'DataVolume5DClassCreateAndDestroyTest4DInt8.f90'
include 'DataVolume5DClassCreateAndDestroyTest4DReal4.f90'
include 'DataVolume5DClassCreateAndDestroyTest4DReal8.f90'
include 'DataVolume5DClassCreateAndDestroyTest4DLogical.f90'

include 'DataVolume5DClassCreateAndDestroyTest3DInt4.f90'
include 'DataVolume5DClassCreateAndDestroyTest3DInt8.f90'
include 'DataVolume5DClassCreateAndDestroyTest3DReal4.f90'
include 'DataVolume5DClassCreateAndDestroyTest3DReal8.f90'
include 'DataVolume5DClassCreateAndDestroyTest3DLogical.f90'

include 'DataVolume5DClassCreateAndDestroyTest2DInt4.f90'
include 'DataVolume5DClassCreateAndDestroyTest2DInt8.f90'
include 'DataVolume5DClassCreateAndDestroyTest2DReal4.f90'
include 'DataVolume5DClassCreateAndDestroyTest2DReal8.f90'
include 'DataVolume5DClassCreateAndDestroyTest2DLogical.f90'

include 'DataVolume5DClassCreateAndDestroyTest1DInt4.f90'
include 'DataVolume5DClassCreateAndDestroyTest1DInt8.f90'
include 'DataVolume5DClassCreateAndDestroyTest1DReal4.f90'
include 'DataVolume5DClassCreateAndDestroyTest1DReal8.f90'
include 'DataVolume5DClassCreateAndDestroyTest1DLogical.f90'

  RETURN

100 CONTINUE

  IF (.NOT. passedTest) THEN
   IF (masterNode) THEN
    WRITE(0,*) '--------------------------------'
    WRITE(0,*) 'Testing object create and destroy routines.'
    WRITE(0,*) '      Tests have FAILED.'
    WRITE(0,*) '--------------------------------'
   END IF
   CALL SetError(object          = errorInfoObject, &
                 errorInfoString = charStringObject)

  END IF

 101 CONTINUE

  IF (.NOT. passedTest) THEN
   charStringObject%charString = ' in '//location
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

   charStringObject%charString = moduleLocation
   CALL AddErrorInformation(object          = errorInfoObject, &
                            errorInfoString = charStringObject)

  END IF

  RETURN
END SUBROUTINE CreateAndDestroyDataVolume5DTest

END MODULE DataVolume5DClassCreateAndDestroyTest
