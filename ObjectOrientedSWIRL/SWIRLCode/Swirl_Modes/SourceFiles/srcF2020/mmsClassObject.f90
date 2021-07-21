MODULE swirlClassObject
!
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      USE analysisModule
      USE boundaryModule
      USE derivsModule
      USE fdgridModule
      USE fdrivsModule
      USE globalModule
      USE gridModule
      ! USE inputModule
      ! USE interpModule
      ! USE machoutModule
      USE rmachModule
      USE smachAndSndspdModule
      USE FindResidualVectorModule
      USE L2NormModule

      IMPLICIT NONE

      PRIVATE
      PUBLIC ::&
          FindResidualData

! Interfaces

! Finds S = [A]{x} - i*eigVal*[B]{x}, x \equiv eigen vector
         INTERFACE FindResidualData
             MODULE PROCEDURE GetResidualVector
         END INTERFACE FindResidualData
!

      INTEGER, PARAMETER:: rDef = REAL64
! Type Declaration and necessary variables
      TYPE SwirlClassType
      END TYPE SwirlClassType

! Local Variable Declaration
  CONTAINS

      SUBROUTINE GetResidualVector(object,S)
          TYPE(SwirlClassType), INTENT(IN) ::&
              object
          COMPLEX(KIND = rDef), DIMENSION(object%numberOfRadialPoints*4), INTENT(INOUT) :: &
          S

          INTEGER :: SZ

          IF (object%isInitialized.eqv..TRUE.) then
              SZ = SIZE(S)
              WRITE(6,*) SZ
          endif
          CALL getSvector( &
              A      = object%aa_before ,   &
              B      = object%bb_before ,   &
              x      = object%vr        ,   &
              lambda = object%wvn       ,   &
              np4    = SZ              ,   &
              S_MMS  = S ) 

      S = object%S_MMS
      ! write(6,*) 

      END SUBROUTINE GetResidualVector
!
  END MODULE swirlClassObject
