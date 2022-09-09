MODULE trapezoidalRuleModule
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      PRIVATE 
      PUBLIC :: trapezoidalRule

      INTERFACE trapezoidalRule
          MODULE PROCEDURE trapezoidalRule1
      END INTERFACE trapezoidalRule

      INTEGER, PARAMETER :: rDef = REAL64 

  CONTAINS 

  SUBROUTINE trapezoidalRule1() 

  ! INTEGER, INTENT(IN) :: &
  ! npts 
      
  ! REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
  ! rr
        ! DO k=1,npts

        !     DO i = npts-1,k,-1

        !         IF (rr(i).gt.0.0_rDef) then

        !             rswi    = rmsw(i)*rmsw(i)/rr(i)
        !             rsw1    = rmsw(i+1)*rmsw(i+1)/rr(i+1)
        !             xi      = rr(i)
        !             x1      = rr(i+1)
        !             snd(i) = snd(i+1) +0.5_rDef*(rswi +rsw1)*(x1 -xi)

        !         ELSE
        !             snd(i) = 2.0_rDef*rmsw(i)*(rmsw(i+1) -rmsw(i))/rr(i+1)

        !         ENDIF

        !     ENDDO

        !     snd(k) = exp(-0.5_rDef*gm1*snd(k))


        ! END DO
    END SUBROUTINE trapezoidalRule1
END MODULE trapezoidalRuleModule
