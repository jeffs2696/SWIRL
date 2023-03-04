MODULE trapezoidalRuleModule
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      PRIVATE 
      PUBLIC :: trapezoidalRule

      INTERFACE trapezoidalRule
          MODULE PROCEDURE trapezoidalRule1
          MODULE PROCEDURE trapezoidalRule2 ! single value output
      END INTERFACE trapezoidalRule

      INTEGER, PARAMETER :: rDef = REAL64 

      
  CONTAINS 

  SUBROUTINE trapezoidalRule1(&
          rr  , &
          integrand , &
          output ) 


  ! INTEGER, INTENT(IN) :: &
  ! npts 
      
  REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
      rr, &
      integrand


  REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) ::&
      output

  INTEGER :: &
      npts, &
      i, &
      k

  REAL(KIND=rDef) :: &
      rsw1, &
      rswi, &
      x1, &
      xi,  &
      h , total !a, b, integral_result

  npts = SIZE(rr)
  h = (rr(npts) - rr(1))/(npts-1)
  

  total = 0.0_rDef
  output(1) = integrand(1)*rr(1)
  ! total = 0.5_rDef*(integrand(1) + integrand(2)) 
  

  
  ! output(1) = 0.5_rDef*(integrand( 1 ) + integrand(npts))
  ! output(npts) = 0.5_rDef*(integrand( npts - 1 ) + integrand(npts))
  DO i = 2 ,npts

      ! rswi = integrand(i-1)
      ! rsw1 = integrand(i)
      ! xi      = rr(i-1)
      ! x1      = rr(i)

      ! WRITE(0,*) i,k
      total = total + 0.5_rDef*( integrand(i-1) + integrand(i))
      output(i) = output(i-1) + total*h 

  END DO
  
  

  ! DO k=1,npts 
  ! DO i = npts-1,k,-1 
  ! IF (rr(i).gt.0.0_rDef) then

  !     ! rswi    = integrand(i)*integrand(i)/rr(i)
  !     ! rsw1    = integrand(i+1)*integrand(i+1)/rr(i+1)
  !     rswi = integrand(i)
  !     rsw1 = integrand(i+1)
  !     xi      = rr(i)
  !     x1      = rr(i+1)

  !     ! WRITE(0,*) i,k
  !     output(i) = output(i+1) + 0.5_rDef*(rswi +rsw1)*(x1 -xi)
  ! ELSE
  !     output(i) = 2.0_rDef*integrand(i)*(integrand(i+1) -integrand(i))/rr(i+1) 
  ! ENDIF 
  ! END DO 
  ! ! output(k) = output(k-1) + 0.5_rDef*(rswi +rsw1)*(x1 -xi)
  ! ! snd(k) = exp(-0.5_rDef*gm1*snd(k)) 
  ! END DO
! local variables 
! INTEGER :: i, &
!             j, &
!             k

!         REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) ::&
!             rmsw,  &
!             rmswp

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
    SUBROUTINE trapezoidalRule2(&
            rr  , &
            integrand , &
            total ) 


  ! INTEGER, INTENT(IN) :: &
  ! npts 
      
  REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
      rr, &
      integrand


  REAL(KIND=rDef),  INTENT(OUT) ::&
      total

  INTEGER :: &
      npts, &
      i, &
      k

  REAL(KIND=rDef) :: &
      rsw1, &
      rswi, &
      x1, &
      xi,  &
      h  !a, b, integral_result

  npts = SIZE(rr)
  h = (rr(npts) - rr(1))/(npts-1)
  

  total = 0.5_rDef*(integrand(1) + integrand(npts)) 
  

  
  ! output(1) = 0.5_rDef*(integrand( 1 ) + integrand(npts))
  ! output(npts) = 0.5_rDef*(integrand( npts - 1 ) + integrand(npts))
  DO i = 2 ,npts

      ! rswi = integrand(i-1)
      ! rsw1 = integrand(i)
      ! xi      = rr(i-1)
      ! x1      = rr(i)

      ! WRITE(0,*) i,k
      total = total + integrand(i) 

  END DO
  total = total*h
  
  

  ! DO k=1,npts 
  ! DO i = npts-1,k,-1 
  ! IF (rr(i).gt.0.0_rDef) then

  !     ! rswi    = integrand(i)*integrand(i)/rr(i)
  !     ! rsw1    = integrand(i+1)*integrand(i+1)/rr(i+1)
  !     rswi = integrand(i)
  !     rsw1 = integrand(i+1)
  !     xi      = rr(i)
  !     x1      = rr(i+1)

  !     ! WRITE(0,*) i,k
  !     output(i) = output(i+1) + 0.5_rDef*(rswi +rsw1)*(x1 -xi)
  ! ELSE
  !     output(i) = 2.0_rDef*integrand(i)*(integrand(i+1) -integrand(i))/rr(i+1) 
  ! ENDIF 
  ! END DO 
  ! ! output(k) = output(k-1) + 0.5_rDef*(rswi +rsw1)*(x1 -xi)
  ! ! snd(k) = exp(-0.5_rDef*gm1*snd(k)) 
  ! END DO
! local variables 
! INTEGER :: i, &
!             j, &
!             k

!         REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) ::&
!             rmsw,  &
!             rmswp

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
    END SUBROUTINE trapezoidalRule2
END MODULE trapezoidalRuleModule
