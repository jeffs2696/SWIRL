PROGRAM BesselFunctionCode
      USE, INTRINSIC :: ISO_FORTRAN_ENV  
      IMPLICIT NONE
      INTEGER, PARAMETER :: &
          rDef = REAL64, &
          numberOfGridPoints = 50

      INTEGER :: &
          UNIT                  ,&
          azimuthal_mode_number ,&
          radial_mode_number    ,&
          i
      
      
      REAL(KIND=rDef) :: & 
          r_min, &
          r_max, &
          dr,  &
          AMN, &
          BMN, &
          hubTipRatio,&
          convergence_criteria ,& 
          PSI

      REAL(KIND=rDef), DIMENSION(:), ALLOCATABLE :: X

      REAL(KIND=rDef),DIMENSION(2) :: IERROR1, IERROR2
      REAL(KIND=rDef),DIMENSION(4) :: anfu_bessel_function_error 

      REAL(KIND=rDef) ::  anrt_convergence_flag 
      
      REAL(KIND=rDef) :: non_dimensional_roots


      ALLOCATE(X(numberOfGridPoints))

      ! Notes:

      r_min = 1.0_rDef 
      r_max = 3.0_rDef      

      hubTipRatio = r_min/r_max 

      dr    = (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)

      DO i =1,numberOfGridPoints

      X(i)  = (r_min+REAL(i-1, rDef)*dr)!r_max !radial grid
      
      
      ENDDO


      azimuthal_mode_number = 2 
      radial_mode_number = 1
      convergence_criteria = 1.0E-5_rDef
      
      
      CALL ANRT(&
          azimuthal_mode_number,&
          radial_mode_number,&
          hubTipRatio,&
          convergence_criteria,&
          non_dimensional_roots,&
          anfu_bessel_function_error,&
          anrt_convergence_flag)

      WRITE(0,*) 'k_mn=' ,non_dimensional_roots

      IF (anrt_convergence_flag .gt. 0.0_rDef) THEN
          WRITE(0,*) 'ERROR: ANRT DID NOT CONVERGE ',anrt_convergence_flag
      ELSE
      ENDIF

      ! STOP
      ! Obtaining A and B coefficients for radial mode shape
      CALL EIGEN(&
          azimuthal_mode_number, &
          hubTipRatio               , &
          non_dimensional_roots                  , &
          AMN                  , &
          BMN                  , &
          IERROR1)

      ! in the case of cylindrical ducts...
      ! AMN = 0.50_rDef
      ! BMN = 0.50_rDef
      ! AMN = 1.0_rDef
      ! BMN = 0.0_rDef
      WRITE(0,*) 'A =' , AMN
      WRITE(0,*) 'B =' , BMN
      WRITE(0,*) 'non_dimensional_roots =' , non_dimensional_roots
      OPEN(NEWUNIT=UNIT,FILE='radial_mode_data.dat') 
      WRITE(UNIT,*) 'radius ', 'pressure '
       
      DO i = 1,numberOfGridPoints

      CALL RMODE(&
      azimuthal_mode_number,&
      X(i),&
      AMN,&
      BMN,&
      PSI,&
      IERROR2)
  
      WRITE(UNIT,*) X(i)/r_max, PSI

      ENDDO
      CLOSE(UNIT)

END PROGRAM
! Notes:
! Pros of f90 for V072:
!   better management of inputs and outputs
!       - explicit interfaces
