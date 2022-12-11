! BesselFunctionCode
! Author - Jeff Severino
! Date - 11/17/22
!
! Description -
! This code tests various subroutines in the v070nasalib needed to produce 
! a radial mode shape for annular ducts
!
PROGRAM BesselFunctionCode
      USE, INTRINSIC :: ISO_FORTRAN_ENV  
      IMPLICIT NONE

      INTEGER, PARAMETER :: &
          rDef = REAL64, &
          numberOfGridPoints = 50

      LOGICAL :: &
          debug_flag = .FALSE.

      CHARACTER(LEN = 50) :: &
          filename

      INTEGER :: &
          UNIT                  ,&
          azimuthal_mode_number ,&
          radial_mode_number    ,&
          i
      
      
      REAL(KIND=rDef) :: & 
          r_min, &
          r_max, &
          dr,  &
          weighting_coefficient_A, &
          weighting_coefficient_B, &
          hubTipRatio,&
          convergence_criteria ,& 
          mode_shape

      REAL(KIND=rDef), DIMENSION(:), ALLOCATABLE :: &
          radial_grid

      REAL(KIND=rDef),DIMENSION(2) :: &
          rmode_bessel_function_errors

      REAL(KIND=rDef),DIMENSION(4) :: &
          eigen_bessel_function_errors , &
          anfu_bessel_function_errors 

      REAL(KIND=rDef) :: &
          anrt_convergence_flag 
      
      REAL(KIND=rDef) :: &
          non_dimensional_roots


      ALLOCATE(radial_grid(numberOfGridPoints))


      r_min = 0.045537!0.20_rDef 
      r_max = 0.27763!1.0_rDef      
      azimuthal_mode_number = 2 
      radial_mode_number = 2
      convergence_criteria = 1.0E-12_rDef
      hubTipRatio = r_min/r_max 

      dr    = (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)

      DO i =1,numberOfGridPoints

      radial_grid(i)  = (r_min+REAL(i-1, rDef)*dr)!radial grid 
      
      ENDDO
      
      CALL ANRT(&
          azimuthal_mode_number,&
          radial_mode_number,&
          hubTipRatio,&
          convergence_criteria,&
          non_dimensional_roots,&
          anfu_bessel_function_errors,&
          anrt_convergence_flag)

      ! checking ANRT result

      IF (anrt_convergence_flag .gt. 0.0_rDef) THEN
          WRITE(0,*) 'ERROR: ANRT DID NOT CONVERGE ',anrt_convergence_flag
      ELSE
      ENDIF

      IF (debug_flag.eqv..TRUE.) THEN
          ! WRITE(0,*) 'k_mn r_max' ,non_dimensional_roots
      ELSE
      ENDIF
      
      ! Obtaining A and B coefficients for radial mode shape

      CALL EIGEN(&
          azimuthal_mode_number, &
          hubTipRatio          , &
          non_dimensional_roots, &
          weighting_coefficient_A                  , &
          weighting_coefficient_B                  , &
          eigen_bessel_function_errors)

      IF (debug_flag.eqv..TRUE.) THEN
          WRITE(0,*) 'A ' , weighting_coefficient_A
          WRITE(0,*) 'B ' , weighting_coefficient_B
          WRITE(0,*) 'k_mn r_max ' , non_dimensional_roots
          WRITE(0,*) 'k_mn ' , non_dimensional_roots/r_max
      ELSE
      ENDIF

      filename = 'radial_mode_data.dat'
      OPEN(NEWUNIT=UNIT,FILE=TRIM(ADJUSTL(filename))) 

      WRITE(UNIT,*) &
          'radius ', &
          'pressure '
       
      DO i = 1,numberOfGridPoints

      CALL RMODE(&
      azimuthal_mode_number,&
      non_dimensional_roots*radial_grid(i)/r_max,&
      weighting_coefficient_A,&
      weighting_coefficient_B,&
      mode_shape,&
      rmode_bessel_function_errors)
  
      WRITE(UNIT,*) &
          radial_grid(i)/r_max,&
          mode_shape 

      ENDDO

      CLOSE(UNIT)

      OPEN(NEWUNIT=UNIT,FILE='radial_mode_parameters.dat')
      WRITE(UNIT,*) &
          'azimuthal_mode_number ', &
          'radial_mode_number ', &
          'weighting_factor_A ', &
          'weighting_factor_B ', &
          'non_dimensional_roots ' 
      WRITE(UNIT,*) &
          azimuthal_mode_number, &
          radial_mode_number , &
          weighting_coefficient_A, &
          weighting_coefficient_B, &
          non_dimensional_roots 
      CLOSE(UNIT)

      IF (debug_flag) THEN
      ENDIF
END PROGRAM
! Notes:
! Pros of f90 for V072:
!   better management of inputs and outputs
!       - explicit interfaces
