PROGRAM TestGetDissipationModule
      
    USE,  INTRINSIC:: ISO_FORTRAN_ENV
    USE GetDissipationModule


      INTEGER, PARAMETER :: rDef = REAL64
    REAL(KIND = rDef), DIMENSION(:, :)  , ALLOCATABLE::&
    Q_n 
    REAL(KIND = rDef), DIMENSION(:, :)  , ALLOCATABLE :: &
    Dis
    REAL(KIND = rDef), DIMENSION(:)     , ALLOCATABLE :: &
    domain, &
    Jac_Curv

    REAL(KIND = rDef) ::&
    delta_x    ,&
    delta_tau


    INTEGER    ::&
    UNIT    ,&
    iMin    ,&
    iMax    ,&
    bMax    ,&
    bMin    ,&
    nD, i, j, len_nD, i_nD, number_of_grids, k

   INTEGER, DIMENSION(:) , ALLOCATABLE:: &
   grid_point_array
    CHARACTER(LEN=400) :: &
    i_nD_string,  &
    grid_point_string


    CONTINUE
    iMin = 1
    bMin = 1
    bMax = 1 

    len_nD = 5
    number_of_grids = 1
    delta_tau = 1.0_rDef

    iMax = 81 

    ALLOCATE(grid_point_array(number_of_grids))
      DO k = 1, number_of_grids

        grid_point_array(k) = iMax + 40
        iMax = iMax + 40 
        ALLOCATE(&
            domain(iMax),&
            Dis(iMax,bMax),&
            Q_n(iMax,bMax),&
            Jac_Curv(bMax)) 

      ! domain(1) = REAL(iMin,KIND=rDef)
    ! DO i = 2,iMax
    ! domain(i) = domain(i-1)  + delta_X 
      ! write(0,*) domain(i)
    ! ENDDO


    CALL EXECUTE_COMMAND_LINE('ls ../../')
    WRITE( grid_point_string,*)   (iMax)
    WRITE(0,*) TRIM(ADJUSTL(grid_point_string)) 
    OPEN(UNIT, &
    FILE='../Test1_npts'// &
    TRIM(ADJUSTL(grid_point_string)) // &
    '_fd2_domain_a_numerical_mode_shapes_upstream_cuton_radial_mode_number_01_np_0' // &
    TRIM(ADJUSTL(grid_point_string)) // '.dat',&
    STATUS='OLD',ACTION='READ')

    ! STOP
    
    DO j = 1,1
    DO i = 1,iMax

    READ(UNIT,*) domain(i) , Q_n(i,j)
    ! Q_n(i,j) = -Q_n(i,j)
    print *, domain(i), Q_n(i,j)

    ENDDO
    ENDDO

    CLOSE(UNIT)

    delta_X = (domain(2)-domain(1))
    WRITE(0,*) delta_X

    ! WRITE(0,*) Q_n

    ! STOP
    ! for preliminary testing
     ! DO j  = bMin,bMax
     !     DO i = iMin,iMax
     !         Q_n(i,j) =&
     !             SIN(REAL(0.6*(i),KIND=rDef)) + &
     !             SIN(REAL(0.5*(i),KIND=rDef)) + &
     !             SIN(REAL(0.4*(i),KIND=rDef)) + &
     !             SIN(REAL(0.3*(i),KIND=rDef)) + &
     !             SIN(REAL(0.2*(i),KIND=rDef)) + &
     !             SIN(REAL(0.1*(i),KIND=rDef)) 

     !         Jac_Curv(j) = 1.0_rDef 
     !         WRITE(0,*) domain(i), Q_n(i,j)
     !     ENDDO
     ! ENDDO

     delta_X = (domain(2)-domain(1))

    DO i_nD = 1, len_nD

    nD = i_nD 
    CALL Dissipation_RHS(&
    iMin = iMin        ,&
    iMax = iMax        ,&
    bMin = bMin        ,&
    bMax = bMax        ,&
    delta_X = delta_X  ,&
    delta_tau = delta_tau   ,&
    nD = nD          ,&
    Dis  = Dis        ,&
    Q_n = Q_n         ,&
    Jac_Curv = Jac_Curv) 

    WRITE(i_nD_string,*) i_nD

    OPEN(NEWUNIT = UNIT , &
    FILE = &
        'SWIRL_Dissipation_nD_' // &
    TRIM(ADJUSTL(i_nD_string)) // '_' //& 
    TRIM(ADJUSTL(grid_point_string)) //&
        '_fd2' // &
        '.dat' )
    WRITE(UNIT,*) 'domain ', 'dissipation ','no_dissipation ',&
    'dissipation_subtracted '
    DO j= bMin,bMax
    DO i = iMin,iMax
    WRITE(UNIT,*) domain(i), Dis(i,j), Q_n(i,j), Q_n(i,j) + Dis(i,j)
    ENDDO
    ENDDO
    CLOSE(UNIT)
    END DO
    DEALLOCATE(&
            domain,&
            Dis,&
            Q_n,&
            Jac_Curv) 

    END DO
    DEALLOCATE(grid_point_array)
      
END PROGRAM TestGetDissipationModule


