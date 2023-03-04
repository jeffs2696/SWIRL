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
        xMin       ,&
        xMax       ,&
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

    xMin = 0.5
    xMax = 30
    len_nD = 5
    number_of_grids = 2
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

    WRITE( grid_point_string,*)   (iMax)

    delta_X = (xMax - xMin)/REAL(iMax,rDef) 

    WRITE(0,*) delta_X

    DO i = iMin,iMax
    domain(i) = (xMin + REAL(i,KIND=rDef)*delta_X)
    ENDDO
    ! for preliminary testing
    DO j  = bMin,bMax
    DO i = iMin,iMax
    Q_n(i,j) =&
        SIN(REAL(0.6*domain(i),KIND=rDef)) + &
        SIN(REAL(0.5*domain(i),KIND=rDef)) + &
        SIN(REAL(0.4*domain(i),KIND=rDef)) + &
        SIN(REAL(0.3*domain(i),KIND=rDef)) + &
        SIN(REAL(0.2*domain(i),KIND=rDef)) + &
        SIN(REAL(0.1*domain(i),KIND=rDef)) 

    Jac_Curv(j) = 1.0_rDef 
    WRITE(0,*) domain(i), Q_n(i,j)
    ENDDO
    ENDDO


    DO i_nD = 1, len_nD

    nD = i_nD
    CALL Dissipation_RHS(&
        iMin      = iMin        ,&
        iMax      = iMax        ,&
        bMin      = bMin        ,&
        bMax      = bMax        ,&
        delta_X   = delta_X     ,&
        delta_tau = delta_tau   ,&
        nD        = nD          ,&
        Dis       = Dis         ,&
        Q_n       = Q_n         ,&
        Jac_Curv  = Jac_Curv) 

    WRITE(i_nD_string,*) i_nD

    OPEN(NEWUNIT = UNIT , &
        FILE = &
        'TestDissipation' // &
        TRIM(ADJUSTL(i_nD_string)) // '_' //& 
        TRIM(ADJUSTL(grid_point_string)) //&
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


