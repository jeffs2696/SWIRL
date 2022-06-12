PROGRAM MAIN
    USE, INTRINSIC :: ISO_FORTRAN_ENV

    IMPLICIT NONE

    INTEGER, PARAMETER :: &
        rDef = REAL64

    INTEGER :: &
        UNIT ,&
        i , &
        j , &
        numberOfXlocations , &
        numberOfPoints 

    REAL(KIND=rDef) :: x_max, x_min, delta_x, &
        C, S_horiz, S_vert

    REAL(KIND = rDef) , DIMENSION(:), ALLOCATABLE :: &
        A , &
        B 

    REAL(KIND=rDef), DIMENSION(:), ALLOCATABLE :: &
        x, y, R, L, x_loc, y_loc!, A, B

    ! REAL(KIND=rDef), DIMENSION(:,:), ALLOCATABLE :: &
    !     R_array

    LOGICAL :: &
    ! debug = .FALSE.
        debug = .TRUE.

    CONTINUE

    numberOfPoints = 250
    numberOfXlocations = 3

    x_max = 0.70_rDef
    x_min = 0.20_rDef

    ! Equal Width between points requires grid spacing
    delta_x = (x_max-x_min)/REAL(numberOfPoints,KIND = rDef)
    ! delta_tanh = 0.50_rDef

    C      = 1.0_rDef
    S_vert = C
    S_horiz= 1.0_rDef

    ALLOCATE( &
        x(numberOfPoints) ,&
        y(numberOfPoints) ,&
        R(numberOfPoints) ,&
        L(numberOfPoints))!,&

    ALLOCATE( &
        x_loc(numberOfXlocations) , &
        y_loc(numberOfXlocations) , &
        A(numberOfXlocations)     , &
        B(numberOfXlocations)     )

    DO i = 1,numberOfXlocations
        A(i) = 0.01_rDef ! Amplitude
        B(i) = 10.0_rdef ! Concavity 
    ENDDO


    x_loc(1) = x_max
    x_loc(2) = 0.50_rDef
    x_loc(3) = 0.20_rDef

    y_loc(1) = 0.0_rDef
    y_loc(2) = 0.0_rDef
    y_loc(3) = 0.0_rDef

    DO i = 1,numberOfPoints

        x(i) = x_min + delta_x*REAL(i,KIND = rDef)

    ENDDO

    DO i = 1,numberOfPoints

        R(i) = 0.0_rDef
        L(i) = 0.0_rDef
        y(i) = 0.0_rDef

    ENDDO


    DO i = 1,numberOfPoints
        DO j = 1,SIZE(x_loc)

            R(i) = R(i) + y_loc(j) + A(j)*tanh(B(j)*(x(i) - x_loc(j)))
            L(i) = L(i) + y_loc(j) + A(j)*tanh(B(j)*(x_loc(j) - x_max))
            y(i) = S_vert + (L(i)+R(i))         

        ENDDO
    ENDDO

! Writing data to file
    OPEN(NEWUNIT = UNIT, FILE = '../ExecutableFiles/data.dat')
    DO i = 1,numberOfPoints
        WRITE(UNIT,*)  x(i), y(i)


        IF (debug) THEN
            WRITE(0,*) x(i), y(i)
        ENDIF

    ENDDO
    CLOSE(UNIT)
    DEALLOCATE( &
        x ,&
        y ,&
        R ,&
        L ,&
        A ,&
        B ,&
        x_loc)
END PROGRAM MAIN
