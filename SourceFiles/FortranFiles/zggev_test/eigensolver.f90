program eigensolver
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    USE L2NormModule
    USE F90_ZGGEV
    IMPLICIT NONE

    INTEGER, PARAMETER :: &
        rDef = REAL64

    INTEGER :: N, LDA, LDB, LDVL, LDVR, INFO, LWORK, i, j, eigenIndex
    LOGICAL :: precondition_flag
    COMPLEX(KIND=rDef) :: L2
    COMPLEX(KIND=rDef) , DIMENSION(8) :: WORK
    REAL(KIND=rDef) , DIMENSION(:)  , ALLOCATABLE :: RWORK

    COMPLEX(KIND=rDef), DIMENSION(:,:), ALLOCATABLE :: A(:,:),A_before(:,:), B(:,:) , B_before(:,:)
    COMPLEX(KIND=rDef), DIMENSION(:), ALLOCATABLE :: ALPHA(:), BETA(:), VL(:,:), VR(:,:) , WVN(:), S(:)
    CHARACTER(LEN=1) JOBVL,JOBVR

    CONTINUE

    eigenIndex = 2
    N = 2
    LDA = N
    LDB = N
    LDVL = N
    LDVR = N
    JOBVL = 'N'
    JOBVR = 'V'
    ALLOCATE(&
        WVN(N)  , &
        S(N)    , &
        A(LDA,N), &
        A_before(LDA,N), &
        B_before(LDA,N), &
        B(LDB,N), &
        ALPHA(N), &
        BETA(N), &
        VL(LDVL,N), &
        VR(LDVR,N),&
        RWORK(8*N))

    IF (N.eq.3) THEN
        A  = reshape([&
            (1.0, 2.0), (3.0, 4.0), (5.0, 6.0), &
            (7.0, 8.0), (9.0, 10.0), (11.0, 12.0), &
            (13.0, 0.0), (14.0, 0.0), (15.0, 0.0)], [N,N])
        B = reshape([(1.0, 0.0), (0.0, 0.0), (0.0, 0.0), &
            (0.0, 0.0), (1.0, 0.0), (0.0, 0.0), &
            (0.0, 0.0), (0.0, 0.0), (1.0, 0.0)], [N,N])
    ELSE IF (N.eq.2) THEN
        A  = reshape([&
            (0.0, 0.0), (-2.0, 0.0), &
            (1.0, 0.0), (-3.0, 0.0)], [N,N])

        B = reshape([(1.0, 0.0), (0.0, 0.0), &
            (0.0, 0.0), (1.0, 0.0)],  [N,N])

    ENDIF
    DO i = 1,N
    WRITE(0,*) A(i,:)
    ENDDO
    
    ! Saving matricies before they are changed in the LAPACK CALL
    A_before = A
    B_before = B

    ! Compute the eigenvalues and eigenvectors
    ! Do you want to "precondition" or run first try? 
    ! See documentation on LWORK for more information
    ! SWIRL currently does not
    precondition_flag = .FALSE.

    IF (precondition_flag) THEN
        LWORK = -1 

        CALL ZGGEV(JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA, &
            VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, info) 

        LWORK = INT(REAL(WORK(1)))
        WORK = 0

        CALL ZGGEV(JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA, &
            VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, info) 
        
    ELSE
        LWORK = 2*N

        CALL ZGGEV(JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA, &
            VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, info) 
    ENDIF

    IF (info .ne. 0) THEN
        WRITE(0,*) 'The eigenvalue computation failed with error code: ', info
    ELSE
        ! Output the eigenvalues and eigenvectors
        WRITE(0,*) 'Eigenvalues, (alpha):'
        WRITE(0,'(2f16.12)')  ALPHA
        WRITE(0,*) 'Generalized Eigenvalues (alpha/beta):'
        WRITE(0,'(2f16.12)')  ALPHA/BETA
        WRITE(0,*) 'Eigenvectors:'
        WRITE(0,*) 'real', 'imag'
        ! DO i = 1, n
         ! WRITE(0,*) i
         DO j = 1, n
            WRITE(0,*) real(VR(:,j)), aimag(VR(:,j))
         END DO
        ! END DO
    END IF

    ! STOP
    ! Determine the optimal size of the work array
    ! Check for convergence

    DO i = 1,N
        WVN(i) = ALPHA(i)/BETA(i)
    END DO
    WRITE(0,*) WVN(eigenIndex)
    
    
    S = MATMUL(A_before,VR(:,eigenIndex)) - MATMUL(B_before,VR(:,eigenIndex))*(WVN(eigenIndex))
    DO i = 1,N
        WRITE(0,*) S(i) 
    END DO

    CALL getL2Norm(L2,S)
    WRITE(0,*) 'L2'
    
    WRITE(0,*) L2
    DEALLOCATE(A, B,  BETA, VL, VR, RWORK, S, WVN)
END PROGRAM eigensolver

