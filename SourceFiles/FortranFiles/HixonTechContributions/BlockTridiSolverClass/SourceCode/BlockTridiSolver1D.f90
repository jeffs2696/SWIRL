!-------------------1D block tridi solvers----------------------------------

SUBROUTINE BLKTRI11(numVar,      &
                    iSolveStart, &
                    iSolveEnd,   &
                    aStart,      &
                    A,           &
                    bStart,      &
                    B,           &
                    cStart,      &
                    C,           &
                    xStart,      &
                    X,           &
                    rStart,      &
                    R)

!     .......THIS PROGRAM SOLVES A TRI-BLOCK-DIAGONAL MATRIX.
!      .......numVar = SIZE OF BLOCKS
!      .......nPts   = NUMBER OF BLOCKS
!      .......A,B,C = numVar*numVar MATRICES; A GOES FROM 2 TO nPts, B GOES
!      .......        FROM 1 TO nPts AND C GOES FROM 1 TO nPts-1.
!      .......X = RESULT VECTOR (nPts,numVar)
!      .......R = RIGHT HAND SIDE VECTOR (nPts,numVar)
!     C
      INTEGER, INTENT(IN) :: numVar
      INTEGER, DIMENSION(:), INTENT(IN) :: iSolveStart, & ! dimension 1
                                           iSolveEnd,   &
                                           aStart,      &
                                           bStart,      &
                                           cStart,      &
                                           xStart,      &
                                           rStart
      REAL(KIND=rDef), DIMENSION(aStart(1):,:,:), INTENT(INOUT) :: A
      REAL(KIND=rDef), DIMENSION(bStart(1):,:,:), INTENT(INOUT) :: B
      REAL(KIND=rDef), DIMENSION(cStart(1):,:,:), INTENT(INOUT) :: C
      REAL(KIND=rDef), DIMENSION(xStart(1):,:), INTENT(INOUT) :: X
      REAL(KIND=rDef), DIMENSION(rStart(1):,:), INTENT(INOUT) :: R
!     REAL(KIND=rDef), DIMENSION(nPts(1),numVar,numVar), INTENT(INOUT) :: A, B, C
!     REAL(KIND=rDef), DIMENSION(nPts(1),numVar), INTENT(INOUT) :: X, R
!
! define local variables
!
      INTEGER :: I, M, N, NN
      REAL(KIND=rDef) :: S, T

      CONTINUE ! execution begins here

!     .....FORWARD ELIMINATION-BLOCKS

      DO I=iSolveStart(1),iSolveEnd(1)-1

!     .....GAUSS JORDAN FOR B(I)

       DO M=1,numVar-1
!     .....NORMALIZE PIVOT ROW

        T=1.0_rDef/B(I,M,M)

        DO N=M+1,numVar
         B(I,M,N) = T*B(I,M,N)
        END DO

        DO N=1,numVar
         C(I,M,N) = T*C(I,M,N)
        END DO

        R(I,M) = T*R(I,M)

!     .....LOWER ELIMINATION IN B

        DO N=M+1,numVar

         DO NN=M+1,numVar
          B(I,N,NN) = B(I,N,NN)-B(I,N,M)*B(I,M,NN)
         END DO

         DO NN=1,numVar
          C(I,N,NN) = C(I,N,NN)-B(I,N,M)*C(I,M,NN)
         END DO
         R(I,N) = R(I,N)-B(I,N,M)*R(I,M)
        END DO ! n loop

       END DO ! m loop

!     .....UPPER (GAUSS JORDAN) ELIMINATION IN B
       T= 1.0_rDef/B(I,numVar,numVar)

       DO NN = 1,numVar
        C(I,numVar,NN) = T*C(I,numVar,NN)
       END DO

       R(I,numVar) = T*R(I,numVar)

       DO N=numVar,2,-1
        DO M=N-1,1,-1
         DO NN=1,numVar
          C(I,M,NN) = C(I,M,NN)-B(I,M,N)*C(I,N,NN)
         END DO

         R(I,M) = R(I,M)-B(I,M,N)*R(I,N)
        END DO ! m loop
       END DO  ! n loop
!     .....B(I) IS NOW THE UNIT MATRIX
!     .....ELIMINATE A(I+1)
       DO N=1,numVar
        DO M=1,numVar
         DO NN=1,numVar
          B(I+1,M,NN) = B(I+1,M,NN)-A(I+1,M,N)*C(I,N,NN)
         END DO ! nn loop
         R(I+1,M) = R(I+1,M)-A(I+1,M,N)*R(I,N)
        END DO ! m loop
       END DO ! n loop
      END DO  ! i loop

! no upper diagonal at last point in the matrix

      I = iSolveEnd(1)

!     .....GAUSS JORDAN FOR B(I)

      DO M=1,numVar-1
!     .....NORMALIZE PIVOT ROW

       T=1.0_rDef/B(I,M,M)

       DO N=M+1,numVar
        B(I,M,N) = T*B(I,M,N)
       END DO

       R(I,M) = T*R(I,M)

!     .....LOWER ELIMINATION IN B

       DO N=M+1,numVar

        DO NN=M+1,numVar
         B(I,N,NN) = B(I,N,NN)-B(I,N,M)*B(I,M,NN)
        END DO

        R(I,N) = R(I,N)-B(I,N,M)*R(I,M)
       END DO ! n loop

      END DO ! m loop

!     .....UPPER (GAUSS JORDAN) ELIMINATION IN B
      T= 1.0_rDef/B(I,numVar,numVar)

      R(I,numVar) = T*R(I,numVar)

      DO N=numVar,2,-1
       DO M=N-1,1,-1
        R(I,M) = R(I,M)-B(I,M,N)*R(I,N)
       END DO ! m loop
      END DO  ! n loop
!     .....B(iSolveEnd(1)) IS NOW THE UNIT MATRIX

!     .....BACK SUBSTITUTION

      DO M=1,numVar
       X(I,M) = R(I,M)
      END DO

      DO I=iSolveEnd(1)-1,iSolveStart(1),-1
       DO M=numVar,1,-1
        S=0.0_rDef
        DO NN=1,numVar
         S = C(I,M,NN)*X(I+1,NN)+S
        END DO ! nn loop
        X(I,M) = R(I,M)-S
       END DO ! m loop
      END DO ! i loop

      RETURN
END SUBROUTINE BLKTRI11

!-----------------------------original BLKTRI routine----------------------
!     SUBROUTINE BLKTRI(NDIM, &
!                       MDIM, &
!                       M,    &
!                       N,    &
!                       A,    &
!                       B,    &
!                       C,    &
!                       X,    &
!                       R)

!!    .......THIS PROGRAM SOLVES A TRI-BLOCK-DIAGONAL MATRIX.
!!     .......NDIM = DIMENSION OF THE A,B AND C IN THE MAIN PROGRAM
!!     .......MDIM = DIMENSION OF BLOCKS IN MAIN PROGRAM
!!     .......M = SIZE OF BLOCKS
!!     .......N = NUMBER OF BLOCKS
!!     .......A,B,C = M*M MATRICES; A GOES FROM 2 TO N, B GOES
!!     .......        FROM 1 TO N AND C GOES FROM 1 TO N-1.
!!     .......X = RESULT VECTOR
!!     .......R = RIGHT HAND SIDE VECTOR
!!    C
!     INTEGER, INTENT(IN) :: NDIM, &
!                            MDIM
!     INTEGER, INTENT(IN) :: M, N
!     REAL(KIND=rDef), DIMENSION(:,:,:), INTENT(INOUT) :: A, B, C
!     REAL(KIND=rDef), DIMENSION(:,:), INTENT(INOUT) :: X, R
!!    REAL(KIND=rDef), DIMENSION(NDIM,MDIM,MDIM), INTENT(INOUT) :: A, B, C
!!    REAL(KIND=rDef), DIMENSION(NDIM,MDIM), INTENT(INOUT) :: X, R
!!    DIMENSION A(NDIM,MDIM,MDIM),B(NDIM,MDIM,MDIM),C(NDIM,MDIM,MDIM),
!!   1X(NDIM,MDIM),R(NDIM,MDIM)
!
!!define local variables
!
!     INTEGER :: I,J,JB,K,KK,KR,MP,MPM1,MR
!     REAL(KIND=rDef) :: S, T
!
!!    .....FORWARD ELIMINATION-BLOCKS

!     DO 1 I=1,N

!!    .....GAUSS JORDAN FOR B(I)

!      DO 2 J=1,M-1
!!    .....NORMALIZE PIVOT ROW

!       T=1.0_rDef/B(I,J,J)

!       DO 3 K=J+1,M
!        B(I,J,K) = T*B(I,J,K)
!   3   CONTINUE

!       IF (I < N) THEN

!        DO 4 K=1,M
!!       IF (I.EQ.N) GO TO 4
!         C(I,J,K) = T*C(I,J,K)
!    4   CONTINUE

!       ELSE
!        CONTINUE
!       END IF

!       R(I,J) = T*R(I,J)

!!    .....LOWER ELIMINATION IN B

!       DO 5 K=J+1,M

!        DO 6 KK=J+1,M
!         B(I,K,KK) = B(I,K,KK)-B(I,K,J)*B(I,J,KK)
!   6    CONTINUE

!        IF (I /= N) THEN
!         DO 7 KK=1,M
!!         IF (I.EQ.N) GO TO 7
!          C(I,K,KK) = C(I,K,KK)-B(I,K,J)*C(I,J,KK)
!    7    CONTINUE
!        ELSE
!         CONTINUE
!        END IF
!        R(I,K) = R(I,K)-B(I,K,J)*R(I,J)
!   5   CONTINUE

!   2  CONTINUE

!!    .....UPPER (GAUSS JORDAN) ELIMINATION IN B
!      T= 1.0_rDef/B(I,M,M)
!      DO 20 KK = 1,M
!       C(I,M,KK) = T*C(I,M,KK)
!  20  CONTINUE

!      R(I,M) = T*R(I,M)

!      DO 9  J=1,M-1
!       MP = M-J+1
!       MPM1=MP-1
!       DO 10 K=1,MPM1
!        MR = MP-K
!        IF ( I /= N) THEN
!         DO 11 KK=1,M
!!         IF ( I.EQ.N)GO TO 11
!          C(I,MR,KK) = C(I,MR,KK)-B(I,MR,MP)*C(I,MP,KK)
!   11    CONTINUE
!        ELSE
!         CONTINUE
!        END IF

!        R(I,MR) = R(I,MR)-B(I,MR,MP)*R(I,MP)
!  10   CONTINUE
!   9  CONTINUE
!!    .....B(I) IS NOW THE UNIT MATRIX
!!    .....ELIMINATE A(I+1)
!      IF (I == N) GO TO 1
!!     IF (I.EQ.N) GO TO 1
!      DO 12 J=1,M
!       DO 13 K=1,M
!        DO 14 KK=1,M
!         B(I+1,K,KK) = B(I+1,K,KK)-A(I+1,K,J)*C(I,J,KK)
!  14    CONTINUE
!        R(I+1,K) = R(I+1,K)-A(I+1,K,J)*R(I,J)
!  13   CONTINUE
!  12  CONTINUE
!   1 CONTINUE

!!    .....BACK SUBSTITUTION

!     DO 15 K=1,M
!      X(N,K) = R(N,K)
!  15 CONTINUE
!     DO 16 J=1,N-1
!      JB=N-J
!      DO 17 K=1,M
!       KR = M-K+1
!       S=0.0_rDef
!       DO 18 KK=1,M
!        S = C(JB,KR,KK)*X(JB+1,KK)+S
!  18   CONTINUE
!       X(JB,KR) = R(JB,KR)-S
!  17  CONTINUE
!  16 CONTINUE

!     RETURN
!     IF (NDIM > 0) CONTINUE ! to spoof the compiler
!     IF (MDIM > 0) CONTINUE

!     END SUBROUTINE BLKTRI

