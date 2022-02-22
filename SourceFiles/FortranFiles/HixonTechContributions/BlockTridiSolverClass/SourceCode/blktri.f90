MODULE BlockTridiSolver

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: SolveBlockTridi

INTERFACE SolveBlockTridi

  MODULE PROCEDURE BLKTRI_Real32
  MODULE PROCEDURE BLKTRI_Real64

END INTERFACE SolveBlockTridi

CONTAINS

      SUBROUTINE BLKTRI_Real32(NDIM, &
                               MDIM, &
                               M,    &
                               N,    &
                               A,    &
                               B,    &
                               C,    &
                               X,    &
                               R)

!     .......THIS PROGRAM SOLVES A TRI-BLOCK-DIAGONAL MATRIX.
!      .......NDIM = DIMENSION OF THE A,B AND C IN THE MAIN PROGRAM
!      .......MDIM = DIMENSION OF BLOCKS IN MAIN PROGRAM
!      .......M = SIZE OF BLOCKS
!      .......N = NUMBER OF BLOCKS
!      .......A,B,C = M*M MATRICES; A GOES FROM 2 TO N, B GOES
!      .......        FROM 1 TO N AND C GOES FROM 1 TO N-1.
!      .......X = RESULT VECTOR
!      .......R = RIGHT HAND SIDE VECTOR
!     C
      INTEGER, INTENT(IN) :: NDIM, &
                             MDIM
      INTEGER, INTENT(IN) :: M, N
!     REAL(KIND=REAL32), DIMENSION(NDIM,MDIM,MDIM), INTENT(INOUT) :: A, B, C
!     REAL(KIND=REAL32), DIMENSION(NDIM,MDIM), INTENT(INOUT) :: X, R
      REAL(KIND=REAL32), DIMENSION(:,:,:), INTENT(INOUT) :: A, B, C
      REAL(KIND=REAL32), DIMENSION(:,:), INTENT(INOUT) :: X, R
!     DIMENSION A(NDIM,MDIM,MDIM),B(NDIM,MDIM,MDIM),C(NDIM,MDIM,MDIM),
!    1X(NDIM,MDIM),R(NDIM,MDIM)
!
! define local variables
!
      INTEGER :: I,J,JB,K,KK,KR,MP,MPM1,MR
      REAL(KIND=REAL32) :: S, T
!
!     .....FORWARD ELIMINATION-BLOCKS

      DO 1 I=1,N

!     .....GAUSS JORDAN FOR B(I)

       DO 2 J=1,M-1
!     .....NORMALIZE PIVOT ROW

        T=1.0_REAL32/B(I,J,J)

        DO 3 K=J+1,M
         B(I,J,K) = T*B(I,J,K)
    3   CONTINUE

        IF (I < N) THEN

         DO 4 K=1,M
!        IF (I.EQ.N) GO TO 4
          C(I,J,K) = T*C(I,J,K)
     4   CONTINUE

        ELSE
         CONTINUE
        END IF

        R(I,J) = T*R(I,J)

!     .....LOWER ELIMINATION IN B

        DO 5 K=J+1,M

         DO 6 KK=J+1,M
          B(I,K,KK) = B(I,K,KK)-B(I,K,J)*B(I,J,KK)
    6    CONTINUE

         IF (I /= N) THEN
          DO 7 KK=1,M
!          IF (I.EQ.N) GO TO 7
           C(I,K,KK) = C(I,K,KK)-B(I,K,J)*C(I,J,KK)
     7    CONTINUE
         ELSE
          CONTINUE
         END IF
         R(I,K) = R(I,K)-B(I,K,J)*R(I,J)
    5   CONTINUE

    2  CONTINUE

!     .....UPPER (GAUSS JORDAN) ELIMINATION IN B
       T= 1.0_REAL32/B(I,M,M)
       DO 20 KK = 1,M
        C(I,M,KK) = T*C(I,M,KK)
   20  CONTINUE

       R(I,M) = T*R(I,M)

       DO 9  J=1,M-1
        MP = M-J+1
        MPM1=MP-1
        DO 10 K=1,MPM1
         MR = MP-K
         IF ( I /= N) THEN
          DO 11 KK=1,M
!          IF ( I.EQ.N)GO TO 11
           C(I,MR,KK) = C(I,MR,KK)-B(I,MR,MP)*C(I,MP,KK)
    11    CONTINUE
         ELSE
          CONTINUE
         END IF

         R(I,MR) = R(I,MR)-B(I,MR,MP)*R(I,MP)
   10   CONTINUE
    9  CONTINUE
!     .....B(I) IS NOW THE UNIT MATRIX
!     .....ELIMINATE A(I+1)
       IF (I == N) GO TO 1
!      IF (I.EQ.N) GO TO 1
       DO 12 J=1,M
        DO 13 K=1,M
         DO 14 KK=1,M
          B(I+1,K,KK) = B(I+1,K,KK)-A(I+1,K,J)*C(I,J,KK)
   14    CONTINUE
         R(I+1,K) = R(I+1,K)-A(I+1,K,J)*R(I,J)
   13   CONTINUE
   12  CONTINUE
    1 CONTINUE

!     .....BACK SUBSTITUTION

      DO 15 K=1,M
       X(N,K) = R(N,K)
   15 CONTINUE
      DO 16 J=1,N-1
       JB=N-J
       DO 17 K=1,M
        KR = M-K+1
        S=0.0_REAL32
        DO 18 KK=1,M
         S = C(JB,KR,KK)*X(JB+1,KK)+S
   18   CONTINUE
        X(JB,KR) = R(JB,KR)-S
   17  CONTINUE
   16 CONTINUE

      RETURN

      END SUBROUTINE BLKTRI_Real32

      SUBROUTINE BLKTRI_Real64(NDIM, &
                               MDIM, &
                               M,    &
                               N,    &
                               A,    &
                               B,    &
                               C,    &
                               X,    &
                               R)

!     .......THIS PROGRAM SOLVES A TRI-BLOCK-DIAGONAL MATRIX.
!      .......NDIM = DIMENSION OF THE A,B AND C IN THE MAIN PROGRAM
!      .......MDIM = DIMENSION OF BLOCKS IN MAIN PROGRAM
!      .......M = SIZE OF BLOCKS
!      .......N = NUMBER OF BLOCKS
!      .......A,B,C = M*M MATRICES; A GOES FROM 2 TO N, B GOES
!      .......        FROM 1 TO N AND C GOES FROM 1 TO N-1.
!      .......X = RESULT VECTOR
!      .......R = RIGHT HAND SIDE VECTOR
!     C
      INTEGER, INTENT(IN) :: NDIM, &
                             MDIM
      INTEGER, INTENT(IN) :: M, N
      REAL(KIND=REAL64), DIMENSION(:,:,:), INTENT(INOUT) :: A, B, C
      REAL(KIND=REAL64), DIMENSION(:,:), INTENT(INOUT) :: X, R
!     REAL(KIND=REAL64), DIMENSION(NDIM,MDIM,MDIM), INTENT(INOUT) :: A, B, C
!     REAL(KIND=REAL64), DIMENSION(NDIM,MDIM), INTENT(INOUT) :: X, R
!     DIMENSION A(NDIM,MDIM,MDIM),B(NDIM,MDIM,MDIM),C(NDIM,MDIM,MDIM),
!    1X(NDIM,MDIM),R(NDIM,MDIM)
!
! define local variables
!
      INTEGER :: I,J,JB,K,KK,KR,MP,MPM1,MR
      REAL(KIND=REAL64) :: S, T
!
!     .....FORWARD ELIMINATION-BLOCKS

      DO 1 I=1,N

!     .....GAUSS JORDAN FOR B(I)

       DO 2 J=1,M-1
!     .....NORMALIZE PIVOT ROW

        T=1.0_REAL64/B(I,J,J)

        DO 3 K=J+1,M
         B(I,J,K) = T*B(I,J,K)
    3   CONTINUE

        IF (I < N) THEN

         DO 4 K=1,M
!        IF (I.EQ.N) GO TO 4
          C(I,J,K) = T*C(I,J,K)
     4   CONTINUE

        ELSE
         CONTINUE
        END IF

        R(I,J) = T*R(I,J)

!     .....LOWER ELIMINATION IN B

        DO 5 K=J+1,M

         DO 6 KK=J+1,M
          B(I,K,KK) = B(I,K,KK)-B(I,K,J)*B(I,J,KK)
    6    CONTINUE

         IF (I /= N) THEN
          DO 7 KK=1,M
!          IF (I.EQ.N) GO TO 7
           C(I,K,KK) = C(I,K,KK)-B(I,K,J)*C(I,J,KK)
     7    CONTINUE
         ELSE
          CONTINUE
         END IF
         R(I,K) = R(I,K)-B(I,K,J)*R(I,J)
    5   CONTINUE

    2  CONTINUE

!     .....UPPER (GAUSS JORDAN) ELIMINATION IN B
       T= 1.0_REAL64/B(I,M,M)
       DO 20 KK = 1,M
        C(I,M,KK) = T*C(I,M,KK)
   20  CONTINUE

       R(I,M) = T*R(I,M)

       DO 9  J=1,M-1
        MP = M-J+1
        MPM1=MP-1
        DO 10 K=1,MPM1
         MR = MP-K
         IF ( I /= N) THEN
          DO 11 KK=1,M
!          IF ( I.EQ.N)GO TO 11
           C(I,MR,KK) = C(I,MR,KK)-B(I,MR,MP)*C(I,MP,KK)
    11    CONTINUE
         ELSE
          CONTINUE
         END IF

         R(I,MR) = R(I,MR)-B(I,MR,MP)*R(I,MP)
   10   CONTINUE
    9  CONTINUE
!     .....B(I) IS NOW THE UNIT MATRIX
!     .....ELIMINATE A(I+1)
       IF (I == N) GO TO 1
!      IF (I.EQ.N) GO TO 1
       DO 12 J=1,M
        DO 13 K=1,M
         DO 14 KK=1,M
          B(I+1,K,KK) = B(I+1,K,KK)-A(I+1,K,J)*C(I,J,KK)
   14    CONTINUE
         R(I+1,K) = R(I+1,K)-A(I+1,K,J)*R(I,J)
   13   CONTINUE
   12  CONTINUE
    1 CONTINUE

!     .....BACK SUBSTITUTION

      DO 15 K=1,M
       X(N,K) = R(N,K)
   15 CONTINUE
      DO 16 J=1,N-1
       JB=N-J
       DO 17 K=1,M
        KR = M-K+1
        S=0.0_REAL64
        DO 18 KK=1,M
         S = C(JB,KR,KK)*X(JB+1,KK)+S
   18   CONTINUE
        X(JB,KR) = R(JB,KR)-S
   17  CONTINUE
   16 CONTINUE

      RETURN

      END SUBROUTINE BLKTRI_Real64

END MODULE BlockTridiSolver
