C modified to reach machine precision 12/20/15 by NMB

C-----------double precision routines------------------
C modified for double precision 7.9.10 by DRH
C     ALGORITHM 433 COLLECTED ALGORITHMS FROM ACM.
C     ALGORITHM APPEARED IN COMM. ACM, VOL. 15, NO. 10,
C     P. 914.
      SUBROUTINE  INTRPLD(IU,L,X,Y,N,U,V)                             ! INTR  10

C     IMPLICIT NONE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

C INTERPOLATION OF A SINGLE-VALUED FUNCTION
C THIS SUBROUTINE INTERPOLATES, FROM VALUES OF THE FUNCTION
C GIVEN AS ORDINATES OF INPUT DATA POINTS IN AN X-Y PLANE
C AND FOR A GIVEN SET OF X VALUES (ABSCISSAS), THE VALUES OF
C A SINGLE-VALUED FUNCTION Y = Y(X).
C THE INPUT PARAMETERS ARE
C     IU = LOGICAL UNIT NUMBER OF STANDARD OUTPUT UNIT
C     L  = NUMBER OF INPUT DATA POINTS
C          (MUST BE 2 OR GREATER)
C     X  = ARRAY OF DIMENSION L STORING THE X VALUES
C          (ABSCISSAS) OF INPUT DATA POINTS
C          (IN ASCENDING ORDER)
C     Y  = ARRAY OF DIMENSION L STORING THE Y VALUES
C          (ORDINATES) OF INPUT DATA POINTS
C     N  = NUMBER OF POINTS AT WHICH INTERPOLATION OF THE
C          Y VALUE (ORDINATE) IS DESIRED
C          (MUST BE 1 OR GREATER)
C     U  = ARRAY OF DIMENSION N STORING THE X VALUES
C          (ABSCISSAS) OF DESIRED POINTS
C THE OUTPUT PARAMETER IS
C     V  = ARRAY OF DIMENSION N WHERE THE INTERPOLATED Y
C          VALUES (ORDINATES) ARE TO BE DISPLAYED
C DECLARATION STATEMENTS
      DIMENSION    X(L),Y(L),U(N),V(N)
      EQUIVALENCE  (P0,X3),(Q0,Y3),(Q1,T3)
      DOUBLE PRECISION M1,M2,M3,M4,M5
      EQUIVALENCE  (UK,DX),(IMN,X2,A1,M1),(IMX,X5,A5,M5),
     1             (J,SW,SA),(Y2,W2,W4,Q2),(Y5,W3,Q3)
      DOUBLE PRECISION, PARAMETER :: epsilon = 1.0E-15_8
C PRELIMINARY PROCESSING
CC 10 L0=L
      L0=L
      LM1=L0-1
      LM2=LM1-1
      LP1=L0+1
      N0=N
      IF(LM2.LT.0)        GO TO 90
      IF(N0.LE.0)         GO TO 91
      DO 11  I=2,L0
CC      IF(X(I-1)-X(I))   11,95,96
        IF ((X(I-1)-X(I)) < 0.0d0) THEN
         GOTO 11
        ELSE IF ((X(I-1)-X(I)) > 0.0d0) THEN
         GOTO 96
        ELSE
         GOTO 95
        END IF
   11   CONTINUE
      IPV=0
C MAIN DO-LOOP
CC    DO 80  K=1,N0
      DO K=1,N0
        UK=U(K)
C ROUTINE TO LOCATE THE DESIRED POINT
CC 20   IF(LM2.EQ.0)      GO TO 27
        IF(LM2.EQ.0)      GO TO 27
        IF(UK.GE.X(L0))   GO TO 26
        IF(UK.LT.X(1))    GO TO 25
        IMN=2
        IMX=L0
   21   I=(IMN+IMX)/2
        IF(UK.GE.X(I))    GO TO 23
CC 22   IMX=I
        IMX=I
        GO TO 24
   23   IMN=I+1
   24   IF(IMX.GT.IMN)    GO TO 21
        I=IMX
        GO TO 30
   25   I=1
        GO TO 30
   26   I=LP1
        GO TO 30
   27   I=2
C CHECK IF I = IPV
   30   IF(I.EQ.IPV)      GO TO 70
        IPV=I
C ROUTINES TO PICK UP NECESSARY X AND Y VALUES AND
C          TO ESTIMATE THEM IF NECESSARY
CC 40   J=I
        J=I
        IF(J.EQ.1)        J=2
        IF(J.EQ.LP1)      J=L0
        X3=X(J-1)
        Y3=Y(J-1)
        X4=X(J)
        Y4=Y(J)
        A3=X4-X3
        M3=(Y4-Y3)/A3
        IF(LM2.EQ.0)      GO TO 43
        IF(J.EQ.2)        GO TO 41
        X2=X(J-2)
        Y2=Y(J-2)
        A2=X3-X2
        M2=(Y3-Y2)/A2
        IF(J.EQ.L0)       GO TO 42
   41   X5=X(J+1)
        Y5=Y(J+1)
        A4=X5-X4
        M4=(Y5-Y4)/A4
        IF(J.EQ.2)        M2=M3+M3-M4
        GO TO 45
   42   M4=M3+M3-M2
        GO TO 45
   43   M2=M3
        M4=M3
   45   IF(J.LE.3)        GO TO 46
        A1=X2-X(J-3)
        M1=(Y2-Y(J-3))/A1
        GO TO 47
   46   M1=M2+M2-M3
   47   IF(J.GE.LM1)      GO TO 48
        A5=X(J+2)-X5
        M5=(Y(J+2)-Y5)/A5
        GO TO 50
   48   M5=M4+M4-M3
C NUMERICAL DIFFERENTIATION
   50   IF(I.EQ.LP1)      GO TO 52
        W2=ABS(M4-M3)
        W3=ABS(M2-M1)
        SW=W2+W3
CC      IF(SW.NE.0.0d0)     GO TO 51
        IF(ABS(SW)>epsilon) GO TO 51
        W2=0.5d0
        W3=0.5d0
        SW=1.0d0
   51   T3=(W2*M2+W3*M3)/SW
        IF(I.EQ.1)        GO TO 54
   52   W3=ABS(M5-M4)
        W4=ABS(M3-M2)
        SW=W3+W4
CC      IF(SW.NE.0.0d0)     GO TO 53
        IF(ABS(SW)>epsilon) GO TO 53
        W3=0.5d0
        W4=0.5d0
        SW=1.0d0
   53   T4=(W3*M3+W4*M4)/SW
        IF(I.NE.LP1)      GO TO 60
        T3=T4
        SA=A2+A3
        T4=0.5d0*(M4+M5-A2*(A2-A3)*(M2-M3)/(SA*SA))
        X3=X4
        Y3=Y4
        A3=A2
        M3=M4
        GO TO 60
   54   T4=T3
        SA=A3+A4
        T3=0.5d0*(M1+M2-A4*(A3-A4)*(M3-M4)/(SA*SA))
        X3=X3-A4
        Y3=Y3-M2*A4
        A3=A4
        M3=M2
C DETERMINATION OF THE COEFFICIENTS
   60   Q2=(2.0d0*(M3-T3)+M3-T4)/A3
        Q3=(-M3-M3+T3+T4)/(A3*A3)
C COMPUTATION OF THE POLYNOMIAL
   70   DX=UK-P0
        V(K)=Q0+DX*(Q1+DX*(Q2+DX*Q3))
CC 80   V(K)=Q0+DX*(Q1+DX*(Q2+DX*Q3))
      END DO
      RETURN
C ERROR EXIT
   90 WRITE (IU,2090)
      GO TO 99
   91 WRITE (IU,2091)
      GO TO 99
   95 WRITE (IU,2095)
      GO TO 97
   96 WRITE (IU,2096)
   97 WRITE (IU,2097)  I,X(I)
   99 WRITE (IU,2099)  L0,N0
      RETURN
C FORMAT STATEMENTS
C2090 FORMAT(1X/22H  ***   L = 1 OR LESS./)
C2091 FORMAT(1X/22H  ***   N = 0 OR LESS./)
C2095 FORMAT(1X/27H  ***   IDENTICAL X VALUES./)
C2096 FORMAT(1X/33H  ***   X VALUES OUT OF SEQUENCE./)
C2097 FORMAT(6H   I =,I7,10X,6HX(I) =,E12.3)
C2099 FORMAT(6H   L =,I7,10X,3HN =,I7/
C    1       36H ERROR DETECTED IN ROUTINE    INTRPL)
 2090 FORMAT(1X/'  ***   L = 1 OR LESS.'/)
 2091 FORMAT(1X/'  ***   N = 0 OR LESS.'/)
 2095 FORMAT(1X/'  ***   IDENTICAL X VALUES.'/)
 2096 FORMAT(1X/'  ***   X VALUES OUT OF SEQUENCE.'/)
 2097 FORMAT('   I =',I7,10X,'X(I) =',E12.3)
 2099 FORMAT('   L =',I7,10X,'N =',I7/
     1       ' ERROR DETECTED IN ROUTINE    INTRPL')
      END
      SUBROUTINE  CRVFITD(IU,MD,L,X,Y,M,N,U,V)                         ! CRVF1670

C     IMPLICIT NONE
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

C SMOOTH CURVE FITTING
C THIS SUBROUTINE FITS A SMOOTH CURVE TO A GIVEN SET OF IN-
C PUT DATA POINTS IN AN X-Y PLANE.  IT INTERPOLATES POINTS
C IN EACH INTERVAL BETWEEN A PAIR OF DATA POINTS AND GENER-
C ATES A SET OF OUTPUT POINTS CONSISTING OF THE INPUT DATA
C POINTS AND THE INTERPOLATED POINTS.  IT CAN PROCESS EITHER
C A SINGLE-VALUED FUNCTION OR A MULTIPLE-VALUED FUNCTION.
C THE INPUT PARAMETERS ARE
C     IU = LOGICAL UNIT NUMBER OF STANDARD OUTPUT UNIT
C     MD = MODE OF THE CURVE (MUST BE 1 OR 2)
C        = 1 FOR A SINGLE-VALUED FUNCTION
C        = 2 FOR A MULTIPLE-VALUED FUNCTION
C     L  = NUMBER OF INPUT DATA POINTS
C          (MUST BE 2 OR GREATER)
C     X  = ARRAY OF DIMENSION L STORING THE ABSCISSAS OF
C          INPUT DATA POINTS (IN ASCENDING OR DESCENDING
C          ORDER FOR MD = 1)
C     Y  = ARRAY OF DIMENSION L STORING THE ORDINATES OF
C          INPUT DATA POINTS
C     M  = NUMBER OF SUBINTERVALS BETWEEN EACH PAIR OF
C          INPUT DATA POINTS (MUST BE 2 OR GREATER)
C     N  = NUMBER OF OUTPUT POINTS
C        = (L-1)*M+1
C THE OUTPUT PARAMETERS ARE
C     U  = ARRAY OF DIMENSION N WHERE THE ABSCISSAS OF
C          OUTPUT POINTS ARE TO BE DISPLAYED
C     V  = ARRAY OF DIMENSION N WHERE THE ORDINATES OF
C          OUTPUT POINTS ARE TO BE DISPLAYED
C DECLARATION STATEMENTS
      DIMENSION    X(L),Y(L),U(N),V(N)
      EQUIVALENCE  (M1,B1),(M2,B2),(M3,B3),(M4,B4),
     1             (X2,P0),(Y2,Q0),(T2,Q1)
      DOUBLE PRECISION         M1,M2,M3,M4
      EQUIVALENCE  (W2,Q2),(W3,Q3),(A1,P2),(B1,P3),
     1             (A2,DZ),(SW,R,Z)
      DOUBLE PRECISION, PARAMETER :: epsilon = 1.0E-15_8
C PRELIMINARY PROCESSING
CC 10 MD0=MD
      MD0=MD
      MDM1=MD0-1
      L0=L
      LM1=L0-1
      M0=M
      MM1=M0-1
      N0=N
      IF(MD0.LE.0)        GO TO 90
      IF(MD0.GE.3)        GO TO 90
      IF(LM1.LE.0)        GO TO 91
      IF(MM1.LE.0)        GO TO 92
      IF(N0.NE.LM1*M0+1)  GO TO 93
CC    GO TO (11,16), MD0
      IF (MD0.EQ.1) THEN
       GOTO 11
      ELSE IF (MD0.EQ.2) THEN
       GOTO 16
      ELSE
       CONTINUE
      END IF
   11 I=2
CC    IF(X(1)-X(2))       12,95,14
      IF ((X(1)-X(2)) < 0.0d0) THEN
       GOTO 12
      ELSE IF ( (X(1)-X(2))> 0.0d0) THEN
       GOTO 14
      ELSE
       GOTO 95
      END IF
   12 DO 13  I=3,L0
CC      IF(X(I-1)-X(I))   13,95,96
        IF ((X(I-1)-X(I)) < 0.0d0) THEN
         GOTO 13
        ELSE IF ((X(I-1)-X(I)) > 0.0d0) THEN
         GOTO 96
        ELSE
         GOTO 95
        END IF
   13   CONTINUE
      GO TO 18
   14 DO 15  I=3,L0
CC      IF(X(I-1)-X(I))   96,95,15
        IF ((X(I-1)-X(I)) < 0.0d0) THEN
         GOTO 96
        ELSE IF ((X(I-1)-X(I)) > 0.0d0) THEN
         GOTO 15
        ELSE
         GOTO 95
        END IF
   15   CONTINUE
      GO TO 18
   16 DO 17  I=2,L0
CC      IF(X(I-1).NE.X(I))  GO TO 17
CC      IF(Y(I-1).EQ.Y(I))  GO TO 97
        IF(ABS(X(I-1)-X(I))>epsilon) GO TO 17
        IF(ABS(Y(I-1)-Y(I))<epsilon) GO TO 97
   17   CONTINUE
   18 K=N0+M0
      I=L0+1
CC    DO 19  J=1,L0
      DO J=1,L0
        K=K-M0
        I=I-1
        U(K)=X(I)
        V(K)=Y(I)
CC 19   V(K)=Y(I)
      END DO
      RM=REAL(M0,8)
      RM=1.0d0/RM
C MAIN DO-LOOP
CC 20 K5=M0+1
      K5=M0+1
      DO 80  I=1,L0
C ROUTINES TO PICK UP NECESSARY X AND Y VALUES AND
C          TO ESTIMATE THEM IF NECESSARY
        IF(I.GT.1)        GO TO 40
CC 30   X3=U(1)
        X3=U(1)
        Y3=V(1)
        X4=U(M0+1)
        Y4=V(M0+1)
        A3=X4-X3
        B3=Y4-Y3
        IF(MDM1.EQ.0)     M3=B3/A3
        IF(L0.NE.2)       GO TO 41
        A4=A3
        B4=B3
CC 31   GO TO (33,32), MD0
   31   IF (MD0.EQ.1) THEN
         GOTO 33
        ELSE IF (MD0.EQ.2) THEN
         GOTO 32
        ELSE
         CONTINUE
        END IF
   32   A2=A3+A3-A4
        A1=A2+A2-A3
   33   B2=B3+B3-B4
        B1=B2+B2-B3
CC      GO TO (51,56), MD0
        IF (MD0.EQ.1) THEN
         GOTO 51
        ELSE IF (MD0.EQ.2) THEN
         GOTO 56
        ELSE
         CONTINUE
        END IF
   40   X2=X3
        Y2=Y3
        X3=X4
        Y3=Y4
        X4=X5
        Y4=Y5
        A1=A2
        B1=B2
        A2=A3
        B2=B3
        A3=A4
        B3=B4
        IF(I.GE.LM1)      GO TO 42
   41   K5=K5+M0
        X5=U(K5)
        Y5=V(K5)
        A4=X5-X4
        B4=Y5-Y4
        IF(MDM1.EQ.0)     M4=B4/A4
        GO TO 43
   42   IF(MDM1.NE.0)     A4=A3+A3-A2
        B4=B3+B3-B2
   43   IF(I.EQ.1)        GO TO 31
CC      GO TO (50,55), MD0
        IF (MD0.EQ.1) THEN
         GOTO 50
        ELSE IF (MD0.EQ.2) THEN
         GOTO 55
        ELSE
         CONTINUE
        END IF
C NUMERICAL DIFFERENTIATION
   50   T2=T3
   51   W2=ABS(M4-M3)
        W3=ABS(M2-M1)
        SW=W2+W3
CC      IF(SW.NE.0.0d0)     GO TO 52
        IF(ABS(SW)>epsilon) GO TO 52
        W2=0.5d0
        W3=0.5d0
        SW=1.0d0
   52   T3=(W2*M2+W3*M3)/SW
CC      IF(I-1) 80,80,60
        IF ((I-1) < 0) THEN
         GOTO 80
        ELSE IF ((I-1) > 0) THEN
         GOTO 60
        ELSE
         GOTO 80
        END IF
   55   COS2=COS3
        SIN2=SIN3
   56   W2=ABS(A3*B4-A4*B3)
        W3=ABS(A1*B2-A2*B1)
CC      IF(W2+W3.NE.0.0d0)  GO TO 57
        IF(ABS(W2+W3)>epsilon) GO TO 57
        W2=SQRT(A3*A3+B3*B3)
        W3=SQRT(A2*A2+B2*B2)
   57   COS3=W2*A2+W3*A3
        SIN3=W2*B2+W3*B3
        R=COS3*COS3+SIN3*SIN3
CC      IF(R.EQ.0.0d0)      GO TO 58
        IF(ABS(R)<epsilon)  GO TO 58
        R=SQRT(R)
        COS3=COS3/R
        SIN3=SIN3/R
CC 58   IF(I-1) 80,80,65
   58   IF ((I-1) < 0) THEN
         GOTO 80
        ELSE IF ((I-1) > 0) THEN
         GOTO 65
        ELSE
         GOTO 80
        END IF
C DETERMINATION OF THE COEFFICIENTS
   60   Q2=(2.0d0*(M2-T2)+M2-T3)/A2
        Q3=(-M2-M2+T2+T3)/(A2*A2)
        GO TO 70
   65   R=SQRT(A2*A2+B2*B2)
        P1=R*COS2
        P2=3.0d0*A2-R*(COS2+COS2+COS3)
        P3=A2-P1-P2
        Q1=R*SIN2
        Q2=3.0d0*B2-R*(SIN2+SIN2+SIN3)
        Q3=B2-Q1-Q2
        GO TO 75
C COMPUTATION OF THE POLYNOMIALS
   70   DZ=A2*RM
        Z=0.0d0
CC      DO 71  J=1,MM1
        DO J=1,MM1
          K=K+1
          Z=Z+DZ
          U(K)=P0+Z
          V(K)=Q0+Z*(Q1+Z*(Q2+Z*Q3))
CC 71     V(K)=Q0+Z*(Q1+Z*(Q2+Z*Q3))
        END DO
        GO TO 79
   75   Z=0.0d0
CC      DO 76  J=1,MM1
        DO J=1,MM1
          K=K+1
          Z=Z+RM
          U(K)=P0+Z*(P1+Z*(P2+Z*P3))
          V(K)=Q0+Z*(Q1+Z*(Q2+Z*Q3))
CC 76     V(K)=Q0+Z*(Q1+Z*(Q2+Z*Q3))
        END DO
   79   K=K+1
   80   CONTINUE
      RETURN
C ERROR EXIT
   90 WRITE (IU,2090)
      GO TO 99
   91 WRITE (IU,2091)
      GO TO 99
   92 WRITE (IU,2092)
      GO TO 99
   93 WRITE (IU,2093)
      GO TO 99
   95 WRITE (IU,2095)
      GO TO 98
   96 WRITE (IU,2096)
      GO TO 98
   97 WRITE (IU,2097)
   98 WRITE (IU,2098)  I,X(I),Y(I)
   99 WRITE (IU,2099)  MD0,L0,M0,N0
      RETURN
C FORMAT STATEMENTS
C2090 FORMAT(1X/31H  ***   MD OUT OF PROPER RANGE./)
C2091 FORMAT(1X/22H  ***   L = 1 OR LESS./)
C2092 FORMAT(1X/22H  ***   M = 1 OR LESS./)
C2093 FORMAT(1X/25H  ***   IMPROPER N VALUE./)
C2095 FORMAT(1X/27H  ***   IDENTICAL X VALUES./)
C2096 FORMAT(1X/33H  ***   X VALUES OUT OF SEQUENCE./)
C2097 FORMAT(1X/33H  ***   IDENTICAL X AND Y VALUES./)
C2098 FORMAT(7H   I  =,I4,10X,6HX(I) =,E12.3,
C    1                    10X,6HY(I) =,E12.3)
C2099 FORMAT(7H   MD =,I4,8X,3HL =,I5,8X,
C    1       3HM =,I5,8X,3HN =,I5/
C    2       36H ERROR DETECTED IN ROUTINE    CRVFIT)
 2090 FORMAT(1X/'  ***   MD OUT OF PROPER RANGE.'/)
 2091 FORMAT(1X/'  ***   L = 1 OR LESS.'/)
 2092 FORMAT(1X/'  ***   M = 1 OR LESS.'/)
 2093 FORMAT(1X/'  ***   IMPROPER N VALUE.'/)
 2095 FORMAT(1X/'  ***   IDENTICAL X VALUES.'/)
 2096 FORMAT(1X/'  ***   X VALUES OUT OF SEQUENCE.'/)
 2097 FORMAT(1X/'  ***   IDENTICAL X AND Y VALUES.'/)
 2098 FORMAT('   I  =',I4,10X,'X(I) =',E12.3,
     1                    10X,'Y(I) =',E12.3)
 2099 FORMAT('   MD =',I4,8X,'L =',I5,8X,
     1       'M =',I5,8X,'N =',I5/
     2       ' ERROR DETECTED IN ROUTINE    CRVFIT')
      END
C----------single precision routines-----------------------
C modified for double precision 7.9.10 by DRH
C     ALGORITHM 433 COLLECTED ALGORITHMS FROM ACM.
C     ALGORITHM APPEARED IN COMM. ACM, VOL. 15, NO. 10,
C     P. 914.
      SUBROUTINE  INTRPLS(IU,L,X,Y,N,U,V)                             ! INTR  10

C     IMPLICIT NONE
      IMPLICIT REAL(A-H,O-Z)

C INTERPOLATION OF A SINGLE-VALUED FUNCTION
C THIS SUBROUTINE INTERPOLATES, FROM VALUES OF THE FUNCTION
C GIVEN AS ORDINATES OF INPUT DATA POINTS IN AN X-Y PLANE
C AND FOR A GIVEN SET OF X VALUES (ABSCISSAS), THE VALUES OF
C A SINGLE-VALUED FUNCTION Y = Y(X).
C THE INPUT PARAMETERS ARE
C     IU = LOGICAL UNIT NUMBER OF STANDARD OUTPUT UNIT
C     L  = NUMBER OF INPUT DATA POINTS
C          (MUST BE 2 OR GREATER)
C     X  = ARRAY OF DIMENSION L STORING THE X VALUES
C          (ABSCISSAS) OF INPUT DATA POINTS
C          (IN ASCENDING ORDER)
C     Y  = ARRAY OF DIMENSION L STORING THE Y VALUES
C          (ORDINATES) OF INPUT DATA POINTS
C     N  = NUMBER OF POINTS AT WHICH INTERPOLATION OF THE
C          Y VALUE (ORDINATE) IS DESIRED
C          (MUST BE 1 OR GREATER)
C     U  = ARRAY OF DIMENSION N STORING THE X VALUES
C          (ABSCISSAS) OF DESIRED POINTS
C THE OUTPUT PARAMETER IS
C     V  = ARRAY OF DIMENSION N WHERE THE INTERPOLATED Y
C          VALUES (ORDINATES) ARE TO BE DISPLAYED
C DECLARATION STATEMENTS
      DIMENSION    X(L),Y(L),U(N),V(N)
      EQUIVALENCE  (P0,X3),(Q0,Y3),(Q1,T3)
      REAL         M1,M2,M3,M4,M5
      EQUIVALENCE  (UK,DX),(IMN,X2,A1,M1),(IMX,X5,A5,M5),
     1             (J,SW,SA),(Y2,W2,W4,Q2),(Y5,W3,Q3)
      REAL, PARAMETER :: epsilon = 1.0E-6
C PRELIMINARY PROCESSING
CC 10 L0=L
      L0=L
      LM1=L0-1
      LM2=LM1-1
      LP1=L0+1
      N0=N
      IF(LM2.LT.0)        GO TO 90
      IF(N0.LE.0)         GO TO 91
      DO 11  I=2,L0
CC      IF(X(I-1)-X(I))   11,95,96
        IF ((X(I-1)-X(I)) < 0.0) THEN
         GOTO 11
        ELSE IF ((X(I-1)-X(I)) > 0.0) THEN
         GOTO 96
        ELSE
         GOTO 95
        END IF
   11   CONTINUE
      IPV=0
C MAIN DO-LOOP
CC    DO 80  K=1,N0
      DO K=1,N0
        UK=U(K)
C ROUTINE TO LOCATE THE DESIRED POINT
CC 20   IF(LM2.EQ.0)      GO TO 27
        IF(LM2.EQ.0)      GO TO 27
        IF(UK.GE.X(L0))   GO TO 26
        IF(UK.LT.X(1))    GO TO 25
        IMN=2
        IMX=L0
   21   I=(IMN+IMX)/2
        IF(UK.GE.X(I))    GO TO 23
CC 22   IMX=I
        IMX=I
        GO TO 24
   23   IMN=I+1
   24   IF(IMX.GT.IMN)    GO TO 21
        I=IMX
        GO TO 30
   25   I=1
        GO TO 30
   26   I=LP1
        GO TO 30
   27   I=2
C CHECK IF I = IPV
   30   IF(I.EQ.IPV)      GO TO 70
        IPV=I
C ROUTINES TO PICK UP NECESSARY X AND Y VALUES AND
C          TO ESTIMATE THEM IF NECESSARY
CC 40   J=I
        J=I
        IF(J.EQ.1)        J=2
        IF(J.EQ.LP1)      J=L0
        X3=X(J-1)
        Y3=Y(J-1)
        X4=X(J)
        Y4=Y(J)
        A3=X4-X3
        M3=(Y4-Y3)/A3
        IF(LM2.EQ.0)      GO TO 43
        IF(J.EQ.2)        GO TO 41
        X2=X(J-2)
        Y2=Y(J-2)
        A2=X3-X2
        M2=(Y3-Y2)/A2
        IF(J.EQ.L0)       GO TO 42
   41   X5=X(J+1)
        Y5=Y(J+1)
        A4=X5-X4
        M4=(Y5-Y4)/A4
        IF(J.EQ.2)        M2=M3+M3-M4
        GO TO 45
   42   M4=M3+M3-M2
        GO TO 45
   43   M2=M3
        M4=M3
   45   IF(J.LE.3)        GO TO 46
        A1=X2-X(J-3)
        M1=(Y2-Y(J-3))/A1
        GO TO 47
   46   M1=M2+M2-M3
   47   IF(J.GE.LM1)      GO TO 48
        A5=X(J+2)-X5
        M5=(Y(J+2)-Y5)/A5
        GO TO 50
   48   M5=M4+M4-M3
C NUMERICAL DIFFERENTIATION
   50   IF(I.EQ.LP1)      GO TO 52
        W2=ABS(M4-M3)
        W3=ABS(M2-M1)
        SW=W2+W3
CC      IF(SW.NE.0.0)     GO TO 51
        IF(ABS(SW)>epsilon) GO TO 51
        W2=0.5
        W3=0.5
        SW=1.0
   51   T3=(W2*M2+W3*M3)/SW
        IF(I.EQ.1)        GO TO 54
   52   W3=ABS(M5-M4)
        W4=ABS(M3-M2)
        SW=W3+W4
CC      IF(SW.NE.0.0)     GO TO 53
        IF(ABS(SW)>epsilon) GO TO 53
        W3=0.5
        W4=0.5
        SW=1.0
   53   T4=(W3*M3+W4*M4)/SW
        IF(I.NE.LP1)      GO TO 60
        T3=T4
        SA=A2+A3
        T4=0.5*(M4+M5-A2*(A2-A3)*(M2-M3)/(SA*SA))
        X3=X4
        Y3=Y4
        A3=A2
        M3=M4
        GO TO 60
   54   T4=T3
        SA=A3+A4
        T3=0.5*(M1+M2-A4*(A3-A4)*(M3-M4)/(SA*SA))
        X3=X3-A4
        Y3=Y3-M2*A4
        A3=A4
        M3=M2
C DETERMINATION OF THE COEFFICIENTS
   60   Q2=(2.0*(M3-T3)+M3-T4)/A3
        Q3=(-M3-M3+T3+T4)/(A3*A3)
C COMPUTATION OF THE POLYNOMIAL
   70   DX=UK-P0
        V(K)=Q0+DX*(Q1+DX*(Q2+DX*Q3))
CC 80   V(K)=Q0+DX*(Q1+DX*(Q2+DX*Q3))
      END DO
      RETURN
C ERROR EXIT
   90 WRITE (IU,2090)
      GO TO 99
   91 WRITE (IU,2091)
      GO TO 99
   95 WRITE (IU,2095)
      GO TO 97
   96 WRITE (IU,2096)
   97 WRITE (IU,2097)  I,X(I)
   99 WRITE (IU,2099)  L0,N0
      RETURN
C FORMAT STATEMENTS
C2090 FORMAT(1X/22H  ***   L = 1 OR LESS./)
C2091 FORMAT(1X/22H  ***   N = 0 OR LESS./)
C2095 FORMAT(1X/27H  ***   IDENTICAL X VALUES./)
C2096 FORMAT(1X/33H  ***   X VALUES OUT OF SEQUENCE./)
C2097 FORMAT(6H   I =,I7,10X,6HX(I) =,E12.3)
C2099 FORMAT(6H   L =,I7,10X,3HN =,I7/
C    1       36H ERROR DETECTED IN ROUTINE    INTRPL)
 2090 FORMAT(1X/'  ***   L = 1 OR LESS.'/)
 2091 FORMAT(1X/'  ***   N = 0 OR LESS.'/)
 2095 FORMAT(1X/'  ***   IDENTICAL X VALUES.'/)
 2096 FORMAT(1X/'  ***   X VALUES OUT OF SEQUENCE.'/)
 2097 FORMAT('   I =',I7,10X,'X(I) =',E12.3)
 2099 FORMAT('   L =',I7,10X,'N =',I7/
     1       ' ERROR DETECTED IN ROUTINE    INTRPL')
      END
      SUBROUTINE  CRVFITS(IU,MD,L,X,Y,M,N,U,V)                         ! CRVF1670

C     IMPLICIT NONE
      IMPLICIT REAL(A-H,O-Z)

C SMOOTH CURVE FITTING
C THIS SUBROUTINE FITS A SMOOTH CURVE TO A GIVEN SET OF IN-
C PUT DATA POINTS IN AN X-Y PLANE.  IT INTERPOLATES POINTS
C IN EACH INTERVAL BETWEEN A PAIR OF DATA POINTS AND GENER-
C ATES A SET OF OUTPUT POINTS CONSISTING OF THE INPUT DATA
C POINTS AND THE INTERPOLATED POINTS.  IT CAN PROCESS EITHER
C A SINGLE-VALUED FUNCTION OR A MULTIPLE-VALUED FUNCTION.
C THE INPUT PARAMETERS ARE
C     IU = LOGICAL UNIT NUMBER OF STANDARD OUTPUT UNIT
C     MD = MODE OF THE CURVE (MUST BE 1 OR 2)
C        = 1 FOR A SINGLE-VALUED FUNCTION
C        = 2 FOR A MULTIPLE-VALUED FUNCTION
C     L  = NUMBER OF INPUT DATA POINTS
C          (MUST BE 2 OR GREATER)
C     X  = ARRAY OF DIMENSION L STORING THE ABSCISSAS OF
C          INPUT DATA POINTS (IN ASCENDING OR DESCENDING
C          ORDER FOR MD = 1)
C     Y  = ARRAY OF DIMENSION L STORING THE ORDINATES OF
C          INPUT DATA POINTS
C     M  = NUMBER OF SUBINTERVALS BETWEEN EACH PAIR OF
C          INPUT DATA POINTS (MUST BE 2 OR GREATER)
C     N  = NUMBER OF OUTPUT POINTS
C        = (L-1)*M+1
C THE OUTPUT PARAMETERS ARE
C     U  = ARRAY OF DIMENSION N WHERE THE ABSCISSAS OF
C          OUTPUT POINTS ARE TO BE DISPLAYED
C     V  = ARRAY OF DIMENSION N WHERE THE ORDINATES OF
C          OUTPUT POINTS ARE TO BE DISPLAYED
C DECLARATION STATEMENTS
      DIMENSION    X(L),Y(L),U(N),V(N)
      EQUIVALENCE  (M1,B1),(M2,B2),(M3,B3),(M4,B4),
     1             (X2,P0),(Y2,Q0),(T2,Q1)
      REAL         M1,M2,M3,M4
      EQUIVALENCE  (W2,Q2),(W3,Q3),(A1,P2),(B1,P3),
     1             (A2,DZ),(SW,R,Z)
      REAL, PARAMETER :: epsilon = 1.0E-6
C PRELIMINARY PROCESSING
CC 10 MD0=MD
      MD0=MD
      MDM1=MD0-1
      L0=L
      LM1=L0-1
      M0=M
      MM1=M0-1
      N0=N
      IF(MD0.LE.0)        GO TO 90
      IF(MD0.GE.3)        GO TO 90
      IF(LM1.LE.0)        GO TO 91
      IF(MM1.LE.0)        GO TO 92
      IF(N0.NE.LM1*M0+1)  GO TO 93
CC    GO TO (11,16), MD0
      IF (MD0.EQ.1) THEN
       GOTO 11
      ELSE IF (MD0.EQ.2) THEN
       GOTO 16
      ELSE
       CONTINUE
      END IF
   11 I=2
CC    IF(X(1)-X(2))       12,95,14
      IF ((X(1)-X(2)) < 0.0) THEN
       GOTO 12
      ELSE IF ( (X(1)-X(2))> 0.0) THEN
       GOTO 14
      ELSE
       GOTO 95
      END IF
   12 DO 13  I=3,L0
CC      IF(X(I-1)-X(I))   13,95,96
        IF ((X(I-1)-X(I)) < 0.0) THEN
         GOTO 13
        ELSE IF ((X(I-1)-X(I)) > 0.0) THEN
         GOTO 96
        ELSE
         GOTO 95
        END IF
   13   CONTINUE
      GO TO 18
   14 DO 15  I=3,L0
CC      IF(X(I-1)-X(I))   96,95,15
        IF ((X(I-1)-X(I)) < 0.0) THEN
         GOTO 96
        ELSE IF ((X(I-1)-X(I)) > 0.0) THEN
         GOTO 15
        ELSE
         GOTO 95
        END IF
   15   CONTINUE
      GO TO 18
   16 DO 17  I=2,L0
CC      IF(X(I-1).NE.X(I))  GO TO 17
CC      IF(Y(I-1).EQ.Y(I))  GO TO 97
        IF(ABS(X(I-1)-X(I))>epsilon) GO TO 17
        IF(ABS(Y(I-1)-Y(I))<epsilon) GO TO 97
   17   CONTINUE
   18 K=N0+M0
      I=L0+1
CC    DO 19  J=1,L0
      DO J=1,L0
        K=K-M0
        I=I-1
        U(K)=X(I)
        V(K)=Y(I)
CC 19   V(K)=Y(I)
      END DO
      RM=REAL(M0,4)
      RM=1.0/RM
C MAIN DO-LOOP
CC 20 K5=M0+1
      K5=M0+1
      DO 80  I=1,L0
C ROUTINES TO PICK UP NECESSARY X AND Y VALUES AND
C          TO ESTIMATE THEM IF NECESSARY
        IF(I.GT.1)        GO TO 40
CC 30   X3=U(1)
        X3=U(1)
        Y3=V(1)
        X4=U(M0+1)
        Y4=V(M0+1)
        A3=X4-X3
        B3=Y4-Y3
        IF(MDM1.EQ.0)     M3=B3/A3
        IF(L0.NE.2)       GO TO 41
        A4=A3
        B4=B3
CC 31   GO TO (33,32), MD0
   31   IF (MD0.EQ.1) THEN
         GOTO 33
        ELSE IF (MD0.EQ.2) THEN
         GOTO 32
        ELSE
         CONTINUE
        END IF
   32   A2=A3+A3-A4
        A1=A2+A2-A3
   33   B2=B3+B3-B4
        B1=B2+B2-B3
CC      GO TO (51,56), MD0
        IF (MD0.EQ.1) THEN
         GOTO 51
        ELSE IF (MD0.EQ.2) THEN
         GOTO 56
        ELSE
         CONTINUE
        END IF
   40   X2=X3
        Y2=Y3
        X3=X4
        Y3=Y4
        X4=X5
        Y4=Y5
        A1=A2
        B1=B2
        A2=A3
        B2=B3
        A3=A4
        B3=B4
        IF(I.GE.LM1)      GO TO 42
   41   K5=K5+M0
        X5=U(K5)
        Y5=V(K5)
        A4=X5-X4
        B4=Y5-Y4
        IF(MDM1.EQ.0)     M4=B4/A4
        GO TO 43
   42   IF(MDM1.NE.0)     A4=A3+A3-A2
        B4=B3+B3-B2
   43   IF(I.EQ.1)        GO TO 31
CC      GO TO (50,55), MD0
        IF (MD0.EQ.1) THEN
         GOTO 50
        ELSE IF (MD0.EQ.2) THEN
         GOTO 55
        ELSE
         CONTINUE
        END IF
C NUMERICAL DIFFERENTIATION
   50   T2=T3
   51   W2=ABS(M4-M3)
        W3=ABS(M2-M1)
        SW=W2+W3
CC      IF(SW.NE.0.0)     GO TO 52
        IF(ABS(SW)>epsilon) GO TO 52
        W2=0.5
        W3=0.5
        SW=1.0
   52   T3=(W2*M2+W3*M3)/SW
CC      IF(I-1) 80,80,60
        IF ((I-1) < 0) THEN
         GOTO 80
        ELSE IF ((I-1) > 0) THEN
         GOTO 60
        ELSE
         GOTO 80
        END IF
   55   COS2=COS3
        SIN2=SIN3
   56   W2=ABS(A3*B4-A4*B3)
        W3=ABS(A1*B2-A2*B1)
CC      IF(W2+W3.NE.0.0)  GO TO 57
        IF(ABS(W2+W3)>epsilon) GO TO 57
        W2=SQRT(A3*A3+B3*B3)
        W3=SQRT(A2*A2+B2*B2)
   57   COS3=W2*A2+W3*A3
        SIN3=W2*B2+W3*B3
        R=COS3*COS3+SIN3*SIN3
CC      IF(R.EQ.0.0)      GO TO 58
        IF(ABS(R)<epsilon)  GO TO 58
        R=SQRT(R)
        COS3=COS3/R
        SIN3=SIN3/R
CC 58   IF(I-1) 80,80,65
   58   IF ((I-1) < 0) THEN
         GOTO 80
        ELSE IF ((I-1) > 0) THEN
         GOTO 65
        ELSE
         GOTO 80
        END IF
C DETERMINATION OF THE COEFFICIENTS
   60   Q2=(2.0*(M2-T2)+M2-T3)/A2
        Q3=(-M2-M2+T2+T3)/(A2*A2)
        GO TO 70
   65   R=SQRT(A2*A2+B2*B2)
        P1=R*COS2
        P2=3.0*A2-R*(COS2+COS2+COS3)
        P3=A2-P1-P2
        Q1=R*SIN2
        Q2=3.0*B2-R*(SIN2+SIN2+SIN3)
        Q3=B2-Q1-Q2
        GO TO 75
C COMPUTATION OF THE POLYNOMIALS
   70   DZ=A2*RM
        Z=0.0
CC      DO 71  J=1,MM1
        DO J=1,MM1
          K=K+1
          Z=Z+DZ
          U(K)=P0+Z
          V(K)=Q0+Z*(Q1+Z*(Q2+Z*Q3))
CC 71     V(K)=Q0+Z*(Q1+Z*(Q2+Z*Q3))
        END DO
        GO TO 79
   75   Z=0.0
CC      DO 76  J=1,MM1
        DO J=1,MM1
          K=K+1
          Z=Z+RM
          U(K)=P0+Z*(P1+Z*(P2+Z*P3))
          V(K)=Q0+Z*(Q1+Z*(Q2+Z*Q3))
CC 76     V(K)=Q0+Z*(Q1+Z*(Q2+Z*Q3))
        END DO
   79   K=K+1
   80   CONTINUE
      RETURN
C ERROR EXIT
   90 WRITE (IU,2090)
      GO TO 99
   91 WRITE (IU,2091)
      GO TO 99
   92 WRITE (IU,2092)
      GO TO 99
   93 WRITE (IU,2093)
      GO TO 99
   95 WRITE (IU,2095)
      GO TO 98
   96 WRITE (IU,2096)
      GO TO 98
   97 WRITE (IU,2097)
   98 WRITE (IU,2098)  I,X(I),Y(I)
   99 WRITE (IU,2099)  MD0,L0,M0,N0
      RETURN
C FORMAT STATEMENTS
C2090 FORMAT(1X/31H  ***   MD OUT OF PROPER RANGE./)
C2091 FORMAT(1X/22H  ***   L = 1 OR LESS./)
C2092 FORMAT(1X/22H  ***   M = 1 OR LESS./)
C2093 FORMAT(1X/25H  ***   IMPROPER N VALUE./)
C2095 FORMAT(1X/27H  ***   IDENTICAL X VALUES./)
C2096 FORMAT(1X/33H  ***   X VALUES OUT OF SEQUENCE./)
C2097 FORMAT(1X/33H  ***   IDENTICAL X AND Y VALUES./)
C2098 FORMAT(7H   I  =,I4,10X,6HX(I) =,E12.3,
C    1                    10X,6HY(I) =,E12.3)
C2099 FORMAT(7H   MD =,I4,8X,3HL =,I5,8X,
C    1       3HM =,I5,8X,3HN =,I5/
C    2       36H ERROR DETECTED IN ROUTINE    CRVFIT)
 2090 FORMAT(1X/'  ***   MD OUT OF PROPER RANGE.'/)
 2091 FORMAT(1X/'  ***   L = 1 OR LESS.'/)
 2092 FORMAT(1X/'  ***   M = 1 OR LESS.'/)
 2093 FORMAT(1X/'  ***   IMPROPER N VALUE.'/)
 2095 FORMAT(1X/'  ***   IDENTICAL X VALUES.'/)
 2096 FORMAT(1X/'  ***   X VALUES OUT OF SEQUENCE.'/)
 2097 FORMAT(1X/'  ***   IDENTICAL X AND Y VALUES.'/)
 2098 FORMAT('   I  =',I4,10X,'X(I) =',E12.3,
     1                    10X,'Y(I) =',E12.3)
 2099 FORMAT('   MD =',I4,8X,'L =',I5,8X,
     1       'M =',I5,8X,'N =',I5/
     2       ' ERROR DETECTED IN ROUTINE    CRVFIT')
      END
