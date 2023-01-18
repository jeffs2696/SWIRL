      FUNCTION FAC(N)
      IMPLICIT REAL*8(A-H,O-Z)

      IF(N.GT.1) GO TO 1
      FAC=1.D0
      RETURN
 1    FAC=1.D0
      DO 2 j=1,N
      FAC=FAC*j
 2    CONTINUE
      RETURN
      END

