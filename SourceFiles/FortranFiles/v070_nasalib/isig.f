        FUNCTION ISIG(X)
        REAL*8 X
C
C CALLED BY : ANFU
C
          ISIG = 1
          IF (X .LT. 0.D0) ISIG = -1
        RETURN
        END
