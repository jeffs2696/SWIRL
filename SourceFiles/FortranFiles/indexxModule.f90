MODULE indexxModule
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: indexx

INTERFACE indexx
  MODULE PROCEDURE indexx1
END INTERFACE indexx

   INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      SUBROUTINE INDEXX1(N,ARRIN,INDX)

      INTEGER, INTENT(IN) :: n
      REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: arrin
      INTEGER, DIMENSION(:), INTENT(OUT) :: indx

! local variables

      INTEGER :: j, &
             indxt, &
                 l, &
                ir, &
                 i

      REAL(KIND=rDef) :: q

!     DIMENSION ARRIN(N),INDX(N)

      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      IF(N.EQ.1)RETURN
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END
END MODULE indexxModule
