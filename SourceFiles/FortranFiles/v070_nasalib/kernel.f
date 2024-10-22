        SUBROUTINE KERNEL(X,K,GAMMA,MR,H1,H2,ERROR,CKF)
C
C       THIS SUBROUTINE IS USED BY V071 - V076
C
C PURPOSE : CALCULATE KERNAL FUNCTION, CKF, TO BE USED IN CALCULATION OF
C           BLADE/VANE COMPLEX PRESSURE DISTRIBUTION
C
C METHOD : REFER TO EQUATIONS B-25 AND B-26 IN C.S. VENTRES,ET AL.,
C          "TURBOFAN NOISE GENERATION", NASA-CR-167951,JULY 1982
C
C PARAMETERS :
C X -
C K - REDUCED FREQUENCY (INPUT)
C GAMMA - NON-DIMENSIONAL WAVE NUMBER OF THE CONVECTED GUST (INPUT)
C MR - STREAMLINE ABSOLUTE MACH NUMBER OF THE FREESTREAM FLOW,STATOR
C      L.E (INPUT)
C H1 - NON-DIMENSIONAL STATOR VANE SPACING IN THE DIRECTION PARALLEL TO
C      THE VANE (INPUT)
C H2 - NON-DIMENSIONAL STATOR VANE SPACING IN THE DIRECTION
C      PERPENDICULAR TO THE VANE (INPUT)
C ERROR - 0.0001
C CKF - KERNAL FUNCTION VALUE (OUTPUT)
C
        IMPLICIT REAL*8 (A - H , O - Z )
        REAL*8 MR,K
        COMPLEX*16 I,CKF,AN,ANP,ANM,RN,ROOT
        RN(AN)=BETAR**2*H2/(2.*D**2)*(AN**2-K**2)*CDEXP(I*(AN+K*MR)
     1  *X)/((AN-GN*H1/D**2)*(AN+K/MR))
        DATA I/(0.0D+00,1.0D+00)/,XMIN/.001D+00/
        PI=4.0D+00 * DATAN(1.0D+00)
        BETAR=DSQRT(1.-MR**2)
        IF(DABS(X).GT.XMIN) GO TO 10
        CKF=BETAR/(2.*PI)*CDEXP(-I*BETAR**2*K*X/MR)*(1./X-I*K/MR
     1  *DLOG(DABS(X)))
        RETURN
 10     CONTINUE
        D=DSQRT(H1**2+(BETAR*H2)**2)
        R=D*SQRT(K**2+(D*DLOG(ERROR)/(BETAR*H2*X))**2)
        N1=(GAMMA-R)/(2.*PI)
        N2=(GAMMA+R)/(2.*PI)
        CKF=0.0D+00
        IF(X.GT.0.) GO TO 50
        DO 40 N=N1,N2
        GN=GAMMA-2.*PI*N
        S=(K*D)**2-GN**2
        IF(S.GE.0.) ROOT=-DSQRT(S)*DABS(K)/K
        IF(S.LT.0.) ROOT=I*DSQRT(-S)
        ANM=(GN*H1-BETAR*H2*ROOT)/D**2
        CKF=CKF+RN(ANM)
 40     CONTINUE
        RETURN
 50     CONTINUE
        DO 70 N=N1,N2
        GN=GAMMA-2.*PI*N
        S=(K*D)**2-GN**2
        IF(S.GE.0.) ROOT=-DSQRT(S)*DABS(K)/K
        IF(S.LT.0.) ROOT=I*DSQRT(-S)
        ANP=(GN*H1+BETAR*H2*ROOT)/D**2
        CKF=CKF-RN(ANP)
 70     CONTINUE
        CKF=CKF-BETAR**2*K/(2.*MR)*DSINH(BETAR**2*K
     1  *H2/MR)*CDEXP(-I*BETAR**2*K*X/MR)/(DCOSH(BETAR**2*K*H2/MR)-
     2  DCOS(GAMMA+K*H1/MR))
        RETURN
        END
