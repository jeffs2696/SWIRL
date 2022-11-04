        SUBROUTINE EIGEN(MM,SIGMAR,XMN,AMN,BMN,IER)
C       THIS SUBROUTINE IS USED BY V071 - V076
C
C SOFTWARE ID : V070
C
C
C AUTHOR :    UNKNOWN
C
C DATE : UNKNOWN
C
C
C DATED REVISIONS :
C
C
C  11/17/87 S. L. SKILES ADDED ERROR CHECK FOR Y BESSEL FUNCTION SET TO
C           - INFINITY IN THE CALCULATION OF THE WEIGHTING FACTORS FOR
C           THE RADIAL MODE SHAPE (AMN AND BMN) BY CALLING ERRMSG(12)
C           WHEN IER(2),IER(4),IER(6) OR IER(8) = 3
C
c  04/01/91 B.E. PRUITT PROGRAM CHANGED TO ACCEPT BESSEL FUNCTIONS AND
C           THEIR DERIVATIVES FROM THE BESJ AND BESY SUBROUTINES.
C
C  08/15/91 B.E. PRUITT REDUCED NUMBER OF ERROR INDICATORS (FEWER CALLS
C           TO BESSEL FUNCTIONS) AND PLACED IER=3 ERROR IN THIS
C           ROUTINE.
C           
C$
C -------------------------------------------------------------------
C
C PURPOSE : COMPUTE WEIGHTING FACTORS, AMN AND BMN, FOR RADIAL MODE
C           SHAPE
C
C
C FORM OF CALL:
C       CALL EIGEN(MM,SIGMAR,XMN,AMN,BMN,IER)
C
C
C ARGUMENT LIST  :
C MM - CIRCUMFERENTIAL MODE NUMBER (INPUT)
C SIGMAR - HUB RADIUS/TIP RADIUS (INPUT)
C XMN - NON-DIMENSIONAL ROOT OF THE EIGEN VALUE EQUATION (KMMU)(INPUT)
C AMN AND BMN - WEIGHTING FACTORS FOR RADIAL MODE SHAPE (OUTPUT)
C
C
C INPUT: UNKNOWN
C
C
C OUTPUT: UNKNOWN
C
C
C METHOD : REFER TO C.S. VENTRES,ET AL., "TURBOFAN NOISE GENERATION",
C          NASA-CR-167951 AND NASA-CR-167952,JULY 1982
C
C
C IDIOSYNCRASIES : UNKNOWN
C
C
C -------------------------------------------------------------------
C$
C**********************************************************************
C
        IMPLICIT REAL*8 (A - H , O - Z )
        REAL*8 BJPRIM,AMN,BMN,PART1,PART2,FACTOR,S
        DIMENSION IER(4)
        INTEGER MM,IER,N
        IF(XMN.EQ.0.) GO TO 22
        N=MM
        IF(N.LT.0) N=-N
        XMNHUB=XMN*SIGMAR
        S=SIGMAR**2
C=======================================================================     
c=>
c   
C        B E S J
        CALL BESJ (XMN,N,BJM,BJPRIM,'EIGEN ',IER(1))
 
c=======================================================================     
c=>
c   
C        B E S Y
        CALL BESY (XMN,N,BYM,BYPRIM,'EIGEN ',IER(2))
c======================================================================
c=>
c
C        B E S J
        CALL BESJ (XMNHUB,N,BJMH,BJMHPR,'EIGEN ',IER(3))
C=======================================================================   
C=>      B E S Y
C
        CALL BESY (XMNHUB,N,BYMH,BYMHPR,'EIGEN ',IER(4))
C
        IF (IER(2) .EQ. 3 .OR. IER(4) .EQ. 3)WRITE(3,100)MM
 100  FORMAT(1X,//,
     &25X,'* *  N  O  T  E  :  * *',///,
     &12X,'Y BESSEL FUNCTION SET TO MINUS MACHINE INFINITY WHILE',//,
     &12X,'CALCULATING THE RADIAL MODE SHAPE WEIGHTING FACTORS, ',
     &'AMN AND BMN FOR "M ='I4,//)
C 
C
C$$$$$$$$$$$$$$$$$$$$$$$ END OF BESJ AND BESY CALLS $$$$$$$$$$$$$$$$$$$$$$$$
C
        IF(DABS(BJPRIM).LT.DABS(BYPRIM)) GO TO 10
        AMN=-BYPRIM/BJPRIM
        BMN=1.0D+00
        GO TO 20
10      AMN=1.0D+00
        BMN=-BJPRIM/BYPRIM
20      PART1=(1.-(MM/XMN)**2)*(AMN*BJM+BMN*BYM)**2/(1.-S)
        PART2=(S-(MM/XMN)**2)*(AMN*BJMH+BMN*BYMH)**2/(1.-S)
        FACTOR=(PART1-PART2)**.5
        AMN=AMN/FACTOR
        BMN=BMN/FACTOR
        RETURN
22      CONTINUE
        AMN=1.0D+00
        BMN=0.0D+00
        DO 24 J=1,4
 24     IER(J)=0
        RETURN
        END
