      SUBROUTINE BESJ(X,N,BJ,BJPRIM,SUBNAM,IER)
C*                                                                     *
C* SOFTWARE ID    :   BESJ                                             *
C*                                                                     *
C* AUTHOR         :  D. B. HANSON                                      *
C*                                                                     *
C* PURPOSE        :   COMPUTE J-TYPE BESSEL FUNCTIONS AND THEIR        *
C*                    DERIVATIVES OF POSITIVE OR ZERO ORDER N AND ZERO *
C*                    OR POSITIVE ARGUMENT X.                          *
C*                    MAXIMUM ORDER & ARGUMENT ARE 1000. THIS LIMIT CAN*
C*                    BE CHANGED BY INCREASING THE DIMENSION OF J( )   *
C*                    ABOVE 1030.                                      * 
C*                    RETURNS BJ=JSUBN(X) AND BJPRIM SINGLE VALUES ONLY*
C*                    NOT AN ARRAY. ROUTINE USES BACKWARD RECURSION.   *
C*                    NO INDEX SHIFT AS IN 'IMSL' ROUTINES.  THIS      * 
C*                    FUNCTION WAS WRITTEN BY  D.B. HANSON, 09/30.88.  *
C*                    WAS MODIFIED BY D.A. TOPOL AND B. E. PRUITT TO   *
C*                    CALCULATE BESSEL FUNCTION DERIVATIVE ON 4/1/91.  *
C*                                                                     *
C*                                                                     *
C* ARGUMENT LIST  :   N      - ORDER OF THE 'J' BESSEL FUNCTION (I)    *
C*                    X      - ARGUMENT OF THE 'J' BESSEL FUNCTION (I))*
C*                    BJ     - THE RESULTANT OF 'J' BESSEL FUNCTION    *
C*                             DESIRED (O)                             *
C*                    BJPRIM - THE RESULTANT OF 'J' PRIME BESSEL       *
C*                             FUNCTION DESIRED (O)                    *
C*                    SUBNAM - CALLING SUBROUTINE'S NAME. IDENTIFIES   *
C*                             WHERE ERROR IS FOR IER=1,2,3.           *
C*                    IER    - ERROR INDICATOR                         *
C3                       `     =0, NO ERROR                            *
C*                             =1, N IS NEGATIVE                       *
C*                             =2, X IS NEGATIVE                       *
C*                             =3, N OR X IS OUT OF RANGE(MAXIMUM N OR * 
C*                                 X IS 1000)                          *
C*                                                                     *
C* INPUT          :   NONE                                             *
C*                                                                     *
C* OUTPUT         :   NONE                                             *
C*                                                                     *
C* IDIOSYNCRASIES :   NONE                                             *
C*                                                                     *
C* DATED REVISIONS:   NONE                                             *
C*                                                                     *
C* --*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
      DOUBLE PRECISION X,J(0:1030),BJ,SUM,BJPRIM                         
      CHARACTER*6 SUBNAM
C* --*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
C=>                                                                     
C
      BJ=0.0D0
      BJPRIM=0.0D0
      NPLUS=N+1
c
      IF(X.eq.0.0d0) return 
      IF(X.LT.0.0) THEN
      IER=2
C
      WRITE(3,100)SUBNAM,N,X
 100  FORMAT(1X,//,
     &20X,'* *  F  A  T  A  L      E  R  R  O  R  * *',///,
     &16X,'ARGUMENT, X OF THE J BESSEL FUNCTION IS NEGATIVE',//,
     &16X,'WHILE BEING CALLED FROM SUBROUTINE ',A6,//,
     &16X,'N=',I4,' X=',D10.4,//,
     &16X,'PROGRAM WILL BE TERMINATED.',//,
     &16X,'SEE PROJECT MANAGER OR PROGRAMMER ANALYST FOR HELP',
     &//)
      STOP
C
      ELSEIF(N.LT.0) THEN
      IER=1
C
      WRITE(3,200)SUBNAM,N,X
 200  FORMAT(1X,//,
     &20X,'* *  F  A  T  A  L      E  R  R  O  R  * *',///,
     &16X,'ORDER, N OF THE J BESSEL FUNCTION IS NEGATIVE',//,
     &16X,'WHILE BEING CALLED FROM SUBROUTINE ',A6,//,
     &16X,'N=',I4,' X=',D10.4,//,
     &16X,'PROGRAM WILL BE TERMINATED.',//,
     &16X,'SEE PROJECT MANAGER OR PROGRAMMER ANALYST FOR HELP',
     &//)
      STOP
C
      ENDIF
C
C   START BACKWARD RECURSION 30 ORDERS ABOVE THE BJ ORDER DESIRED.
C
      MMAX=MAX(INT(X),NPLUS)+30
C
        IF(MMAX.GT.1028) THEN                                           
        IER=3
C
        WRITE(3,300)SUBNAM,N,X
300   FORMAT(1X,//,
     &20X,'* *  F  A  T  A  L      E  R  R  O  R  * *',///,
     &16X,'ORDER, N OR ARGUMENT, X OF THE J BESSEL FUNCTION IS OUT OF'//
     &16X,'RANGE WHILE BEING CALLED FROM SUBROUTINE ',A6,//,
     &16X,'N=',I4,' X=',D10.4,//,
     &16X,'PROGRAM WILL BE TERMINATED.',//,
     &16X,'SEE PROJECT MANAGER OR PROGRAMMER ANALYST FOR HELP',
     &//)
         STOP                                                            BES00670
        ENDIF                                                           
C
C   BACKWARD RECURSION STARTS HERE     
C             
      J(MMAX+2)=0.D0                                                    
      J(MMAX+1)=1.D-30                                                  
        DO 20 I=MMAX,0,-1                                               
        J(I)=(2*(I+1)/X) * J(I+1)-J(I+2)                                
   20 CONTINUE 
C
C   BACKWARD RECURSION ENDS HERE     
C                                                                              
      SUM=J(2)                                                          
        DO 30 I=4,MMAX,2                                                
        SUM=SUM+J(I)                                                    
   30 CONTINUE                                                          
C=>                                                                     
      SUM=J(0)+2*SUM                                                    
      BJ=J(N)/SUM 
C                                                     
C   BACKWARD RECURSION ENDS HERE     
C   IF BESSEL FUNCTION NEAR ZERO AND LESS THAN MACHINE CAN ACCURATELY
C   CALCULATE THEN SET BESSEL FUNCTION EQUAL TO ZERO.
C         
      IF(DABS(BJ).LE.1.D-35) BJ=0.0D0
C
C   CALCULATE BESSEL FUNCTION DERIVATIVE.  IF DERIVATE NEAR ZERO AND 
C   LESS THAN MACHINE CAN ACCURATELY CALCULATE THEN SET DERIVATIVE 
C   EQUAL TO ZERO.
C                                                                     
      IF(N.EQ.0) THEN
      BJPRIM=(-1.0*J(1))/SUM
C
        ELSE
      BJPRIM=0.5*(J(N-1)-J(N+1))/SUM
      IF(DABS(BJPRIM).LE.1.D-14) BJPRIM=0.0D0
C
        ENDIF
C=>
      RETURN                                                            
      END                                                               
