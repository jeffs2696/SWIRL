MODULE FindResidualVectorModule

   USE, INTRINSIC:: ISO_FORTRAN_ENV
   USE L2NormModule
   IMPLICIT NONE
   PRIVATE
   PUBLIC:: getSvector

INTERFACE getSvector 
    MODULE PROCEDURE getSv 
END INTERFACE getSvector 


    INTEGER, PARAMETER:: rDef = REAL64
     
CONTAINS

    SUBROUTINE getSv(A, &
                      B, &
                      x, &
                      lambda, &
                      np4, &
                      S_MMS )

   INTEGER, INTENT(IN):: np4
   INTEGER:: np
       ! np = np4/4
    COMPLEX(KIND = REAL64), DIMENSION(np4, np4), INTENT(IN):: A, &
                                                            B, x
                                                            
   COMPLEX(KIND = REAL64), INTENT(IN), DIMENSION(np4):: lambda
   COMPLEX(KIND = REAL64), INTENT(OUT), DIMENSION(np4):: S_MMS
   COMPLEX(KIND = REAL64),  DIMENSION(np4):: S_MMS_A, S_MMS_B

   INTEGER:: i, j, h, Q, jj
   COMPLEX(KIND = REAL64), DIMENSION(np4, np4):: lambda_array
!   INTEGER, :: k, k1, k2, k3, j, j1, j2, j3

DO i = 1, np4
 DO j = 1, np4
    lambda_array(i, j) = 0.0_rDef
 END DO
END DO
  
DO i = 1, np4

  lambda_array(i, i) = lambda(i)

END DO

   !Eigenvalue index
    h = 1
    np = np4/4 

    S_MMS = MATMUL(A, x(:,h)) - lambda(h)*(MATMUL(B, x(:,h)))



DO i = 1, np4

!    WRITE(6, *) S_MMS(i)

END DO

    END SUBROUTINE getSv 
    
!
END MODULE FindResidualVectorModule
