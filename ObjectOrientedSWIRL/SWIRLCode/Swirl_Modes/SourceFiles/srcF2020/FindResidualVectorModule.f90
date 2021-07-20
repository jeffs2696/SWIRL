MODULE FindResidualVectorModule

   USE, INTRINSIC:: ISO_FORTRAN_ENV
   ! USE L2NormModule
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

   COMPLEX(KIND = REAL64), DIMENSION(np4, np4), INTENT(IN):: A, &
       B

   COMPLEX(KIND = REAL64), DIMENSION(np4), INTENT(IN):: x 
                                                            
   COMPLEX(KIND = REAL64), INTENT(IN) :: lambda
   
   COMPLEX(KIND = REAL64), INTENT(OUT), DIMENSION(np4):: S_MMS

   ! Local variables
   INTEGER:: i, j, h, Q, jj

    S_MMS = MATMUL(A, x) - lambda*(MATMUL(B, x))

   DO i = 1, np4

   WRITE(6, *) S_MMS(i)

   END DO
    
!    WRITE(6,*) SUM(S_MMS)
    
    END SUBROUTINE getSv 
!
END MODULE FindResidualVectorModule
