MODULE zeroCrossingModule

    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: zeroCrossing

    INTERFACE zeroCrossing
        MODULE PROCEDURE zeroCrossingProcedure_real 
        MODULE PROCEDURE zeroCrossingProcedure_complex 
    END INTERFACE zeroCrossing

    INTEGER, PARAMETER :: rDef = REAL64

    INTEGER :: UNIT

CONTAINS

        SUBROUTINE zeroCrossingProcedure_real(&
                dataSet)


            REAL(KIND=REAL64), DIMENSION(:), INTENT(IN) :: dataSet
            
        END SUBROUTINE zeroCrossingProcedure_real

        SUBROUTINE zeroCrossingProcedure_complex(&
                domain, &
                dataSet)

            LOGICAL :: debug = .TRUE.
            INTEGER :: i
            REAL(KIND=REAL64), DIMENSION(:), INTENT(IN) :: domain 
            COMPLEX(KIND=REAL64), DIMENSION(:), INTENT(IN) :: dataSet 


            OPEN( NEWUNIT = UNIT, FILE = 'ZCM_result.dat')
            WRITE(UNIT,'(A24,A20,A20)') 'x', ' REAL(y)' , 'IMAG(y)'

            DO i = 1,SIZE(dataSet)
            IF (debug) THEN    
                WRITE(UNIT,*) domain(i), REAL(dataSet(i)), AIMAG(dataSet(i))
            ELSE

            ENDIF

            ENDDO
            CLOSE(UNIT)
        END SUBROUTINE zeroCrossingProcedure_complex
    
END MODULE zeroCrossingModule
