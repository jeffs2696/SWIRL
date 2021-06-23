MODULE F90_ZGGEV

USE, INTRINSIC :: ISO_FORTRAN_ENV
IMPLICIT NONE
PRIVATE
PUBLIC :: USE_EIGENSOLVER 

INTERFACE USE_EIGENSOLVER 
    MODULE PROCEDURE F77_ZGGEV
END INTERFACE

INTEGER,PARAMETER :: rDef = REAL64

CONTAINS

SUBROUTINE F77_ZGGEV(&
   JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA, &
   VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO ) 


! ZGGEV variables 
CHARACTER, INTENT(IN) :: &
    JOBVL , &
    JOBVR

INTEGER, INTENT(IN) :: &
    INFO , &
    LDA  , &
    LDB  , & 
    LDVL , &
    LDVR , &
    LWORK, &
    N

!     .. Array Arguments ..
REAL(KIND=rDef), DIMENSION(:) , INTENT(IN) :: &
    RWORK

COMPLEX(KIND=rDef) , DIMENSION(:) , INTENT(IN) :: &
    ALPHA , &
    BETA  , &
    WORK

COMPLEX(KIND=rDef) , DIMENSION(:,:) , INTENT(IN) :: & 
    A , &
    B     , &
    VL    , &
    VR    



!    A( LDA, N ), ALPHA( N ), B( LDB, B ), &
!    BETA( N ), VL( LDVL, N ), VR( LDVR, N  ), &
!    WORK( N )


        CALL ZGGEV(&
            JOBVL,   & ! JOBVL
            JOBVR,   & ! JOBVR
            N,     & ! N
            A,      & ! A
            LDA,   & ! LDA
            B,      & ! B
            LDB,   & ! LDB
            ALPHA,   & ! ALPHA
            BETA,    & ! BETA
            VL,      & ! VL
            LDVL,   & ! LDVL
            VR,      & ! VR
            LDVR,   & ! LDVR
            WORK,    & ! WORK
            LWORK, & ! LWORK
            RWORK,   & ! RWORK
            INFO )     ! INFO
WRITE(6,*) INFO
END SUBROUTINE F77_ZGGEV
END MODULE F90_ZGGEV
