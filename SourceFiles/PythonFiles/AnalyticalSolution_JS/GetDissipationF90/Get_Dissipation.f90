MODULE GetDissipationModule
!! Working TestCase

    USE,  INTRINSIC:: ISO_FORTRAN_ENV
    
    IMPLICIT NONE
    PRIVATE


      
    PUBLIC:: Dissipation_RHS

    INTERFACE Dissipation_RHS
      MODULE PROCEDURE Dissipation_RHS1
    END INTERFACE Dissipation_RHS

    INTEGER, PARAMETER:: rDef = REAL64

    CONTAINS
         
    SUBROUTINE Dissipation_RHS1( iMin        ,&
                                iMax        ,&
                                bMin        ,&
                                bMax        ,&
                                delta_X     ,&
                                delta_tau   ,&
                                nD          ,&
                                Dis         ,&
                                Q_n         ,&
                                Jac_Curv)
                                        
    REAL(KIND = rDef), DIMENSION(:, :)  , INTENT(INOUT) ::    Q_n 
    REAL(KIND = rDef), DIMENSION(:, :)  , INTENT(INOUT):: Dis
    REAL(KIND = rDef), DIMENSION(:)     , INTENT(IN):: Jac_Curv

    REAL(KIND = rDef), INTENT(IN):: delta_x    ,&
                                    delta_tau

    INTEGER, INTENT(IN)::   iMin    ,&
                            iMax    ,&
                            bMax    ,&
                            bMin    ,&
                            nD

    REAL(KIND = rDef), DIMENSION(iMax, bMax)::  D2      ,&
                                                D10

    REAL(KIND = rDef), DIMENSION(iMax)::    Density        ,&
                                            Tilde_S         ,&
                                            S               ,&
                                            DeltaS          ,&
                                            DeltaS_current  ,&
                                            epsi

    REAL(KIND = rDef)  ::   KCFac           ,&
                            gam             ,&
                            DeltaS_tot      ,&
                            S_Max           ,&
                            S_Min

    INTEGER:: i, j

    DO j = bMin, bMax
        DO i = iMin, iMax
            Dis(i, j)   = 0.0_rDef 
            D2(i, j)    = 0.0_rDef
            D10(i, j)   = 0.0_rDef
        END DO
    END DO

    DO i = iMin, iMax
        Density(i)          = 0.0_rDef
        S(i)                = 0.0_rDef
        DeltaS(i)           = 0.0_rDef
        epsi(i)             = 0.0_rDef
        Tilde_S(i)          = 0.0_rDef
        DeltaS_current(i)   = 0.0_rDef
    END DO

    !DO i = iMin, iMax
    !    DO j = bMin, bMax 
    !        Q_n(i, j) = Q_n(i, j)*Jac_Curv(i)
    !    END DO
    !END DO

    IF (nD == 1) THEN  ! Second Order Dissipation, D2
        KCFac = 1.0_rDef/(4.0_rDef)
        DO j = bMin, bMax
            DO i = iMin+1, iMax-1
                Dis(i, j) = KCFac*(1.0_rDef*Q_n(i-1, j)     & 
                                -  2.0_rDef*Q_n(i, j)         &
                                +  1.0_rDef*Q_n(i+1, j))
            END DO
        END DO
        DO j = bMin, bMax
            i = iMin
            Dis(i, j) = KCFac*(  -1.0_rDef*Q_n(i, j)        &
                                + 1.0_rDef*Q_n(i+1, j))
            i = iMax
            Dis(i, j) = KCFac*(  -1.0_rDef*Q_n(i, j)        &
                                + 1.0_rDef*Q_n(i-1, j))
        END DO
    ELSE IF (nD == 2) THEN  !Fourth Order Dissipation, D4
        KCFac = 1.0_rDef/(16.0_rDef)
        DO j = bMin, bMax
            DO i = iMin+2, iMax-2
                Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i-2, j)    & 
                                 +  4.0_rDef*Q_n(i-1, j)    &
                                 -  6.0_rDef*Q_n(i    , j)    &
                                 +  4.0_rDef*Q_n(i+1, j)    &
                                 -  1.0_rDef*Q_n(i+2, j))
            END DO
        END DO
        DO j = bMin, bMax
            
            i = iMin
            Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
                             +  2.0_rDef*Q_n(i+1, j)    &
                             -  1.0_rDef*Q_n(i+2, j))
            
            i = iMin+1
            Dis(i, j) = KCFac*( 2.0_rDef*Q_n(i-1, j)    & 
                             -  5.0_rDef*Q_n(i    , j)    &
                             +  4.0_rDef*Q_n(i+1, j)    &
                             -  1.0_rDef*Q_n(i+2, j))
            
            i = iMax
            Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
                             +  2.0_rDef*Q_n(i-1, j)    &
                             -  1.0_rDef*Q_n(i-2, j))
            
            i = iMax-1
            Dis(i, j) = KCFac*( 2.0_rDef*Q_n(i+1, j)    & 
                             -  5.0_rDef*Q_n(i    , j)    &
                             +  4.0_rDef*Q_n(i-1, j)    &
                             -  1.0_rDef*Q_n(i-2, j))
       END DO
    
    ELSE IF (nD == 3) THEN  !Sixth Order Dissipation, D6
        KCFac = 1.0_rDef/(64.0_rDef)
        DO j = bMin, bMax
            DO i = iMin+3, iMax-3
                Dis(i, j) = KCFac*( 1.0_rDef*Q_n(i-3, j)    & 
                                 -  6.0_rDef*Q_n(i-2, j)    &
                                 + 15.0_rDef*Q_n(i-1, j)    &
                                 - 20.0_rDef*Q_n(i    , j)    &
                                 + 15.0_rDef*Q_n(i+1, j)    &
                                 -  6.0_rDef*Q_n(i+2, j)    &
                                 +  1.0_rDef*Q_n(i+3, j))
            END DO
        END DO
        DO j = bMin, bMax
            
            i = iMin
            Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
                             +  3.0_rDef*Q_n(i+1, j)    &
                             -  3.0_rDef*Q_n(i+2, j)    &
                             +  1.0_rDef*Q_n(i+3, j))
            
            i = iMin+1
            Dis(i, j) = KCFac*( 3.0_rDef*Q_n(i-1, j)    & 
                             - 10.0_rDef*Q_n(i    , j)    &
                             + 12.0_rDef*Q_n(i+1, j)    &
                             -  6.0_rDef*Q_n(i+2, j)    &
                             +  1.0_rDef*Q_n(i+3, j))
            
            i = iMin+2
            Dis(i, j) = KCFac*(-3.0_rDef*Q_n(i-2, j)    & 
                             + 12.0_rDef*Q_n(i-1, j)    &
                             - 19.0_rDef*Q_n(i    , j)    &
                             + 15.0_rDef*Q_n(i+1, j)    &
                             -  6.0_rDef*Q_n(i+2, j)    &
                             +  1.0_rDef*Q_n(i+3, j))
            
            i = iMax
            Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
                             +  3.0_rDef*Q_n(i-1, j)    &
                             -  3.0_rDef*Q_n(i-2, j)    &
                             +  1.0_rDef*Q_n(i-3, j))
            
            i = iMax-1
            Dis(i, j) = KCFac*( 3.0_rDef*Q_n(i+1, j)    & 
                             - 10.0_rDef*Q_n(i    , j)    &
                             + 12.0_rDef*Q_n(i-1, j)    &
                             -  6.0_rDef*Q_n(i-2, j)    &
                             +  1.0_rDef*Q_n(i-3, j))

            i = iMax-2
            Dis(i, j) = KCFac*(-3.0_rDef*Q_n(i+2, j)    & 
                             + 12.0_rDef*Q_n(i+1, j)    &
                             - 19.0_rDef*Q_n(i    , j)    &
                             + 15.0_rDef*Q_n(i-1, j)    &
                             -  6.0_rDef*Q_n(i-2, j)    &
                             +  1.0_rDef*Q_n(i-3, j))
       END DO
    ELSE IF (nD == 4) THEN  !Eighteth Order Dissipation, D8
        KCFac = 1.0_rDef/(256.0_rDef)
        DO j = bMin, bMax
            DO i = iMin+4, iMax-4
                Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i-4, j)    & 
                                 +  8.0_rDef*Q_n(i-3, j)    &
                                 - 28.0_rDef*Q_n(i-2, j)    &
                                 + 56.0_rDef*Q_n(i-1, j)    &
                                 - 70.0_rDef*Q_n(i    , j)    &
                                 + 56.0_rDef*Q_n(i+1, j)    &
                                 - 28.0_rDef*Q_n(i+2, j)    &
                                 +  8.0_rDef*Q_n(i+3, j)    &
                                 -  1.0_rDef*Q_n(i+4, j))
            END DO
        END DO
        DO j = bMin, bMax
            i = iMin
            Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
                             +  4.0_rDef*Q_n(i+1, j)    &
                             -  6.0_rDef*Q_n(i+2, j)    &
                             +  4.0_rDef*Q_n(i+3, j)    &
                             -  1.0_rDef*Q_n(i+4, j))

            i = iMin+1
            Dis(i, j) = KCFac*( 4.0_rDef*Q_n(i-1, j)    & 
                             - 17.0_rDef*Q_n(i    , j)    &
                             + 28.0_rDef*Q_n(i+1, j)    &
                             - 22.0_rDef*Q_n(i+2, j)    &
                             +  8.0_rDef*Q_n(i+3, j)    &
                             -  1.0_rDef*Q_n(i+3, j))

            i = iMin+2
            Dis(i, j) = KCFac*(-6.0_rDef*Q_n(i-2, j)    & 
                             + 28.0_rDef*Q_n(i-1, j)    &
                             - 53.0_rDef*Q_n(i    , j)    &
                             + 52.0_rDef*Q_n(i+1, j)    &
                             - 28.0_rDef*Q_n(i+2, j)    &
                             +  8.0_rDef*Q_n(i+3, j)    &
                             -  1.0_rDef*Q_n(i+4, j))

            i = iMin+3
            Dis(i, j) = KCFac*( 4.0_rDef*Q_n(i-3, j)    & 
                             - 22.0_rDef*Q_n(i-2, j)    &
                             + 52.0_rDef*Q_n(i-1, j)    &
                             - 69.0_rDef*Q_n(i    , j)    &
                             + 56.0_rDef*Q_n(i+1, j)    &
                             - 28.0_rDef*Q_n(i+2, j)    &
                             +  8.0_rDef*Q_n(i+3, j)    &
                             -  1.0_rDef*Q_n(i+4, j))

            i = iMax
            Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
                             +  4.0_rDef*Q_n(i-1, j)    &
                             -  6.0_rDef*Q_n(i-2, j)    &
                             +  4.0_rDef*Q_n(i-3, j)    &
                             -  1.0_rDef*Q_n(i-4, j))

            i = iMax-1
            Dis(i, j) = KCFac*( 4.0_rDef*Q_n(i+1, j)    & 
                             - 17.0_rDef*Q_n(i    , j)    &
                             + 28.0_rDef*Q_n(i-1, j)    &
                             - 22.0_rDef*Q_n(i-2, j)    &
                             +  8.0_rDef*Q_n(i-3, j)    &
                             -  1.0_rDef*Q_n(i-4, j))

            i = iMax-2
            Dis(i, j) = KCFac*(-6.0_rDef*Q_n(i+2, j)    & 
                             + 28.0_rDef*Q_n(i+1, j)    &
                             - 53.0_rDef*Q_n(i    , j)    &
                             + 52.0_rDef*Q_n(i-1, j)    &
                             - 28.0_rDef*Q_n(i-2, j)    &
                             +  8.0_rDef*Q_n(i-3, j)    &
                             -  1.0_rDef*Q_n(i-4, j))

            i = iMax-3
            Dis(i, j) = KCFac*( 4.0_rDef*Q_n(i+3, j)    & 
                             - 22.0_rDef*Q_n(i+2, j)    &
                             + 52.0_rDef*Q_n(i+1, j)    &
                             - 69.0_rDef*Q_n(i    , j)    &
                             + 56.0_rDef*Q_n(i-1, j)    &
                             - 28.0_rDef*Q_n(i-2, j)    &
                             +  8.0_rDef*Q_n(i-3, j)    &
                             -  1.0_rDef*Q_n(i-4, j))
       END DO
    ELSE IF (nD == 5) THEN  !Tenth Order Dissipation, D10
        KCFac = 1.0_rDef/(1024.0_rDef*delta_tau)
        DO j = bMin, bMax
            DO i = iMin+5, iMax-5
                Dis(i, j) = KCFac*(  1.0_rDef*Q_n(i-5, j)    & 
                                 -  10.0_rDef*Q_n(i-4, j)    &
                                 +  45.0_rDef*Q_n(i-3, j)    &
                                 - 120.0_rDef*Q_n(i-2, j)    &
                                 + 210.0_rDef*Q_n(i-1, j)    &
                                 - 252.0_rDef*Q_n(i    , j)    &
                                 + 210.0_rDef*Q_n(i+1, j)    &
                                 - 120.0_rDef*Q_n(i+2, j)    &
                                 +  45.0_rDef*Q_n(i+3, j)    &
                                 -  10.0_rDef*Q_n(i+4, j)    &
                                 +   1.0_rDef*Q_n(i+5, j))
            END DO
        END DO
        DO j = bMin, bMax
            i = iMin
            Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
                             +  5.0_rDef*Q_n(i+1, j)    &
                             - 10.0_rDef*Q_n(i+2, j)    &
                             + 10.0_rDef*Q_n(i+3, j)    &
                             -  5.0_rDef*Q_n(i+4, j)    &
                             +  1.0_rDef*Q_n(i+5, j))

            i = iMin+1
            Dis(i, j) = KCFac*( 5.0_rDef*Q_n(i-1, j)    & 
                             - 26.0_rDef*Q_n(i    , j)    &
                             + 55.0_rDef*Q_n(i+1, j)    &
                             - 60.0_rDef*Q_n(i+2, j)    &
                             + 35.0_rDef*Q_n(i+3, j)    &
                             - 10.0_rDef*Q_n(i+4, j)    &
                             +  1.0_rDef*Q_n(i+5, j))

            i = iMin+2
            Dis(i, j) = KCFac*(-10.0_rDef*Q_n(i-2, j)    & 
                             +  55.0_rDef*Q_n(i-1, j)    &
                             - 126.0_rDef*Q_n(i    , j)    &
                             + 155.0_rDef*Q_n(i+1, j)    &
                             - 110.0_rDef*Q_n(i+2, j)    &
                             +  45.0_rDef*Q_n(i+3, j)    &
                             -  10.0_rDef*Q_n(i+4, j)    &
                             +   1.0_rDef*Q_n(i+5, j))

            i = iMin+3
            Dis(i, j) = KCFac*( 10.0_rDef*Q_n(i-3, j)    & 
                             -  60.0_rDef*Q_n(i-2, j)    &
                             + 155.0_rDef*Q_n(i-1, j)    &
                             - 226.0_rDef*Q_n(i    , j)    &
                             + 205.0_rDef*Q_n(i+1, j)    &
                             - 120.0_rDef*Q_n(i+2, j)    &
                             +  45.0_rDef*Q_n(i+3, j)    &
                             -  10.0_rDef*Q_n(i+4, j)    &
                             +   1.0_rDef*Q_n(i+5, j))

            i = iMin+4
            Dis(i, j) = KCFac*( -5.0_rDef*Q_n(i-4, j)    & 
                             +  35.0_rDef*Q_n(i-3, j)    &
                             - 110.0_rDef*Q_n(i-2, j)    &
                             + 205.0_rDef*Q_n(i-1, j)    &
                             - 251.0_rDef*Q_n(i    , j)    &
                             + 210.0_rDef*Q_n(i+1, j)    &
                             - 120.0_rDef*Q_n(i+2, j)    &
                             +  45.0_rDef*Q_n(i+3, j)    &
                             -  10.0_rDef*Q_n(i+4, j)    &
                             +   1.0_rDef*Q_n(i+5, j))
            
            i = iMax
            Dis(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
                             +  5.0_rDef*Q_n(i-1, j)    &
                             - 10.0_rDef*Q_n(i-2, j)    &
                             + 10.0_rDef*Q_n(i-3, j)    &
                             -  5.0_rDef*Q_n(i-4, j)    &
                             +  1.0_rDef*Q_n(i-5, j))

            i = iMax-1
            Dis(i, j) = KCFac*( 5.0_rDef*Q_n(i+1, j)    & 
                             - 26.0_rDef*Q_n(i    , j)    &
                             + 55.0_rDef*Q_n(i-1, j)    &
                             - 60.0_rDef*Q_n(i-2, j)    &
                             + 35.0_rDef*Q_n(i-3, j)    &
                             - 10.0_rDef*Q_n(i-4, j)    &
                             +  1.0_rDef*Q_n(i-5, j))

            i = iMax-2
            Dis(i, j) = KCFac*(-10.0_rDef*Q_n(i+2, j)    & 
                             +  55.0_rDef*Q_n(i+1, j)    &
                             - 126.0_rDef*Q_n(i    , j)    &
                             + 155.0_rDef*Q_n(i-1, j)    &
                             - 110.0_rDef*Q_n(i-2, j)    &
                             +  45.0_rDef*Q_n(i-3, j)    &
                             -  10.0_rDef*Q_n(i-4, j)    &
                             +   1.0_rDef*Q_n(i-5, j))

            i = iMax-3
            Dis(i, j) = KCFac*( 10.0_rDef*Q_n(i+3, j)    & 
                             -  60.0_rDef*Q_n(i+2, j)    &
                             + 155.0_rDef*Q_n(i+1, j)    &
                             - 226.0_rDef*Q_n(i    , j)    &
                             + 205.0_rDef*Q_n(i-1, j)    &
                             - 120.0_rDef*Q_n(i-2, j)    &
                             +  45.0_rDef*Q_n(i-3, j)    &
                             -  10.0_rDef*Q_n(i-4, j)    &
                             +   1.0_rDef*Q_n(i-5, j))

            i = iMax-4
            Dis(i, j) = KCFac*( -5.0_rDef*Q_n(i+4, j)    & 
                             +  35.0_rDef*Q_n(i+3, j)    &
                             - 110.0_rDef*Q_n(i+2, j)    &
                             + 205.0_rDef*Q_n(i+1, j)    &
                             - 251.0_rDef*Q_n(i    , j)    &
                             + 210.0_rDef*Q_n(i-1, j)    &
                             - 120.0_rDef*Q_n(i-2, j)    &
                             +  45.0_rDef*Q_n(i-3, j)    &
                             -  10.0_rDef*Q_n(i-4, j)    &
                             +   1.0_rDef*Q_n(i-5, j))
       END DO
    END IF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! Shock Capturing Method !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    gam     = 1.40_rDef
    S_Min   = 0.010_rDef
    S_Max   = 0.015_rDef

    !!!! Get Tilde{S} to solve for 'S'. Not that at i = iMin and i = iMax values
    !!!! of zero are used. This is no problem since a Min and Max 'S' are 
    !!!! predefined in the JST paper (it will eventually override)

    !!DO i = iMin, iMax
    !!    Density(i) = Q_n(i, 1)*Jac_Curv(i)
    !!END DO
    !!
    !!DO i = iMin, iMax
    !!    IF (i == iMin) THEN
    !!        Tilde_S(i) = 0.0_rDef
    !!    ELSEIF (i == iMax) THEN
    !!        Tilde_S(i) = 0.0_rDef
    !!    ELSE
    !!        Tilde_S(i) = &
    !!            ABS(Density(i+1) - 2.0_rDef*Density(i) + Density(i-1))/&
    !!               (ABS(Density(i+1)) + 2.0_rDef*ABS(Density(i)) + ABS(Density(i-1)))
    !!    END IF
    !!END DO
    !!DO i = iMin, iMax
    !!    IF (i == iMin) THEN
    !!        S(i) = S_Min
    !!    ELSEIF (i == iMax) THEN
    !!        S(i) = S_Max
    !!    ELSE
    !!        S(i) = MAX(Tilde_S(i-1), Tilde_S(i), Tilde_S(i+1)) 
    !!    END IF
    !!END DO

    !!DeltaS_tot = S_Max  -   S_Min

    !!DO i = iMin, iMax
    !!    DeltaS_current(i)   = MAX(0.0_rDef, S(i) - S_Min)
    !!    DeltaS(i)           = MIN(DeltaS_current(i), DeltaS_tot)/(DeltaS_tot)
    !!END DO

    !!DO i = iMin, iMax
    !!    epsi(i)             =   3.0_rDef*(DeltaS(i)*DeltaS(i))              - &
    !!                            2.0_rDef*(DeltaS(i)*DeltaS(i)*DeltaS(i))
    !!END DO

    !!KCFac = 1.0_rDef/((4.0_rDef)*(4.0_rDef))
    !!DO j = bMin, bMax
    !!    DO i = iMin+1, iMax-1
    !!        D2(i, j) = KCFac*(1.0_rDef*Q_n(i-1, j)     & 
    !!                        -  2.0_rDef*Q_n(i, j)         &
    !!                        +  1.0_rDef*Q_n(i+1, j))
    !!    END DO
    !!END DO
    !!DO j = bMin, bMax
    !!    i = iMin
    !!    D2(i, j) = KCFac*(  -1.0_rDef*Q_n(i, j)        &
    !!                        + 1.0_rDef*Q_n(i+1, j))
    !!    i = iMax
    !!    D2(i, j) = KCFac*(  -1.0_rDef*Q_n(i, j)        &
    !!                        + 1.0_rDef*Q_n(i-1, j))
    !!END DO

    !!KCFac = 1.0_rDef/(1024.0_rDef)
    !!DO j = bMin, bMax
    !!    DO i = iMin+5, iMax-5
    !!        D10(i, j) = KCFac*(  1.0_rDef*Q_n(i-5, j)    & 
    !!                         -  10.0_rDef*Q_n(i-4, j)    &
    !!                         +  45.0_rDef*Q_n(i-3, j)    &
    !!                         - 120.0_rDef*Q_n(i-2, j)    &
    !!                         + 210.0_rDef*Q_n(i-1, j)    &
    !!                         - 252.0_rDef*Q_n(i    , j)    &
    !!                         + 210.0_rDef*Q_n(i+1, j)    &
    !!                         - 120.0_rDef*Q_n(i+2, j)    &
    !!                         +  45.0_rDef*Q_n(i+3, j)    &
    !!                         -  10.0_rDef*Q_n(i+4, j)    &
    !!                         +   1.0_rDef*Q_n(i+5, j))
    !!    END DO
    !!END DO
    !!DO j = bMin, bMax
    !!    i = iMin
    !!    D10(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
    !!                     +  5.0_rDef*Q_n(i+1, j)    &
    !!                     - 10.0_rDef*Q_n(i+2, j)    &
    !!                     + 10.0_rDef*Q_n(i+3, j)    &
    !!                     -  5.0_rDef*Q_n(i+4, j)    &
    !!                     +  1.0_rDef*Q_n(i+5, j))

    !!    i = iMin+1
    !!    D10(i, j) = KCFac*( 5.0_rDef*Q_n(i-1, j)    & 
    !!                     - 26.0_rDef*Q_n(i    , j)    &
    !!                     + 55.0_rDef*Q_n(i+1, j)    &
    !!                     - 60.0_rDef*Q_n(i+2, j)    &
    !!                     + 35.0_rDef*Q_n(i+3, j)    &
    !!                     - 10.0_rDef*Q_n(i+4, j)    &
    !!                     +  1.0_rDef*Q_n(i+5, j))

    !!    i = iMin+2
    !!    D10(i, j) = KCFac*(-10.0_rDef*Q_n(i-2, j)    & 
    !!                     +  55.0_rDef*Q_n(i-1, j)    &
    !!                     - 126.0_rDef*Q_n(i    , j)    &
    !!                     + 155.0_rDef*Q_n(i+1, j)    &
    !!                     - 110.0_rDef*Q_n(i+2, j)    &
    !!                     +  45.0_rDef*Q_n(i+3, j)    &
    !!                     -  10.0_rDef*Q_n(i+4, j)    &
    !!                     +   1.0_rDef*Q_n(i+5, j))

    !!    i = iMin+3
    !!    D10(i, j) = KCFac*( 10.0_rDef*Q_n(i-3, j)    & 
    !!                     -  60.0_rDef*Q_n(i-2, j)    &
    !!                     + 155.0_rDef*Q_n(i-1, j)    &
    !!                     - 226.0_rDef*Q_n(i    , j)    &
    !!                     + 205.0_rDef*Q_n(i+1, j)    &
    !!                     - 120.0_rDef*Q_n(i+2, j)    &
    !!                     +  45.0_rDef*Q_n(i+3, j)    &
    !!                     -  10.0_rDef*Q_n(i+4, j)    &
    !!                     +   1.0_rDef*Q_n(i+5, j))

    !!    i = iMin+4
    !!    D10(i, j) = KCFac*( -5.0_rDef*Q_n(i-4, j)    & 
    !!                     +  35.0_rDef*Q_n(i-3, j)    &
    !!                     - 110.0_rDef*Q_n(i-2, j)    &
    !!                     + 205.0_rDef*Q_n(i-1, j)    &
    !!                     - 251.0_rDef*Q_n(i    , j)    &
    !!                     + 210.0_rDef*Q_n(i+1, j)    &
    !!                     - 120.0_rDef*Q_n(i+2, j)    &
    !!                     +  45.0_rDef*Q_n(i+3, j)    &
    !!                     -  10.0_rDef*Q_n(i+4, j)    &
    !!                     +   1.0_rDef*Q_n(i+5, j))
    !!    
    !!    i = iMax
    !!    D10(i, j) = KCFac*(-1.0_rDef*Q_n(i    , j)    & 
    !!                     +  5.0_rDef*Q_n(i-1, j)    &
    !!                     - 10.0_rDef*Q_n(i-2, j)    &
    !!                     + 10.0_rDef*Q_n(i-3, j)    &
    !!                     -  5.0_rDef*Q_n(i-4, j)    &
    !!                     +  1.0_rDef*Q_n(i-5, j))

    !!    i = iMax-1
    !!    D10(i, j) = KCFac*( 5.0_rDef*Q_n(i+1, j)    & 
    !!                     - 26.0_rDef*Q_n(i    , j)    &
    !!                     + 55.0_rDef*Q_n(i-1, j)    &
    !!                     - 60.0_rDef*Q_n(i-2, j)    &
    !!                     + 35.0_rDef*Q_n(i-3, j)    &
    !!                     - 10.0_rDef*Q_n(i-4, j)    &
    !!                     +  1.0_rDef*Q_n(i-5, j))

    !!    i = iMax-2
    !!    D10(i, j) = KCFac*(-10.0_rDef*Q_n(i+2, j)    & 
    !!                     +  55.0_rDef*Q_n(i+1, j)    &
    !!                     - 126.0_rDef*Q_n(i    , j)    &
    !!                     + 155.0_rDef*Q_n(i-1, j)    &
    !!                     - 110.0_rDef*Q_n(i-2, j)    &
    !!                     +  45.0_rDef*Q_n(i-3, j)    &
    !!                     -  10.0_rDef*Q_n(i-4, j)    &
    !!                     +   1.0_rDef*Q_n(i-5, j))

    !!    i = iMax-3
    !!    D10(i, j) = KCFac*( 10.0_rDef*Q_n(i+3, j)    & 
    !!                     -  60.0_rDef*Q_n(i+2, j)    &
    !!                     + 155.0_rDef*Q_n(i+1, j)    &
    !!                     - 226.0_rDef*Q_n(i    , j)    &
    !!                     + 205.0_rDef*Q_n(i-1, j)    &
    !!                     - 120.0_rDef*Q_n(i-2, j)    &
    !!                     +  45.0_rDef*Q_n(i-3, j)    &
    !!                     -  10.0_rDef*Q_n(i-4, j)    &
    !!                     +   1.0_rDef*Q_n(i-5, j))

    !!    i = iMax-4
    !!    D10(i, j) = KCFac*( -5.0_rDef*Q_n(i+4, j)       & 
    !!                     +  35.0_rDef*Q_n(i+3, j)       &
    !!                     - 110.0_rDef*Q_n(i+2, j)       &
    !!                     + 205.0_rDef*Q_n(i+1, j)       &
    !!                     - 251.0_rDef*Q_n(i  , j)       &
    !!                     + 210.0_rDef*Q_n(i-1, j)       &
    !!                     - 120.0_rDef*Q_n(i-2, j)       &
    !!                     +  45.0_rDef*Q_n(i-3, j)       &
    !!                     -  10.0_rDef*Q_n(i-4, j)       &
    !!                     +   1.0_rDef*Q_n(i-5, j))
    !!END DO

    !!epsi  = 0.0_rDef

    !!DO i = iMin, iMax
    !!    DO j = bMin, bMax 
    !!        Dis(i, j) = (1.0_rDef-epsi(i))*D10(i, j) + epsi(i)*D2(i, j)
    !!    END DO
    !!END DO

    !!DO i = iMin, iMax
    !!    DO j = bMin, bMax 
    !!        Q_n(i, j) = Q_n(i, j)!/Jac_Curv(i)
    !!    END DO
    !!END DO
    

    END SUBROUTINE Dissipation_RHS1

END MODULE GetDissipationModule
