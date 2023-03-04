MODULE globalModule
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: globalM

    INTERFACE globalM
        MODULE PROCEDURE globalM1
    END INTERFACE globalM

    INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

    subroutine globalM1(np,np4,sig,mode,om,snd,dd, &
        rr,rx,dr,rt,dt,aa,bb,S_aa,S_bb,row,col)

        INTEGER, INTENT(IN) :: &
            row, &
            col, &
            np,  &
            np4, &
            mode

        REAL(KIND=rDef), INTENT(IN) ::&
            sig

        REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: &
            dd

        REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
            snd, &
            rr,  &
            rx,  &
            dr,  &
            dt

        COMPLEX(KIND=rDef), INTENT(IN) :: &
            om

        COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(OUT) ::&
            aa, &
            bb, &
            S_aa,&
            S_bb

! define local variables

        REAL(KIND=rDef), PARAMETER :: &
            gm = 1.4_rDef , &
            tolerance = 10e-11_rDef

        INTEGER :: &
            j,  &
            j1, &
            j2, &
            j3, &
            k,  &
            k1, &
            k2, &
            k3

        REAL(KIND=rDef) :: &
            r

        REAL(KIND=rDef), DIMENSION(np) :: &
            rt

        COMPLEX(KIND=rDef) :: ci

! zero out the matrices

        do j=1,np4
            do k=1,np4

                aa(k,j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
                bb(k,j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
                S_aa(k,j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
                S_bb(k,j) = CMPLX(0.0_rDef,0.0_rDef,rDef)

            enddo
        enddo

        ci = CMPLX(0.0_rDef,1.0_rDef,rDef)

! Global matrices.

! np x np matrix, with 4x4 blocks in each entry
!
! In this one, the matrix is (4*np)x(4*np).
!
        do k=1,np        ! k  == v_r
            k1 =   np + k   ! k1 == v_{\theta}
            k2 = 2*np + k   ! k2 == v_x
            k3 = 3*np + k   ! k3 == p

            r   = rr(k)

            do j=1,np       ! j  == v_r
                j1 =   np + j  ! j1 == v_{\theta}
                j2 = 2*np + j  ! j2 == v_x
                j3 = 3*np + j  ! j3 == p

! dd is the derivative matrix for the radial direction

! derivatives appear in two locations in the [A] block matrix:
!       (1,4) and (4,1)

                aa(k,j3) = CMPLX(dd(k,j),KIND=rDef)  ! (1,4) : v_r eqn, p entry     d/dr
                aa(k3,j) = CMPLX(dd(k,j),KIND=rDef)  ! (4,1) : p   eqn, v_r entry   d/dr

                if (k.eq.j) then
                    aa(k,j)   = -ci*om/CMPLX(snd(k),KIND=rDef) ! (1,1) : v_r eqn, v_r entry:         -i k/A
                    aa(k1,j1) = -ci*om/CMPLX(snd(k),KIND=rDef) ! (2,2) : v_theta eqn, v_theta entry: -i k/A
                    aa(k2,j2) = -ci*om/CMPLX(snd(k),KIND=rDef) ! (3,3) : v_x eqn, v_x entry:         -i k/A
                    aa(k3,j3) = -ci*om/CMPLX(snd(k),KIND=rDef) ! (4,4) : p eqn, p entry:             -i k/A

                    aa(k1,j)  = CMPLX(dt(j),KIND=rDef)         ! (2,1) : v_theta eqn, v_r entry: dM_th/dr
                    aa(k2,j)  = CMPLX(dr(j),KIND=rDef)         ! (3,1) : v_x eqn, v_r entry: dM_x/dr

                    if (r.gt.0.0_rDef) then

                        aa(k1,j3) = ci*CMPLX(mode,KIND=rDef)/CMPLX(r,KIND=rDef) ! (2,4): v_theta eqn, p entry: i m/r

                        aa(k3,j1) = ci*CMPLX(mode,KIND=rDef)/CMPLX(r,KIND=rDef) ! (4,2): p eqn, v_theta entry: i m/r

                        aa(k3,j)  = aa(k3,j)      & ! (4,1): p eqn, v_r entry:
                            +&
                            CMPLX(1.0_rDef,KIND=rDef)/&
                            CMPLX(r,KIND=rDef)    & !                          +1/r
                            +&
                            CMPLX(gm +1.0_rDef,KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)/&
                            (&
                            CMPLX(2.0_rDef,KIND=rDef)*&
                            CMPLX(r,KIND=rDef)&
                            ) !  + ((gam+1)M_th^2)/(2r)

                        ! (1,1): v_r eqn, v_r entry:

                        aa(k,j)   = aa(k,j) &
                            +ci*&
                            CMPLX(mode,KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)/&
                            CMPLX(r,KIND=rDef) !    +(i m M_th)/r

                        aa(k1,j1) = aa(k1,j1) &                ! (2,2): v_th eqn, v_th entry:
                            +ci*&
                            CMPLX(mode,KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)/&
                            CMPLX(r,KIND=rDef) !    +(i m M_th)/r

                        aa(k2,j2) = aa(k2,j2) &                ! (3,3): v_x eqn, v_x entry:
                            +ci*&
                            CMPLX(mode,KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)/&
                            CMPLX(r,KIND=rDef) !    +(i m M_th)/r

                        aa(k3,j3) = aa(k3,j3) &                ! (4,4): p eqn, p entry:
                            +ci*CMPLX(mode,KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)/&
                            CMPLX(r,KIND=rDef) !    +(i m M_th)/r

                        aa(k,j1)  = CMPLX(-2.0_rDef,KIND=rDef)*&
                            CMPLX(rt(j)    ,KIND=rDef)/&
                            CMPLX(r,KIND=rDef)          ! (1,2): v_r eqn, v_th entry:  -(2 M_th)/r

                        aa(k,j3)  = aa(k,j3) &                 ! (1,4): v_r eqn, p entry:
                            +CMPLX((gm -1.0_rDef),KIND=rDef)*CMPLX(rt(j),KIND=rDef)*CMPLX(rt(j),KIND=rDef)/CMPLX(r,KIND=rDef) !  + (gam-1)*(M_th^2)/r

                        aa(k1,j) = aa(k1,j) &                  ! (2,1): v_th eqn, v_r entry:
                            +&
                            CMPLX(rt(j),KIND=rDef)/&
                            CMPLX(r,KIND=rDef) &                  !                   +(M_th)/r
                            +&
                            CMPLX((gm -1.0_rDef),KIND=rDef)*&
                            (CMPLX(rt(j),KIND=rDef)**3)/&
                            (CMPLX(2.0_rDef,KIND=rDef)*&
                            CMPLX(r,KIND=rDef)) !       +((gam-1)*M_th^3)/(2 r)

                        aa(k2,j) = aa(k2,j) &                  ! (3,1): v_x eqn, v_r entry:
                            +&
                            CMPLX((gm -1.0_rDef),KIND=rDef)*&
                            CMPLX(rx(j),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)/&
                            (&
                            CMPLX(2.0_rDef,KIND=rDef)*&
                            CMPLX(r       ,KIND=rDef)&
                            ) ! +((gam-1)*M_x*M_th^2)/(2 r)

                        !

                    elseif (r.lt.tolerance) THEN ! r == 0
                        aa(k1,j3) = CMPLX(0.0_rDef,0.0_rDef,rDef) ! (2,4): v_th eqn, p entry: 0
                        aa(k3,j1) = CMPLX(0.0_rDef,0.0_rDef,rDef) ! (4,2): p eqn, v_th entry: 0
                        aa(k,j)   = aa(k,j) &                     ! (1,1): v_r eqn, v_r entry:
                            +ci*CMPLX(mode,KIND=rDef)*CMPLX(dt(j),KIND=rDef)      !    + i m M_th
                        aa(k1,j1) = aa(k1,j1) &                   ! (2,2): v_th eqn, v_th entry:
                            +ci*CMPLX(mode,KIND=rDef)*CMPLX(dt(j),KIND=rDef)      !    + i m M_th
                        aa(k2,j2) = aa(k2,j2) &                   ! (3,3): v_x eqn, v_x entry:
                            +ci*CMPLX(mode,KIND=rDef)*CMPLX(dt(j),KIND=rDef)      !    + i m M_th
                        aa(k3,j3) = aa(k3,j3) &                   ! (4,4): p eqn, p entry:
                            +ci*&
                            CMPLX(mode,KIND=rDef)*&
                            CMPLX(dt(j),KIND=rDef)      !    + i m M_th
                        aa(k,j1)  = CMPLX(-2.0_rDef,KIND=rDef)*CMPLX(dt(j),KIND=rDef)               ! (1,2): v_r eqn, v_th entry: - 2 M_th
                        aa(k,j3)  = aa(k,j3) &                    ! (1,4): v_r eqn, p entry:
                            +CMPLX(2.0_rDef,KIND=rDef)*CMPLX((gm -1.0_rDef),KIND=rDef)*CMPLX(rt(j),KIND=rDef)*CMPLX(dt(j),KIND=rDef)  ! + 2 (gam-1)*M_th*dM_th/dr
                        aa(k1,j)  = aa(k1,j) &                    ! (2,1): v_th eqn, v_r entry:
                            +(&
                            CMPLX(1.0_rDef,KIND=rDef) +&
                            CMPLX(3.0_rDef,KIND=rDef)*&
                            CMPLX((gm -1.0_rDef),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*&
                            CMPLX(dt(j),KIND=rDef)
                        !        +(1+3*((gam-1)/2)*Mth^2)*dM_th/dr
                        aa(k2,j)  = aa(k2,j) &                    ! (3,1): v_x eqn, v_r entry:
                            +&
                            CMPLX((gm -1.0_rDef),KIND=rDef)/&
                            CMPLX(2.0_rDef,KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)*(&
                            CMPLX(dr(j),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef) +&
                            CMPLX(2.0_rDef,KIND=rDef)*&
                            CMPLX(rx(j),KIND=rDef)*&
                            CMPLX(dt(j),KIND=rDef))
                        !  +((gam-1)/2)*Mth*(Mth*dMx/dr + 2*Mx*dMth/dr)
                        aa(k3,j) = aa(k3,j) &                     ! (4,1): p eqn, v_r entry:
                            +&
                            CMPLX((gm +1.0_rDef),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)*&
                            CMPLX(dt(j),KIND=rDef)    !    + (gam+1)*M_th*dMth/dr
                    else ! r == 0
                        aa(k1,j3) = CMPLX(0.0_rDef,0.0_rDef,rDef) ! (2,4): v_th eqn, p entry: 0
                        aa(k3,j1) = CMPLX(0.0_rDef,0.0_rDef,rDef) ! (4,2): p eqn, v_th entry: 0
                        aa(k,j)   = aa(k,j) &                     ! (1,1): v_r eqn, v_r entry:
                            +ci*CMPLX(mode,KIND=rDef)*CMPLX(dt(j),KIND=rDef)      !    + i m M_th
                        aa(k1,j1) = aa(k1,j1) &                   ! (2,2): v_th eqn, v_th entry:
                            +ci*CMPLX(mode,KIND=rDef)*CMPLX(dt(j),KIND=rDef)      !    + i m M_th
                        aa(k2,j2) = aa(k2,j2) &                   ! (3,3): v_x eqn, v_x entry:
                            +ci*CMPLX(mode,KIND=rDef)*CMPLX(dt(j),KIND=rDef)      !    + i m M_th
                        aa(k3,j3) = aa(k3,j3) &                   ! (4,4): p eqn, p entry:
                            +ci*&
                            CMPLX(mode,KIND=rDef)*&
                            CMPLX(dt(j),KIND=rDef)      !    + i m M_th
                        aa(k,j1)  = CMPLX(-2.0_rDef,KIND=rDef)*CMPLX(dt(j),KIND=rDef)               ! (1,2): v_r eqn, v_th entry: - 2 M_th
                        aa(k,j3)  = aa(k,j3) &                    ! (1,4): v_r eqn, p entry:
                            +CMPLX(2.0_rDef,KIND=rDef)*CMPLX((gm -1.0_rDef),KIND=rDef)*CMPLX(rt(j),KIND=rDef)*CMPLX(dt(j),KIND=rDef)  ! + 2 (gam-1)*M_th*dM_th/dr
                        aa(k1,j)  = aa(k1,j) &                    ! (2,1): v_th eqn, v_r entry:
                            +(&
                            CMPLX(1.0_rDef,KIND=rDef) +&
                            CMPLX(3.0_rDef,KIND=rDef)*&
                            CMPLX((gm -1.0_rDef),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*&
                            CMPLX(dt(j),KIND=rDef)
                        !        +(1+3*((gam-1)/2)*Mth^2)*dM_th/dr
                        aa(k2,j)  = aa(k2,j) &                    ! (3,1): v_x eqn, v_r entry:
                            +&
                            CMPLX((gm -1.0_rDef),KIND=rDef)/&
                            CMPLX(2.0_rDef,KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)*(&
                            CMPLX(dr(j),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef) +&
                            CMPLX(2.0_rDef,KIND=rDef)*&
                            CMPLX(rx(j),KIND=rDef)*&
                            CMPLX(dt(j),KIND=rDef))
                        !  +((gam-1)/2)*Mth*(Mth*dMx/dr + 2*Mx*dMth/dr)
                        aa(k3,j) = aa(k3,j) &                     ! (4,1): p eqn, v_r entry:
                            +&
                            CMPLX((gm +1.0_rDef),KIND=rDef)*&
                            CMPLX(rt(j),KIND=rDef)*&
                            CMPLX(dt(j),KIND=rDef)    !    + (gam+1)*M_th*dMth/dr
                    endif


                    bb(k,j)   = CMPLX(rx(j),KIND=rDef)                          ! (1,1): v_r eqn, v_r entry: M_x
                    bb(k1,j1) = CMPLX(rx(j),KIND=rDef)                          ! (2,2): v_th eqn, v_th entry: M_x
                    bb(k2,j2) = CMPLX(rx(j),KIND=rDef)                          ! (3,3): v_x eqn, v_x entry: M_x
                    bb(k3,j3) = CMPLX(rx(j),KIND=rDef)                          ! (4,4): p eqn, p entry: M_x
                    bb(k2,j3) = CMPLX(1.0_rDef,0.0_rDef,rDef)  ! (3,4): v_x eqn, p entry: 1.0
                    bb(k3,j2) = CMPLX(1.0_rDef,0.0_rDef,rDef)  ! (4,3): p eqn, v_x entry: 1.0
                endif
            enddo
        enddo
!


        OPEN(456)



        do k = 1,np4
            WRITE(456,*) aa(k,:)
            ENDDO
            close(456)
        ! row = 2
        ! col = 2

        do k=1,np        ! k  == v_r
            k1 =   np + k   ! k1 == v_{\theta}
            k2 = 2*np + k   ! k2 == v_x
            k3 = 3*np + k   ! k3 == p

            r   = rr(k)


            do j=1,np       ! j  == v_r
                j1 =   np + j  ! j1 == v_{\theta}
                j2 = 2*np + j  ! j2 == v_x
                j3 = 3*np + j  ! j3 == p



                !v_r
                if (row .eq. 1 .AND. col .eq.1) then

                    S_aa(k ,j)  = aa(k ,j)

                elseif (row .eq. 2 .AND. col .eq.  1) then

                    S_aa(k1,j) = aa(k1,j)

                elseif (row .eq. 3 .AND. col .eq.  1) then

                    S_aa(k2,j) = aa(k2,j)

                elseif (row .eq. 4 .AND. col .eq.  1) then

                    S_aa(k3,j) = aa(k3,j)

                    !v_theta
                elseif (row .eq. 1 .AND. col .eq.  2) then

                    S_aa(k ,j1) = aa(k ,j1)

                elseif (row .eq. 2 .AND. col .eq.  2) then

                    S_aa(k1,j1) = aa(k1,j1)

                elseif (row .eq. 3 .AND. col .eq.  2) then

                    S_aa(k2,j1) = aa(k2,j1)

                elseif (row .eq. 4 .AND. col .eq.  2) then

                    S_aa(k3,j1) = aa(k3,j1)

                    !v_x
                elseif (row .eq. 1 .AND. col .eq.  3) then

                    S_aa(k ,j2) = aa(k ,j2)

                elseif (row .eq. 2 .AND. col .eq.  3) then

                    S_aa(k1,j2) = aa(k1,j2)

                elseif (row .eq. 3 .AND. col .eq.  3) then

                    S_aa(k2,j2) = aa(k2,j2)

                elseif (row .eq. 4 .AND. col .eq.  3) then

                    S_aa(k3,j2) = aa(k3,j2)
!pressure

                elseif (row .eq. 1 .AND. col .eq.  4) then

                    S_aa(k ,j3) = aa(k ,j3)

                elseif (row .eq. 2 .AND. col .eq.  4) then

                    S_aa(k1,j3) = aa(k1,j3)

                elseif (row .eq. 3 .AND. col .eq.  4) then

                    S_aa(k2,j3) = aa(k2,j3)

                elseif (row .eq. 4 .AND. col .eq.  4) then

                    S_aa(k3,j3) = aa(k3,j3)
                else
                endif
                ! S_aa(k2,j2) = aa(k2,j2)
                ! S_aa(k3,j3) = aa(k3,j3)

                ! S_bb(k,j)   = bb(k,j)
                ! S_bb(k1,j1) = bb(k1,j1)
                ! S_bb(k2,j2) = bb(k2,j2)
                ! S_bb(k3,j3) = bb(k3,j3)
            enddo
        enddo

        ! WRITE(0,*) 'S_aa(k,j)', S_aa(k,j)

!cc = with aa element that i want and Zeros else where
        return
        IF (sig > 0.0_rDef) CONTINUE ! sig is not actually used in this routine
    end

END MODULE globalModule
