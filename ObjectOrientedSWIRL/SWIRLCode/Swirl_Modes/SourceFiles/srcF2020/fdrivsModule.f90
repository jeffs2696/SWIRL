MODULE fdrivsModule
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: fdrivs

    INTERFACE fdrivs
        MODULE PROCEDURE fdrivs1
    END INTERFACE fdrivs

    INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

    subroutine fdrivs1(np,sig,dl1,iorder,ed2,ed4)

        INTEGER, INTENT(IN) :: np, &
            iorder
        REAL(KIND=rDef), INTENT(IN) :: sig, &
            ed2, &
            ed4
        REAL(KIND=rDef), DIMENSION(:,:), INTENT(INOUT) :: dl1
!
! local variables
!
        INTEGER :: i, &
            j, &
            k

        REAL(KIND=rDef) :: coeff, &
            dr, &
            dx, &
            tot

        REAL(KIND=rDef), DIMENSION(np,np) :: dl2,  &
            dl4
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!     dimension dl1(NMAX,NMAX),dl2(NMAX,NMAX),dl4(NMAX,NMAX)
!
! Compute array for finite differences.
!
!JS: Zero out matricies

        do i = 1,np
            do j = 1,np
                dl1(i,j) = 0.0_rDef
            enddo
        enddo
        do i = 1,np
            if (iorder.eq.2) then
                if (i.eq.1) then
                    dl1(i,1) = -25.0_rDef
                    dl1(i,2) =  48.0_rDef
                    dl1(i,3) = -36.0_rDef
                    dl1(i,4) =  16.0_rDef
                    dl1(i,5) =  -3.0_rDef
                elseif (i.eq.2) then
                    dl1(i,1) =  -3.0_rDef
                    dl1(i,2) = -10.0_rDef
                    dl1(i,3) =  18.0_rDef
                    dl1(i,4) =  -6.0_rDef
                    dl1(i,5) =   1.0_rDef
                elseif (i.eq.np-1) then
                    dl1(i,np-4) =  -1.0_rDef
                    dl1(i,np-3) =   6.0_rDef
                    dl1(i,np-2) = -18.0_rDef
                    dl1(i,np-1) =  10.0_rDef
                    dl1(i,np)   =   3.0_rDef
                elseif (i.eq.np) then
                    dl1(i,np-4) =   3.0_rDef
                    dl1(i,np-3) = -16.0_rDef
                    dl1(i,np-2) =  36.0_rDef
                    dl1(i,np-1) = -48.0_rDef
                    dl1(i,np)   =  25.0_rDef
                else
                    dl1(i,i-2) =  1.0_rDef
                    dl1(i,i-1) = -8.0_rDef
                    dl1(i,i)   =  0.0_rDef
                    dl1(i,i+1) =  8.0_rDef
                    dl1(i,i+2) = -1.0_rDef
                endif
            else
                if (i.eq.1) then
                    dl1(i,1) =  -3.0_rDef
                    dl1(i,2) =   4.0_rDef
                    dl1(i,3) =  -1.0_rDef
                elseif (i.eq.np) then
                    dl1(i,np-2) =  1.0_rDef
                    dl1(i,np-1) = -4.0_rDef
                    dl1(i,np)   =  3.0_rDef
                else
                    dl1(i,i-1) = -1.0_rDef
                    dl1(i,i)   =  0.0_rDef
                    dl1(i,i+1) =  1.0_rDef
                endif
            endif
        enddo
        open(unit=15,file='deriv.matrix',status='unknown')
        rewind 15
        do i = 1,np
            write(15,10) (dl1(i,j), j=1,np)
        enddo
10      format(1x,16f7.1)
        close(15)
!
        coeff  = 0.50_rDef*(1.0_rDef -sig)
        dx = 2.0_rDef/REAL(np -1,rDef)
        do j = 1,np
            do i = 1,np
                dr       = coeff*dx
                if (iorder.eq.2) then
                    dl1(i,j) = dl1(i,j)/(12.0_rDef*dr)
                else
                    dl1(i,j) = dl1(i,j)/(2.0_rDef*dr)
                endif
            enddo
        enddo
!
! Compute 2nd and 4th derivative matrices.
        do k=1,np
            do j=1,np
                tot = 0.0_rDef
                do i = 1,np
                    tot = tot +dl1(k,i)*dl1(i,j)
                enddo
                dl2(k,j) = tot
            enddo
        enddo
        do k=1,np
            do j=1,np
                tot = 0.0_rDef
                do i = 1,np
                    tot = tot +dl2(k,i)*dl2(i,j)
                enddo
                dl4(k,j) = tot
            enddo
        enddo
!
! Correct 1st derivative matrix using 2nd and 4th order smoothing.
        do k=1,np
            do j=1,np
                dl1(k,j) = dl1(k,j) +ed2*dl2(k,j) +ed4*dl4(k,j)
            enddo
        enddo
!
        return
    end
END MODULE fdrivsModule
