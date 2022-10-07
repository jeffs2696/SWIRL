! outputModule - extract radial modes and axial wavenumbers
MODULE outputModule
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    USE zeroCrossingModule
    USE indexxModule
    USE egvModule
    USE kapsubModule
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: output

    INTERFACE output
        MODULE PROCEDURE output1
    END INTERFACE

    INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

    subroutine output1(np,np4,mode,rho,omega, &
            egv,attenh,attend,rmx,drm,rmt,drt,snd,rr,wvn,vrm,vphi,is,file_name_string)

        INTEGER, INTENT(IN) :: &
            ! myunit1, &
        ! myunit2, &
        np, &
            np4, &
            mode, &
            is!, & icomp

        REAL(KIND=rDef), INTENT(IN) :: &
            rho!, &
        !ang!, & gam!swirl magn JS

        REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rmx, &
            drm, &
            rmt, &
            drt, &
            snd, &
            rr

        COMPLEX(KIND=rDef), INTENT(IN) :: omega, &
            attenh, &
            attend

        COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: wvn, &
            vphi

        COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: vrm

        CHARACTER(LEN=*), INTENT(IN) :: egv,file_name_string

        CHARACTER(10) :: file_id
        !
        ! local variables
        !

        INTEGER :: &
            i, &
            ! icomp ,&
        j, &
            ! jj, &
        jtmp, &
            ! kk, &
        mumax !,&
        !, & UNIT12,UNIT14,UNIT16!, & n

        ! INTEGER, DIMENSION(np) :: mu

        INTEGER, DIMENSION(np4) :: izeros, &
            indx

        REAL(KIND=rDef) :: eps, &
            pi, &
            cvcmin, &
            cvcmax, &
            r, &
            rx, &
            rt, &
            akx, &
            aim, &
            are, &
            val, &
            vold, &
            gam1a, &
            gam2a, &
            ! aki, &
        alm1, &
            alm2, &
            ! disc, &
        fac, &
            ! fn, &
        ! fn1, &
        gim1, &
            gim2, &
            ! rm, &
        ! rmav, &
        ! term, &
        ! tot, &
        as

        REAL(KIND=rDef), DIMENSION(np4) :: phi, &
            azeros

        ! REAL(KIND=rDef), DIMENSION(np) :: akappa

        COMPLEX(KIND=rDef) :: ci, &
            cv, &
            gamma1

        COMPLEX(KIND=rDef), DIMENSION(np,np4) :: &
            radial_velocity_mode, &
            tangential_velocity_mode, &
            axial_velocity_mode, &
            pressure_mode 

        ! COMPLEX(KIND=rDef), DIMENSION(np) :: gam1, &
        !     gam2
        !
        !

        ! Output files:
        !               output.dat : has everything.
        !               gam.nonconv: nonconvecting mode data.
        !               gam.non.acc: nonconvecting mode data with more digits of accuracy.
        !               gam.compare: compares the result from spectral and q3d methods.
        !

        pi  = 4.0_rDef*ATAN(1.0_rDef)
        ci  = CMPLX(0.0_rDef,1.0_rDef,rDef)
        eps = 1.e-8_rDef
        alm1 = -10000
        alm2 = -10000

        cvcmin =  1.e+4_rDef
        cvcmax = -1.e+4_rDef
        !
        !drh: np == number of radial points
        !
        ! CALL SortAxialWavenumbers(
        !   wvn - wavenumbers
        !   cvcmin/max - max range on convective wavenumber (k/M) 
        !   eps - error tolerance?
        !   cv - convective wavenumber

        ! Compute range of convected wavenumbers.
        do i = 1,np
        r  =  rr(i)
        rx =  rmx(i)
        rt =  rmt(i)
        as =  snd(i)
        ! if r = 0 compute the convective wavenumber as follows...
        if (r.lt. 10e-12_rDef ) then
            cv = (&
                omega/CMPLX(as,KIND=rDef) -&
                CMPLX(mode,KIND=rDef)*CMPLX(rt,KIND=rDef))/CMPLX(rx , KIND = rDef)
        else 
            cv = (omega/CMPLX(as,KIND=rDef) - &
                CMPLX(mode,KIND=rDef)/CMPLX(r*rt,KIND=rDef))/CMPLX(rx,KIND=rDef)
        endif

        if (abs(cv).gt.cvcmax) cvcmax = abs(cv)
        if (abs(cv).lt.cvcmin) cvcmin = abs(cv)

        enddo

        ! 

        if (cvcmin .ge. 0.0_rDef) then
            cvcmin = cvcmin -eps
        else
            cvcmin = cvcmin +eps
        endif
        if (cvcmax .ge. 0.0_rDef) then
            cvcmax = cvcmax +eps
        else
            cvcmax = cvcmax -eps
        endif

        !WRITE(0,*) 'convection speed: ',cvcmin,cvcmax
        !
        ! RH: Compute number of zero crossings for nonconvected modes.

        ! JS: adding zeroCrossing CALL
        ! WRITE(0,*) (vrm)
        
        ! saving each mode for each eigenvalue

        
        DO i = 1,np4
        akx = REAL(wvn(i),KIND=rDef)
        if (akx .le. cvcmin .or. akx .ge. cvcmax) then
            izeros(i) = 0

            radial_velocity_mode(:,i) = vrm(1:np,i)
            tangential_velocity_mode(:,i) = vrm(np+1:2*np,i)
            axial_velocity_mode(:,i) = vrm(2*np+1:3*np,i)
            pressure_mode(:,i) = vrm(3*np+1:np4,i)

        elseif (akx.lt.10e-12_rDef) then
            izeros(i) = 100
        else
            izeros(i) = 200
        endif
        ENDDO

                CALL zeroCrossing(&
                    domain = rr , &
                    dataSet = pressure_mode(:,1))


        do i = 1,np4

        gamma1 = wvn(i)

        akx   = real(gamma1)
        ! First, only check the corresponding mode if it is within bounds on real axis
        if (akx .le. cvcmin .or. akx .ge. cvcmax) then

            

            ! start the zero counter. i.e. at this point there are no zeros
            izeros(i) = 0
            ! Calculate phase of the mode i


            ! WRITE(0,*) 'nonconvecting mode found at index', i

            ! calculating the phase of the first grid point
            aim    = aimag(pressure_mode(1,i))
            are    =  real(pressure_mode(1,i))
            phi(i) = atan2(aim,are)

            ! first point of mode with 0 phase
            vold   = real(pressure_mode(1,i)*exp(-ci*CMPLX(phi(i),KIND=rDef)))


            ! WRITE(0,*) aim, are, phi(i),vold
            do j = 3*np+2,np4

            val = real(vrm(j,i)*exp(-ci*CMPLX(phi(i),KIND = rDef)))

            ! checking for sign change from first point to second
            if (val*vold.lt.0.0_rDef) then
                izeros(i) = izeros(i) +1 
            endif 
            vold = val
            enddo

        elseif (akx.lt.10e-12_rDef) then
            izeros(i) = 100
        else
            izeros(i) = 200
        endif
        enddo
        !

        ! Sort modes by number of zero crossings.

        do i=1,np4
        azeros(i) = REAL(izeros(i),rDef)
        enddo

        CALL indexx(n     = np4,    &
            arrin = azeros, &
            indx  = indx)
        !
        ! Sort nonconvected modes into upstream and downstream.

        eps  = 1.e-4_rDef ! JS: ??? 

        !     do j=1,np4
        do j=1,np4-1
        if (izeros(indx(j)).eq.izeros(indx(j+1))) then
            gam1a = real(wvn(indx(j)))
            gam2a = real(wvn(indx(j+1)))
            if (gam2a .lt. 10e-12_rDef) then
                goto 1000
            endif
            if (abs(gam1a/gam2a -1.0_rDef).lt.eps) then
                gim1 = aimag(wvn(indx(j)))
                gim2 = aimag(wvn(indx(j+1)))
                if (gim1.lt.gim2) then
                    jtmp      = indx(j)
                    indx(j)   = indx(j+1)
                    indx(j+1) = jtmp
                endif
            else
                if (gam1a .lt. 10e-12_rDef .or. gam2a .lt. 10e-12_rDef) then
                    !print*, 'gam1a = ',gam1a,'  gam2a = ',gam2a
                    IF (gam1a == 0.0_rDef) THEN
                        alm1 = 0.0_rDef
                        alm2 = 2.0_rDef*PI/gam2a
                    END IF
                    IF (gam2a == 0.0_rDef) THEN
                        alm1 = 2.0_rDef*PI/gam1a
                        alm2 = 0.0_rDef
                    END IF
                else
                    alm1 = 2.0_rDef*PI/gam1a
                    alm2 = 2.0_rDef*PI/gam2a
                endif
                ! WRITE(6,*) 'alm1\2 :' ,alm1, alm2
                if (abs(alm1).lt.abs(alm2)) then
                    jtmp       = indx(j)
                    indx(j)   = indx(j+1)
                    indx(j+1) = jtmp
                endif
            endif
        endif
        enddo
        1000    continue

        CALL saveEGV(&
            np = np,&
            vrm = vrm,&
            rr = rr)

        WRITE(file_id, '(i0.4)') np
        open(unit=14,             &
            file='03-EVanalysis/' // &
            TRIM(file_name_string) //&
            'gam.nonconv.'//&
            TRIM(ADJUSTL(file_id)) )

        open(unit=16,             &
            file='03-EVanalysis/' // TRIM(file_name_string) //'gam.nonconv_acc.'// TRIM(ADJUSTL(file_id))  )
        rewind 14
        rewind 16
        write(14,40)
        write(16,42)
        40      format('#',2x,'Re{gam}',6x,'Im{gam}',4x,'Re{gam/ak}',    &
            3x,'Im{gam/ak}',2x,'nz')
        42      format('#',2x,'Re{gam}',13x,'Im{gam}',11x,'Re{gam/ak}', &
            10x,'Im{gam/ak}',5x,'nz')
        do j=1,np4
        gamma1 = wvn(indx(j))
        fac   = (1.0_rDef +rho)/2.0_rDef
        if (izeros(indx(j)).lt.np-4) then
            write(14,10) indx(j),gamma1,gamma1/omega,izeros(indx(j))-1!Changing zero order to match NASA convention for radial modes
            write(16,12) indx(j),gamma1,gamma1/omega,izeros(indx(j))-1
        endif
        enddo
        10      format(1x,i4,4e13.5,i4)
        12      format(1x,i4,4e20.12,i4)
        close(14)
        close(16)
        !
        mumax = int(REAL(np,rDef)/PI)

        ! took out gamma comparison JS
        ! icomp = 0
        !if (icomp .eq. 1) then
        !
        !           open(unit=15,            &
        !               file='gam.compare', &
        !               status='unknown')
        !           rewind 15
        !           write(15,35)
        !5          format(4x,'i',18x,'gam_spec',32x,'gam_q3d',17x,'mu')
        !
        ! Compute kappas.
        !
        !      mumax = int(REAL(np,rDef)/PI)
        !
        ! test -- don't we already have the kappas?
        !!
        !       CALL kappa(mm    = mode,  &
        !                  mumax = mumax, &
        !                  sig   = rho,   &
        !                  mu    = mu,    &
        !                  akap  = akappa)
        !
        !
        !! Compute average axial Mach number.
        !            tot = 0.0_rDef
        !            do n = 2,np
        !                fn1 = rmx(n-1)
        !                fn  = rmx(n)
        !                tot = tot +(fn1 +fn)*(rr(n) -rr(n-1))
        !            enddo
        !            rmav = tot/(1.0_rDef -rho)/2.0_rDef
        !            rm   = rmav
        !            write(6,*) rmav
        !            do i = 1,mumax
        !                aki  = akappa(i)
        !        term = real(omega) -mode*ang
        !        disc = term*term +(rm*rm -1.0_rDef)*aki*aki
        !        if (disc .ge. 0.0_rDef) then
        !         gam1(i) = (rm*term +sqrt(disc))/(rm*rm -1.0_rDef)
        !         gam2(i) = (rm*term -sqrt(disc))/(rm*rm -1.0_rDef)
        !            else
        !         gam1(i) = (rm*term +ci*sqrt(-disc))/(rm*rm -1.0_rDef)
        !         gam2(i) = (rm*term -ci*sqrt(-disc))/(rm*rm -1.0_rDef)
        !            endif
        !            enddo 
        !        endif

        !25  format(1x,i4,4e20.12,i4)
        !
        !    do i = 1,2*np
        !        if (izeros(indx(i)).lt.mumax) then
        !            jj = (i +1)/2
        !            kk = i/2
        !            if (mod(i,2) .eq. 1) then
        !!                write(0,*) izeros(indx(i)),wvn(indx(i))!,gam2(jj)
        !                write(15,*) izeros(indx(i)),wvn(indx(i))!,gam2(jj)
        !            else
        !                write(15,*) izeros(indx(i)),wvn(indx(i)),mu(kk)! ,gam1(kk)
        !            endif
        !        endif
        !    enddo
        !
        return
        WRITE(6,*) drm,drt,egv,is,vphi
    end subroutine
END MODULE outputModule
