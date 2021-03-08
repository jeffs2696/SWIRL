! This module calculates the source terms needed for the MMS
! for equations 2.38-2.41 in the SwirlCodePaper
MODULE sourceTermModule
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: getSourceTerms

  INTERFACE getSourceTerms
    MODULE PROCEDURE getSourceTerms1
  END INTERFACE
 
    INTEGER, PARAMETER :: rDef = REAL64
    
!    ci  = CMPLX(0.0,1.0)
  CONTAINS

  SUBROUTINE getSourceTerms1(np             ,&
                             mm             ,&
                             gm1            ,&
                             ak             ,&
                             ci             ,&
                             axialWavenumber,&
                             r              ,&
                             rmach          ,&
                             smach          ,& 
                             snd            ,& 
                             rvel           ,& 
                             svel           ,&
                             xvel           ,&
                             p              ,&
                             dp_dr          ,&
                             dsmach_dr      ,&
                             drmach_dr      ,&
                             drvel_dr       ,&
                             S_1            ,&
                             S_2            ,&
                             S_3            ,&
                             S_4 )
    ! INTENT IN
    INTEGER, INTENT(IN) :: np,mm
    COMPLEX(KIND=rDef) ,INTENT(IN) :: ak, & 
                                      gm1,&
                                      ci, & 
                                      axialWavenumber
                                  
    COMPLEX(KIND=rDef), DIMENSION(:) ,INTENT(INOUT) :: S_1  ,&
                                                       S_2  ,&
                                                       S_3  ,&
                                                       S_4

  
    REAL(KIND=rDef), DIMENSION(:), INTENT(IN) ::r,&
                                                rmach,&
                                                smach,&
                                                snd  ,&
                                                rvel ,&
                                                svel ,&
                                                xvel ,&
                                                p    ,&
                                                dp_dr,&
                                                dsmach_dr,&
                                                drmach_dr,&
                                                drvel_dr
    ! redefining the flow varibles as complex data types to clean up the 
    ! source terms
    COMPLEX(KIND=rDef) :: mmC
    COMPLEX(KIND=rDef), DIMENSION(np)  ::rC,&
                                        rmachC,&
                                        smachC,& 
                                        sndC,  &
                                        rvelC,&
                                        svelC ,&
                                        xvelC ,&
                                        pC    ,&
                                        dp_drC,&
                                        dsmach_drC,&
                                        drmach_drC,&
                                        drvel_drC
    mmC    =CMPLX( mm   ,rDef)
    rC     =CMPLX( r    ,rDef) 
    rmachC =CMPLX( rmach,rDef)
    smachC =CMPLX( smach,rDef)
    dsmach_drC =CMPLX( dsmach_dr,rDef)
    drmach_drC =CMPLX( drmach_dr,rDef)
    sndC   =CMPLX( snd  ,rDef) 
    rvelC  =CMPLX( rvel ,rDef)
    svelC  =CMPLX( svel ,rDef)
    xvelC  =CMPLX( xvel ,rDef)
    pC     =CMPLX( p    ,rDef)
    dp_drC =CMPLX( dp_dr,rDef)

    
    S_1 = ci*( ak/sndC - (mmC/rC)*smachC - axialWavenumber*rmachC)*rvelC +&
               (2.0_rDef/rC)*smachC*svelC - dp_drC -&
               (gm1/rC)*smachC**(2.0_rDef)*pC   
                                   
    S_2 = -ci*( ak/sndC - (mmC/rC)*smachC -axialWavenumber*rmachC)*svelC +&
              (smachC/rC + dsmach_drC + (gm1/(2*rC))*smachC**(3.0_rDef))*rvelC  +& 
              (ci*mmC/rC)*pC 

    S_3 = -ci*( ak/sndC - (mmC/rC)*smachC -axialWavenumber*rmachC)*xvelC +&
              (drmach_drC + (gm1/(2*rC))*rmachC*smachC**(2.0_rDef))*rvelC  +& 
              ci*axialWavenumber*pC

    S_4 = -ci*( ak/sndC - (mmC/rC)*smachC -axialWavenumber*rmachC)*pC +&
              drvel_drC + ((gm1/(2*rC))*smachC**(2.0_rDef) + 1.0_rDef/rC)*rvelC  +& 
              (ci*mmC/rC)*svelC+ci*axialWavenumber*rvelC
!   DO i = 1,4
!     DO j = 1,np
!
!    S(i) = 
!   END DO
!  END DO 
  END SUBROUTINE getSourceTerms1
END MODULE sourceTermModule
