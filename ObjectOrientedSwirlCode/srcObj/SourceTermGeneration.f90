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
                             p              ,&
                             dp_dr          ,&
                             S_1       )
    ! INTENT IN
    INTEGER, INTENT(IN) :: np,mm
    COMPLEX(KIND=rDef) ,INTENT(IN) :: ak, & 
                                      gm1,&
                                      ci, & 
                                      axialWavenumber
                                  
    COMPLEX(KIND=rDef), DIMENSION(:) ,INTENT(INOUT) :: S_1  
  
    REAL(KIND=rDef), DIMENSION(:), INTENT(IN) ::r,&
                                   rmach, smach, snd, rvel, svel,p,dp_dr
    ! redefining the flow varibles as complex data types to clean up the 
    ! source terms
    COMPLEX(KIND=rDef) :: mmC
    COMPLEX(KIND=rDef), DIMENSION(np)  ::rC,&
                                        rmachC, smachC, sndC, rvelC,&
                                        svelC,pC,dp_drC
    mmC    =CMPLX( mm   ,rDef)
    rC     =CMPLX( r    ,rDef) 
    rmachC =CMPLX( rmach,rDef)
    smachC =CMPLX( smach,rDef)
    sndC   =CMPLX( snd  ,rDef) 
    rvelC  =CMPLX( rvel ,rDef)
    svelC  =CMPLX( svel ,rDef)
    pC     =CMPLX( p    ,rDef)
    dp_drC =CMPLX( dp_dr,rDef)

    
    S_1 = ci*( ak/sndC - (mmC/rC)*smachC - axialWavenumber*rmachC)*rvelC +&
               2.0_rDef/rC*smachC*svelC - dp_drC -&
               gm1/rC*smachC**(2.0_rDef*pC)   
                                   

!   DO i = 1,4
!     DO j = 1,np
!
!    S(i) = 
!   END DO
!  END DO 
  END SUBROUTINE getSourceTerms1
END MODULE sourceTermModule
