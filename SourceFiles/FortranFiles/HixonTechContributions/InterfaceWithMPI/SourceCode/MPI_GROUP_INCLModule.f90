!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                    !
!  This code copyright 2007 by Hixon Technologies, LLC.              !
!                                                                    !
!  Used by permission in the NASA BASS CAA code, and                 !
!   cannot be modified or used in other applications without the     !
!   express permission of Hixon Technologies, LLC.                   !
!                                                                    !
!  Contact:  Ray Hixon, Hixon Technologies, LLC.                     !
!            (440) 979-1783                                          !
!            email:  rhixon@wideopenwest.com                         !
!                                                                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE MPI_GROUP_INCLCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_GROUP_INCL call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: IncludeMPIGroup

CONTAINS

SUBROUTINE IncludeMPIGroup(GROUP,    &
                           N,        &
                           RANKS,    &
                           NEWGROUP, &
                           IERROR)

  INTEGER, INTENT(IN)  :: GROUP ! handle
  INTEGER, INTENT(IN)  :: N
  INTEGER, DIMENSION(:), INTENT(IN)  :: RANKS 
  INTEGER, INTENT(OUT) :: NEWGROUP ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_GROUP_INCL(GROUP,    &
                      N,        &
                      RANKS(1), &
                      NEWGROUP, &
                      IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE IncludeMPIGroup

END MODULE MPI_GROUP_INCLCall

MODULE MPI_GROUP_INCLModule
  USE MPI_GROUP_INCLCall

! this module is the wrapper to make the new F95 MPI_GROUP_INCL
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GROUP_INCL

CONTAINS

SUBROUTINE HTMPI_GROUP_INCL(GROUP,    &
                            N,        &
                            RANKS,    &
                            NEWGROUP, &
                            IERROR)

  INTEGER, INTENT(IN)  :: GROUP ! handle
  INTEGER, INTENT(IN)  :: N
  INTEGER, DIMENSION(:), INTENT(IN)  :: RANKS 
  INTEGER, INTENT(OUT) :: NEWGROUP ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL IncludeMPIGroup(GROUP    = GROUP,    &
                       N        = N,        &
                       RANKS    = RANKS,    &
                       NEWGROUP = NEWGROUP, &
                       IERROR   = IERROR)

  RETURN
END SUBROUTINE HTMPI_GROUP_INCL

END MODULE MPI_GROUP_INCLModule
