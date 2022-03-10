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

MODULE MPI_INTERCOMM_CREATECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_INTERCOMM_CREATE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: CreateMPIIntercomm

CONTAINS

SUBROUTINE CreateMPIIntercomm(LOCAL_COMM,    &
                              LOCAL_LEADER,  &
                              BRIDGE_COMM,   &
                              REMOTE_LEADER, &
                              TAG,           &
                              NEWINTERCOMM,  &
                              IERROR)

  INTEGER, INTENT(IN)  :: LOCAL_COMM ! handle
  INTEGER, INTENT(IN)  :: LOCAL_LEADER
  INTEGER, INTENT(IN)  :: BRIDGE_COMM ! handle
  INTEGER, INTENT(IN)  :: REMOTE_LEADER
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(OUT) :: NEWINTERCOMM ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_INTERCOMM_CREATE(LOCAL_COMM, &
                            LOCAL_LEADER,  &
                            BRIDGE_COMM,   &
                            REMOTE_LEADER, &
                            TAG,           &
                            NEWINTERCOMM,  &
                            IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE CreateMPIIntercomm

END MODULE MPI_INTERCOMM_CREATECall

MODULE MPI_INTERCOMM_CREATEModule
  USE MPI_INTERCOMM_CREATECall

! this module is the wrapper to make the new F95 MPI_INTERCOMM_CREATE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_INTERCOMM_CREATE

CONTAINS

SUBROUTINE HTMPI_INTERCOMM_CREATE(LOCAL_COMM,    &
                                  LOCAL_LEADER,  &
                                  BRIDGE_COMM,   &
                                  REMOTE_LEADER, &
                                  TAG,           &
                                  NEWINTERCOMM,  &
                                  IERROR)

  INTEGER, INTENT(IN)  :: LOCAL_COMM ! handle
  INTEGER, INTENT(IN)  :: LOCAL_LEADER
  INTEGER, INTENT(IN)  :: BRIDGE_COMM ! handle
  INTEGER, INTENT(IN)  :: REMOTE_LEADER
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(OUT) :: NEWINTERCOMM ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL CreateMPIIntercomm(LOCAL_COMM    = LOCAL_COMM,    &
                          LOCAL_LEADER  = LOCAL_LEADER,  &
                          BRIDGE_COMM   = BRIDGE_COMM,   &
                          REMOTE_LEADER = REMOTE_LEADER, &
                          TAG           = TAG,           &
                          NEWINTERCOMM  = NEWINTERCOMM,  &
                          IERROR        = IERROR)

  RETURN
END SUBROUTINE HTMPI_INTERCOMM_CREATE

END MODULE MPI_INTERCOMM_CREATEModule
