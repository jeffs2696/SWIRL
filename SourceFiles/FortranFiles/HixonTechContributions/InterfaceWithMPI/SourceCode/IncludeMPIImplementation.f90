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

MODULE IncludeMPIImplementation

! this module (should be) the only place that the MPI implementation is
!  actually seen by the code....

  USE mpi, ONLY: MPI_ADDRESS_KIND,     &
                 MPI_ANY_SOURCE,       &
                 MPI_ANY_TAG,          &
                 MPI_CHARACTER,        &
                 MPI_COMM_SELF,        &
                 MPI_COMM_WORLD,       &
                 MPI_DOUBLE_PRECISION, &
                 MPI_ERROR,            &
                 MPI_ERROR_STRING,     &
                 MPI_ERRORS_RETURN,    &
                 MPI_ERR_BUFFER,       &
                 MPI_ERR_COMM,         &
                 MPI_ERR_COUNT,        &
                 MPI_ERR_RANK,         &
                 MPI_ERR_TAG,          &
                 MPI_ERR_TRUNCATE,     &
                 MPI_ERR_TYPE,         &
                 MPI_INFO_NULL,        &
                 MPI_INTEGER,          &
                 MPI_INTEGER4,         &
                 MPI_INTEGER8,         &
                 MPI_LOGICAL,          &
                 MPI_LAND,             &
                 MPI_LOR,              &
                 MPI_LXOR,             &
                 MPI_MAX,              &
                 MPI_MIN,              &
                 MPI_MAXLOC,           &
                 MPI_MINLOC,           &
                 MPI_OFFSET_KIND,      &
                 MPI_PACKED,           &
                 MPI_PROC_NULL,        &
                 MPI_PROD,             &
                 MPI_REAL,             &
                 MPI_REAL4,            &
                 MPI_REAL8,            &
                 MPI_SIZEOF,           &
                 MPI_SOURCE,           &
                 MPI_STATUS_SIZE,      &
                 MPI_SUCCESS,          &
                 MPI_SUM
  USE mpi, ONLY: MPI_TAG,              &
                 MPI_TAG_UB,           &
                 MPI_UNDEFINED,        &
                 MPI_WTICK,            &
                 MPI_WTIME

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: MPI_ADDRESS_KIND,     &
            MPI_ANY_SOURCE,       &
            MPI_ANY_TAG,          &
            MPI_CHARACTER,        &
            MPI_COMM_SELF,        &
            MPI_COMM_WORLD,       &
            MPI_DOUBLE_PRECISION, &
            MPI_ERROR,            &
            MPI_ERROR_STRING,     &
            MPI_ERRORS_RETURN,    &
            MPI_ERR_BUFFER,       &
            MPI_ERR_COMM,         &
            MPI_ERR_COUNT,        &
            MPI_ERR_RANK,         &
            MPI_ERR_TAG,          &
            MPI_ERR_TRUNCATE,     &
            MPI_ERR_TYPE,         &
            MPI_INFO_NULL,        &
            MPI_INTEGER,          &
            MPI_INTEGER4,         &
            MPI_INTEGER8,         &
            MPI_LOGICAL,          &
            MPI_LAND,             &
            MPI_LOR,              &
            MPI_LXOR,             &
            MPI_MAX,              &
            MPI_MIN,              &
            MPI_MAXLOC,           &
            MPI_MINLOC,           &
            MPI_OFFSET_KIND
  PUBLIC :: MPI_PACKED,           &
            MPI_PROC_NULL,        &
            MPI_PROD,             &
            MPI_REAL,             &
            MPI_REAL4,            &
            MPI_REAL8,            &
            MPI_SIZEOF,           &
            MPI_SOURCE,           &
            MPI_STATUS_SIZE,      &
            MPI_SUCCESS,          &
            MPI_SUM,              &
            MPI_TAG,              &
            MPI_TAG_UB,           &
            MPI_UNDEFINED,        &
            MPI_WTICK,            &
            MPI_WTIME

! INCLUDE 'mpif.h'

END MODULE IncludeMPIImplementation
