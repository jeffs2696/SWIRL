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

MODULE MessagePassingInterface

  USE IncludeMPIImplementation, ONLY : MPI_ADDRESS_KIND,     &
                                       MPI_ANY_SOURCE,       &
                                       MPI_ANY_TAG,          &
                                       MPI_CHARACTER,        &
                                       MPI_COMM_SELF,        &
                                       MPI_COMM_WORLD,       &
                                       MPI_DOUBLE_PRECISION, &
                                       MPI_ERROR,            &
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
                                       MPI_SOURCE,           &
                                       MPI_STATUS_SIZE,      &
                                       MPI_SUCCESS,          &
                                       MPI_SUM

  USE IncludeMPIImplementation, ONLY : MPI_TAG,              &
                                       MPI_TAG_UB,           &
                                       MPI_UNDEFINED
                                     
  USE MPIKindDefs

  USE MPI_ABORTModule
  USE MPI_ALLGATHERCharacterModule
  USE MPI_ALLGATHERReal8Module
  USE MPI_ALLGATHERInt4Module
  USE MPI_ALLGATHERInt8Module
  USE MPI_ALLGATHERLogicalModule
  USE MPI_ALLGATHERReal4Module
  USE MPI_ALLGATHERCharacterModule0D
  USE MPI_ALLGATHERReal8Module0D
  USE MPI_ALLGATHERInt4Module0D
  USE MPI_ALLGATHERInt8Module0D
  USE MPI_ALLGATHERLogicalModule0D
  USE MPI_ALLGATHERReal4Module0D
  USE MPI_ALLREDUCEReal8Module
  USE MPI_ALLREDUCEInt4Module
  USE MPI_ALLREDUCEInt8Module
  USE MPI_ALLREDUCELogicalModule
  USE MPI_ALLREDUCEReal4Module
  USE MPI_ALLREDUCEReal8Module0D
  USE MPI_ALLREDUCEInt4Module0D
  USE MPI_ALLREDUCEInt8Module0D
  USE MPI_ALLREDUCELogicalModule0D
  USE MPI_ALLREDUCEReal4Module0D
  USE MPI_ALLTOALLVReal8Module
  USE MPI_BARRIERModule
  USE MPI_BCASTCharacterModule
  USE MPI_BCASTInt4Module
  USE MPI_BCASTInt8Module
  USE MPI_BCASTLogicalModule
  USE MPI_BCASTReal4Module
  USE MPI_BCASTReal8Module
  USE MPI_BCASTCharacterModule0D
  USE MPI_BCASTInt4Module0D
  USE MPI_BCASTInt8Module0D
  USE MPI_BCASTLogicalModule0D
  USE MPI_BCASTReal4Module0D
  USE MPI_BCASTReal8Module0D
  USE MPI_CART_CREATEModule
  USE MPI_CART_SHIFTModule
  USE MPI_COMM_CREATEModule
  USE MPI_COMM_GET_ATTRModule
  USE MPI_COMM_GROUPModule
  USE MPI_COMM_RANKModule
  USE MPI_COMM_SIZEModule
  USE MPI_COMM_SPLITModule
  USE MPI_ERRHANDLER_SETModule
  USE MPI_ERROR_STRINGModule
  USE MPI_FINALIZEModule
  USE MPI_FINALIZEDModule
  USE MPI_GATHERCharacterModule
  USE MPI_GATHERCharacterModule0D
  USE MPI_GATHERReal8Module
  USE MPI_GATHERReal8Module0D
  USE MPI_GATHERInt4Module
  USE MPI_GATHERInt8Module
  USE MPI_GATHERInt4Module0D
  USE MPI_GATHERInt8Module0D
  USE MPI_GATHERLogicalModule
  USE MPI_GATHERLogicalModule0D
  USE MPI_GATHERReal4Module
  USE MPI_GATHERReal4Module0D
  USE MPI_GET_ADDRESSCharacterModule
  USE MPI_GET_ADDRESSInt4Module
  USE MPI_GET_ADDRESSInt8Module
  USE MPI_GET_ADDRESSLogicalModule
  USE MPI_GET_ADDRESSReal4Module
  USE MPI_GET_ADDRESSReal8Module
  USE MPI_GET_ADDRESSCharModule0D
  USE MPI_GET_ADDRESSInt4Module0D
  USE MPI_GET_ADDRESSInt8Module0D
  USE MPI_GET_ADDRESSLogicalModule0D
  USE MPI_GET_ADDRESSReal4Module0D
  USE MPI_GET_ADDRESSReal8Module0D
  USE MPI_GET_COUNTModule
  USE MPI_GET_VERSIONModule
  USE MPI_GROUP_INCLModule
  USE MPI_GROUP_RANKModule
  USE MPI_GROUP_SIZEModule
  USE MPI_INITModule
  USE MPI_INITIALIZEDModule
  USE MPI_INTERCOMM_CREATEModule
  USE MPI_IPROBEModule
  USE MPI_IRECVCharacterModule
  USE MPI_IRECVInt4Module
  USE MPI_IRECVInt8Module
  USE MPI_IRECVLogicalModule
  USE MPI_IRECVReal4Module
  USE MPI_IRECVReal8Module
  USE MPI_IRECVCharacterModule0D
  USE MPI_IRECVInt4Module0D
  USE MPI_IRECVInt8Module0D
  USE MPI_IRECVLogicalModule0D
  USE MPI_IRECVReal4Module0D
  USE MPI_IRECVReal8Module0D
  USE MPI_ISENDCharacterModule
  USE MPI_ISENDInt4Module
  USE MPI_ISENDInt8Module
  USE MPI_ISENDLogicalModule
  USE MPI_ISENDReal4Module
  USE MPI_ISENDReal8Module
  USE MPI_ISENDCharacterModule0D
  USE MPI_ISENDInt4Module0D
  USE MPI_ISENDInt8Module0D
  USE MPI_ISENDLogicalModule0D
  USE MPI_ISENDReal4Module0D
  USE MPI_ISENDReal8Module0D
  USE MPI_PROBEModule
  USE MPI_RECVCharacterModule
  USE MPI_RECVInt4Module
  USE MPI_RECVInt8Module
  USE MPI_RECVLogicalModule
  USE MPI_RECVReal4Module
  USE MPI_RECVReal8Module
  USE MPI_RECVCharacterModule0D
  USE MPI_RECVInt4Module0D
  USE MPI_RECVInt8Module0D
  USE MPI_RECVLogicalModule0D
  USE MPI_RECVReal4Module0D
  USE MPI_RECVReal8Module0D
  USE MPI_SENDCharacterModule
  USE MPI_SENDInt4Module
  USE MPI_SENDInt8Module
  USE MPI_SENDLogicalModule
  USE MPI_SENDReal4Module
  USE MPI_SENDReal8Module
  USE MPI_SENDCharacterModule0D
  USE MPI_SENDInt4Module0D
  USE MPI_SENDInt8Module0D
  USE MPI_SENDLogicalModule0D
  USE MPI_SENDReal4Module0D
  USE MPI_SENDReal8Module0D
  USE MPI_SENDRECVCharacterModule
  USE MPI_SENDRECVInt4Module
  USE MPI_SENDRECVInt8Module
  USE MPI_SENDRECVLogicalModule
  USE MPI_SENDRECVReal4Module
  USE MPI_SENDRECVReal8Module
  USE MPI_SENDRECVCharacterModule0D
  USE MPI_SENDRECVInt4Module0D
  USE MPI_SENDRECVInt8Module0D
  USE MPI_SENDRECVLogicalModule0D
  USE MPI_SENDRECVReal4Module0D
  USE MPI_SENDRECVReal8Module0D
  USE MPI_SIZEOFCharModule
  USE MPI_SIZEOFInt4Module
  USE MPI_SIZEOFInt8Module
  USE MPI_SIZEOFLogicalModule
  USE MPI_SIZEOFReal4Module
  USE MPI_SIZEOFReal8Module
  USE MPI_TYPE_COMMITModule
  USE MPI_TYPE_CREATE_STRUCTModule
  USE MPI_TYPE_FREEModule
  USE MPI_TYPE_GET_CONTENTSModule
  USE MPI_TYPE_GET_EXTENTModule
  USE MPI_TYPE_GET_ENVELOPEModule
  USE MPI_TYPE_SIZEModule
  USE MPI_TESTModule
  USE MPI_WAITModule
  USE MPI_WTICKModule
  USE MPI_WTIMEModule

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: HTMPI_ABORT

  PUBLIC :: int4Range,              &
            int8Range,              &
            real4Precision,         &
            real8Precision,         &
            int4Kind,               &
            int8Kind,               &
            real4Kind,              &
            real8Kind,              &
            GetMPITypeDef
            
  PUBLIC :: MPI_ADDRESS_KIND,       &
            HTMPI_ALLGATHER,        &
            HTMPI_ALLREDUCE,        &
            HTMPI_ALLTOALLV,        &
            MPI_ANY_SOURCE,         &
            MPI_ANY_TAG

  PUBLIC :: HTMPI_BARRIER,          &
            HTMPI_BCAST

  PUBLIC :: MPI_CHARACTER,          &
            HTMPI_CART_CREATE,      &
            HTMPI_CART_SHIFT,       &
            HTMPI_COMM_CREATE,      &
            HTMPI_COMM_GET_ATTR,    &
            HTMPI_COMM_GROUP,       &
            HTMPI_COMM_RANK,        &
            HTMPI_COMM_SIZE,        &
            HTMPI_COMM_SPLIT,       &
            MPI_COMM_SELF,          &
            MPI_COMM_WORLD

  PUBLIC :: MPI_DOUBLE_PRECISION

  PUBLIC :: MPI_ERROR,              &
            MPI_ERRORS_RETURN,      &
            HTMPI_ERRHANDLER_SET,   &
            HTMPI_ERROR_STRING,     &
            MPI_ERR_BUFFER,         &
            MPI_ERR_COMM,           &
            MPI_ERR_COUNT,          &
            MPI_ERR_RANK,           &
            MPI_ERR_TAG,            &
            MPI_ERR_TRUNCATE,       &
            MPI_ERR_TYPE

  PUBLIC :: HTMPI_FINALIZE,         &
            HTMPI_FINALIZED

  PUBLIC :: HTMPI_GATHER,           &
            HTMPI_GET_ADDRESS,      &
            HTMPI_GET_COUNT,        &
            HTMPI_GET_VERSION,      &
            HTMPI_GROUP_INCL,       &
            HTMPI_GROUP_RANK,       &
            HTMPI_GROUP_SIZE

  PUBLIC :: MPI_INFO_NULL,          &
            HTMPI_INIT,             &
            HTMPI_INITIALIZED,      &
            HTMPI_INTERCOMM_CREATE, &
            MPI_INTEGER,            &
            MPI_INTEGER4,           &
            MPI_INTEGER8,           &
            HTMPI_IPROBE,           &
            HTMPI_IRECV,            &
            HTMPI_ISEND

  PUBLIC :: MPI_LOGICAL,            &
            MPI_LAND,               &
            MPI_LOR,                &
            MPI_LXOR

  PUBLIC :: MPI_MAX,                &
            MPI_MIN,                &
            MPI_MAXLOC,             &
            MPI_MINLOC

  PUBLIC :: MPI_OFFSET_KIND

  PUBLIC :: HTMPI_PROBE,            &
            MPI_PROC_NULL,          &
            MPI_PROD

  PUBLIC :: MPI_REAL,               &
            MPI_REAL4,              &
            MPI_REAL8,              &
            HTMPI_RECV

  PUBLIC :: HTMPI_SEND,             &
            HTMPI_SENDRECV,         &
            HTMPI_SIZEOF,           &
            MPI_SOURCE,             &
            MPI_STATUS_SIZE,        &
            MPI_SUCCESS,            &
            MPI_SUM

  PUBLIC :: MPI_TAG,                  &
            MPI_TAG_UB,               &
            HTMPI_TEST,               &
            HTMPI_TYPE_COMMIT,        &
            HTMPI_TYPE_CREATE_STRUCT, &
            HTMPI_TYPE_FREE,          &
            HTMPI_TYPE_GET_CONTENTS,  &
            HTMPI_TYPE_GET_ENVELOPE,  &
            HTMPI_TYPE_GET_EXTENT,    &
            HTMPI_TYPE_SIZE

  PUBLIC :: MPI_UNDEFINED

  PUBLIC :: HTMPI_WAIT,               &
            HTMPI_WTICK,              &
            HTMPI_WTIME

END MODULE MessagePassingInterface
