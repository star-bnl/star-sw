      INTEGER FUNCTION TAS_USER_TABLES ()
      IMPLICIT NONE
C   Input arguments  (One to a line with definition after ! ) 
C   Output arguments : 
C   Returned value  : 
C     Standard TAS condition values
C   Functional Description : 
C     User entry point for table initializations.
C *******************************************************************
C   Generated automatically by tas_gen. Do NOT Edit!!!!
C********************************************************************
C
C   Error conditions : 
C----------------------------------------------------------------------
#include "tas_ecvalues.inc"
C----------------------------------------------------------------------
      INTEGER status
C----------------------------------------------------------------------
      INTEGER ctg_REGISTER_TABLES
      INTEGER ctg_MSG_ID
      INTEGER cts_REGISTER_TABLES
      INTEGER cts_MSG_ID
      INTEGER ctu_REGISTER_TABLES
      INTEGER ctu_MSG_ID
      INTEGER mct_REGISTER_TABLES
      INTEGER mct_MSG_ID
      INTEGER mwg_REGISTER_TABLES
      INTEGER mwg_MSG_ID
      INTEGER mws_REGISTER_TABLES
      INTEGER mws_MSG_ID
      INTEGER mwu_REGISTER_TABLES
      INTEGER mwu_MSG_ID
      INTEGER rl0_REGISTER_TABLES
      INTEGER rl0_MSG_ID
      INTEGER rl1_REGISTER_TABLES
      INTEGER rl1_MSG_ID
      INTEGER vpm_REGISTER_TABLES
      INTEGER vpm_MSG_ID
      INTEGER vps_REGISTER_TABLES
      INTEGER vps_MSG_ID
      INTEGER vpv_REGISTER_TABLES
      INTEGER vpv_MSG_ID
C--------------------------------------------------------------------
      LOGICAL SCV_STATE
C----------------------------------------------------------------------
      DATA ctg_MSG_ID/0/
      DATA cts_MSG_ID/0/
      DATA ctu_MSG_ID/0/
      DATA mct_MSG_ID/0/
      DATA mwg_MSG_ID/0/
      DATA mws_MSG_ID/0/
      DATA mwu_MSG_ID/0/
      DATA rl0_MSG_ID/0/
      DATA rl1_MSG_ID/0/
      DATA vpm_MSG_ID/0/
      DATA vps_MSG_ID/0/
      DATA vpv_MSG_ID/0/
C----------------------------------------------------------------------
C  Call database generated table initialization routines.
C
      status = tas_ok_cv
C  ctg library -----------------------
C     
      call ctg_tables_init
      status = ctg_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-ctg error'//
     +        ' from ctg_REGISTER_TABLES',
     +        1,ctg_MSG_ID)
         goto 999
      endif
C     
C  cts library -----------------------
C     
      call cts_tables_init
      status = cts_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-cts error'//
     +        ' from cts_REGISTER_TABLES',
     +        1,cts_MSG_ID)
         goto 999
      endif
C     
C  ctu library -----------------------
C     
      call ctu_tables_init
      status = ctu_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-ctu error'//
     +        ' from ctu_REGISTER_TABLES',
     +        1,ctu_MSG_ID)
         goto 999
      endif
C     
C  mct library -----------------------
C     
      call mct_tables_init
      status = mct_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-mct error'//
     +        ' from mct_REGISTER_TABLES',
     +        1,mct_MSG_ID)
         goto 999
      endif
C     
C  mwg library -----------------------
C     
      call mwg_tables_init
      status = mwg_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-mwg error'//
     +        ' from mwg_REGISTER_TABLES',
     +        1,mwg_MSG_ID)
         goto 999
      endif
C     
C  mws library -----------------------
C     
      call mws_tables_init
      status = mws_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-mws error'//
     +        ' from mws_REGISTER_TABLES',
     +        1,mws_MSG_ID)
         goto 999
      endif
C     
C  mwu library -----------------------
C     
      call mwu_tables_init
      status = mwu_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-mwu error'//
     +        ' from mwu_REGISTER_TABLES',
     +        1,mwu_MSG_ID)
         goto 999
      endif
C     
C  rl0 library -----------------------
C     
      call rl0_tables_init
      status = rl0_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-rl0 error'//
     +        ' from rl0_REGISTER_TABLES',
     +        1,rl0_MSG_ID)
         goto 999
      endif
C     
C  rl1 library -----------------------
C     
      call rl1_tables_init
      status = rl1_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-rl1 error'//
     +        ' from rl1_REGISTER_TABLES',
     +        1,rl1_MSG_ID)
         goto 999
      endif
C     
C  vpm library -----------------------
C     
      call vpm_tables_init
      status = vpm_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-vpm error'//
     +        ' from vpm_REGISTER_TABLES',
     +        1,vpm_MSG_ID)
         goto 999
      endif
C     
C  vps library -----------------------
C     
      call vps_tables_init
      status = vps_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-vps error'//
     +        ' from vps_REGISTER_TABLES',
     +        1,vps_MSG_ID)
         goto 999
      endif
C     
C  vpv library -----------------------
C     
      call vpv_tables_init
      status = vpv_register_tables()
      if(.not.scv_state(status,'I')) then
         call message('TAS_USER_TABLES-vpv error'//
     +        ' from vpv_REGISTER_TABLES',
     +        1,vpv_MSG_ID)
         goto 999
      endif
C     
C----------------------------------------------------------------------

C---------------------------------------------------------------------
 999  TAS_USER_TABLES = status
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_mct_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for mctam
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER mctam
C---------------------------------------------------------------------
      data msgid /0/
C
      am_mct_if = mctam(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     %val( tables(4).hptr ), %val( tables(4).rptr ),
     +     %val( tables(5).hptr ), %val( tables(5).rptr ),
     +     %val( tables(6).hptr ), %val( tables(6).rptr ),
     +     %val( tables(7).hptr ), %val( tables(7).rptr ),
     +     %val( tables(8).hptr ), %val( tables(8).rptr ),
     +     %val( tables(9).hptr ), %val( tables(9).rptr ),
     +     %val( tables(10).hptr ), %val( tables(10).rptr ),
     +     %val( tables(11).hptr ), %val( tables(11).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_ctg_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for ctg_main
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER ctg_main
C---------------------------------------------------------------------
      data msgid /0/
C
      am_ctg_if = ctg_main(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_cts_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for cts_main
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER cts_main
C---------------------------------------------------------------------
      data msgid /0/
C
      am_cts_if = cts_main(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     %val( tables(4).hptr ), %val( tables(4).rptr ),
     +     %val( tables(5).hptr ), %val( tables(5).rptr ),
     +     %val( tables(6).hptr ), %val( tables(6).rptr ),
     +     %val( tables(7).hptr ), %val( tables(7).rptr ),
     +     %val( tables(8).hptr ), %val( tables(8).rptr ),
     +     %val( tables(9).hptr ), %val( tables(9).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_ctu_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for ctu_main
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER ctu_main
C---------------------------------------------------------------------
      data msgid /0/
C
      am_ctu_if = ctu_main(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     %val( tables(4).hptr ), %val( tables(4).rptr ),
     +     %val( tables(5).hptr ), %val( tables(5).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_mwg_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for mwg_main
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER mwg_main
C---------------------------------------------------------------------
      data msgid /0/
C
      am_mwg_if = mwg_main(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_mws_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for mws_main
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER mws_main
C---------------------------------------------------------------------
      data msgid /0/
C
      am_mws_if = mws_main(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     %val( tables(4).hptr ), %val( tables(4).rptr ),
     +     %val( tables(5).hptr ), %val( tables(5).rptr ),
     +     %val( tables(6).hptr ), %val( tables(6).rptr ),
     +     %val( tables(7).hptr ), %val( tables(7).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_mwu_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for mwu_main
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER mwu_main
C---------------------------------------------------------------------
      data msgid /0/
C
      am_mwu_if = mwu_main(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     %val( tables(4).hptr ), %val( tables(4).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_rl0_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for rl0_main
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER rl0_main
C---------------------------------------------------------------------
      data msgid /0/
C
      am_rl0_if = rl0_main(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     %val( tables(4).hptr ), %val( tables(4).rptr ),
     +     %val( tables(5).hptr ), %val( tables(5).rptr ),
     +     %val( tables(6).hptr ), %val( tables(6).rptr ),
     +     %val( tables(7).hptr ), %val( tables(7).rptr ),
     +     %val( tables(8).hptr ), %val( tables(8).rptr ),
     +     %val( tables(9).hptr ), %val( tables(9).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_vps_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for vpsim
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER vpsim
C---------------------------------------------------------------------
      data msgid /0/
C
      am_vps_if = vpsim(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_vpm_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for vptmn
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER vptmn
C---------------------------------------------------------------------
      data msgid /0/
C
      am_vpm_if = vptmn(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_vpv_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for vpvtx
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER vpvtx
C---------------------------------------------------------------------
      data msgid /0/
C
      am_vpv_if = vpvtx(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION am_rl1_if( 
     +            mode, ntables, tables, reserved)
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C     mode = calling mode, tu_mode_XXX parameter
C     ntables = number of tables in tables array
C     tables = array of table pointers
C     reserved = reserved for future use
C   Output arguments : 
C     values of tables
C   Returned value  : 
C     a value from TAS_USER_CODES.INC
C   Functional Description : 
C     analysis module interface routine for rl1
C     Generated automatically by tas_gen.   Do NOT Edit!!!
C
C   Error conditions : 
C---------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"
C----------------------------------------------------------------------
      INTEGER mode, ntables, reserved
      RECORD/ table_pointer_st/ tables(ntables)
C----------------------------------------------------------------------
      INTEGER msgid, dummy
C----------------------------------------------------------------------
      INTEGER rl1
C---------------------------------------------------------------------
      data msgid /0/
C
      am_rl1_if = rl1(
     +     %val( tables(1).hptr ), %val( tables(1).rptr ),
     +     %val( tables(2).hptr ), %val( tables(2).rptr ),
     +     %val( tables(3).hptr ), %val( tables(3).rptr ),
     +     %val( tables(4).hptr ), %val( tables(4).rptr ),
     +     %val( tables(5).hptr ), %val( tables(5).rptr ),
     +     dummy )
  999 CONTINUE
      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION TEP_AMINIT()
      IMPLICIT NONE
C   Input arguments (One to a line with definition after ! ) 
C   Output arguments : 
C   Returned value  : 
C     standard TAS condition values
C   Functional Description : 
C       Analysis module registration routine.
C       Generated automatically by tas_gen.  Do NOT Edit!!!
C
C   Error conditions : 
C----------------------------------------------------------------------
#include "tas_ecvalues.inc"
      
      INTEGER status
      INTEGER tep_register_module
      INTEGER tep_decl_tab
C
C--- mct section for module function mctam ------------------
      INTEGER mct_handle
      INTEGER am_mct_if
      EXTERNAL am_mct_if
C
C--- ctg section for module function ctg_main ------------------
      INTEGER ctg_handle
      INTEGER am_ctg_if
      EXTERNAL am_ctg_if
C
C--- cts section for module function cts_main ------------------
      INTEGER cts_handle
      INTEGER am_cts_if
      EXTERNAL am_cts_if
C
C--- ctu section for module function ctu_main ------------------
      INTEGER ctu_handle
      INTEGER am_ctu_if
      EXTERNAL am_ctu_if
C
C--- mwg section for module function mwg_main ------------------
      INTEGER mwg_handle
      INTEGER am_mwg_if
      EXTERNAL am_mwg_if
C
C--- mws section for module function mws_main ------------------
      INTEGER mws_handle
      INTEGER am_mws_if
      EXTERNAL am_mws_if
C
C--- mwu section for module function mwu_main ------------------
      INTEGER mwu_handle
      INTEGER am_mwu_if
      EXTERNAL am_mwu_if
C
C--- rl0 section for module function rl0_main ------------------
      INTEGER rl0_handle
      INTEGER am_rl0_if
      EXTERNAL am_rl0_if
C
C--- vps section for module function vpsim ------------------
      INTEGER vps_handle
      INTEGER am_vps_if
      EXTERNAL am_vps_if
C
C--- vpm section for module function vptmn ------------------
      INTEGER vpm_handle
      INTEGER am_vpm_if
      EXTERNAL am_vpm_if
C
C--- vpv section for module function vpvtx ------------------
      INTEGER vpv_handle
      INTEGER am_vpv_if
      EXTERNAL am_vpv_if
C
C--- rl1 section for module function rl1 ------------------
      INTEGER rl1_handle
      INTEGER am_rl1_if
      EXTERNAL am_rl1_if
C
C-----------------------------------------
      LOGICAL scv_state
C
C----------------------------------------------------------------------
      TEP_AMINIT = tas_ok_cv
C
C--------- mctam ---------------------
      status = tep_register_module( 'mct', 
     +     am_mct_if, mct_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- ctg_main ---------------------
      status = tep_register_module( 'ctg', 
     +     am_ctg_if, ctg_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- cts_main ---------------------
      status = tep_register_module( 'cts', 
     +     am_cts_if, cts_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- ctu_main ---------------------
      status = tep_register_module( 'ctu', 
     +     am_ctu_if, ctu_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- mwg_main ---------------------
      status = tep_register_module( 'mwg', 
     +     am_mwg_if, mwg_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- mws_main ---------------------
      status = tep_register_module( 'mws', 
     +     am_mws_if, mws_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- mwu_main ---------------------
      status = tep_register_module( 'mwu', 
     +     am_mwu_if, mwu_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- rl0_main ---------------------
      status = tep_register_module( 'rl0', 
     +     am_rl0_if, rl0_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- vpsim ---------------------
      status = tep_register_module( 'vps', 
     +     am_vps_if, vps_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- vptmn ---------------------
      status = tep_register_module( 'vpm', 
     +     am_vpm_if, vpm_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- vpvtx ---------------------
      status = tep_register_module( 'vpv', 
     +     am_vpv_if, vpv_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C--------- rl1 ---------------------
      status = tep_register_module( 'rl1', 
     +     am_rl1_if, rl1_handle, 0 )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) return
C----------- declare table dependencies ----------------------------------
      status = tep_decl_tab( mct_handle, 
     +    'mctrl', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mctrl'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mpart', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mpart'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mkine', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mkine'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mvert', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mvert'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mhits_tpc', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_tpc'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mhits_svt', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_svt'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mhits_mwc', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_mwc'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mhits_xtp', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_xtp'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mhits_emc', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_emc'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mhits_ctf', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_ctf'
	return
      endif
      status = tep_decl_tab( mct_handle, 
     +    'mhits_vpd', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_vpd'
	return
      endif
      status = tep_decl_tab( ctg_handle, 
     +    'ctf_geo', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_geo'
	return
      endif
      status = tep_decl_tab( ctg_handle, 
     +    'ctf_slat', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_slat'
	return
      endif
      status = tep_decl_tab( ctg_handle, 
     +    'ctf_cal', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_cal'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'mhits_ctf', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_ctf'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'mkine', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mkine'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'ctf_geo', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_geo'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'ctf_slat', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_slat'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'ctf_cal', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_cal'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'ctf_mpara', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_mpara'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'ctf_mevent', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_mevent'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'ctf_mslat', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_mslat'
	return
      endif
      status = tep_decl_tab( cts_handle, 
     +    'ctf_raw', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_raw'
	return
      endif
      status = tep_decl_tab( ctu_handle, 
     +    'ctf_geo', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_geo'
	return
      endif
      status = tep_decl_tab( ctu_handle, 
     +    'ctf_slat', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_slat'
	return
      endif
      status = tep_decl_tab( ctu_handle, 
     +    'ctf_cal', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_cal'
	return
      endif
      status = tep_decl_tab( ctu_handle, 
     +    'ctf_raw', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_raw'
	return
      endif
      status = tep_decl_tab( ctu_handle, 
     +    'ctf_cor', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_cor'
	return
      endif
      status = tep_decl_tab( mwg_handle, 
     +    'mwc_geo', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_geo'
	return
      endif
      status = tep_decl_tab( mwg_handle, 
     +    'mwc_cal', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_cal'
	return
      endif
      status = tep_decl_tab( mws_handle, 
     +    'mhits_mwc', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_mwc'
	return
      endif
      status = tep_decl_tab( mws_handle, 
     +    'mwc_geo', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_geo'
	return
      endif
      status = tep_decl_tab( mws_handle, 
     +    'mwc_cal', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_cal'
	return
      endif
      status = tep_decl_tab( mws_handle, 
     +    'mwc_mwpar', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_mwpar'
	return
      endif
      status = tep_decl_tab( mws_handle, 
     +    'mwc_mevent', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_mevent'
	return
      endif
      status = tep_decl_tab( mws_handle, 
     +    'mwc_sector', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_sector'
	return
      endif
      status = tep_decl_tab( mws_handle, 
     +    'mwc_raw', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_raw'
	return
      endif
      status = tep_decl_tab( mwu_handle, 
     +    'mwc_geo', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_geo'
	return
      endif
      status = tep_decl_tab( mwu_handle, 
     +    'mwc_cal', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_cal'
	return
      endif
      status = tep_decl_tab( mwu_handle, 
     +    'mwc_raw', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_raw'
	return
      endif
      status = tep_decl_tab( mwu_handle, 
     +    'mwc_cor', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_cor'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'ctf_geo', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_geo'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'mwc_geo', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_geo'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'ctf_raw', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ctf_raw'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'mwc_raw', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mwc_raw'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'vpd_time', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table vpd_time'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'rl0_ctrl', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table rl0_ctrl'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'rl0_data', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table rl0_data'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'rl0ctb_cal', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table rl0ctb_cal'
	return
      endif
      status = tep_decl_tab( rl0_handle, 
     +    'rl0mwc_cal', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table rl0mwc_cal'
	return
      endif
      status = tep_decl_tab( vps_handle, 
     +    'mhits_vpd', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table mhits_vpd'
	return
      endif
      status = tep_decl_tab( vps_handle, 
     +    'vpd_sim', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table vpd_sim'
	return
      endif
      status = tep_decl_tab( vpm_handle, 
     +    'vpd_sim', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table vpd_sim'
	return
      endif
      status = tep_decl_tab( vpm_handle, 
     +    'vpd_time', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table vpd_time'
	return
      endif
      status = tep_decl_tab( vpv_handle, 
     +    'vpd_time', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table vpd_time'
	return
      endif
      status = tep_decl_tab( vpv_handle, 
     +    'vpd_dst', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table vpd_dst'
	return
      endif
      status = tep_decl_tab( vpv_handle, 
     +    'vpd_vert', 'write' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table vpd_vert'
	return
      endif
      status = tep_decl_tab( rl1_handle, 
     +    'rl0_data', 'read' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table rl0_data'
	return
      endif
      status = tep_decl_tab( rl1_handle, 
     +    'ks_ctrl', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ks_ctrl'
	return
      endif
      status = tep_decl_tab( rl1_handle, 
     +    'ks_out', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table ks_out'
	return
      endif
      status = tep_decl_tab( rl1_handle, 
     +    'pw_ctrl', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table pw_ctrl'
	return
      endif
      status = tep_decl_tab( rl1_handle, 
     +    'pw_out', 'update' )
      TEP_AMINIT = status
      if(.not.scv_state(status,'I')) then
	print *,' TEP_AMINIT- failed to declare table pw_out'
	return
      endif
C
  999 CONTINUE
      RETURN
      END

