/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>-------------------------------------------------------------------
**:FILE:         tnt_types.h
**:DESCRIPTION:  Variable types for TNT: Tables to NTuples
**:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:         -- STILL IN DEVELOPMENT --
**:HISTORY:      13jun96-v000a-cet- creation
**:<-------------------------------------------------------------------
*/
#ifndef TNT_TYPES_H
#define TNT_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#include "tnt_macros.h"

/*-------------------------------------------- CORBA                --*/
#ifdef CORBA
#include "tnt_i.hh"
#endif

/*-------------------------------------------- TYPEDEFS             --*/
/*-- 
#ifndef NT_TYPE_CODE_T
typedef enum nt_type_code_t {
   NT_TYPE_CHAR,
   NT_TYPE_LOGICAL,
   NT_TYPE_LONG,
   NT_TYPE_U_LONG,
   NT_TYPE_LONGLONG,
   NT_TYPE_U_LONGLONG,
   NT_TYPE_FLOAT,
   NT_TYPE_DOUBLE,
   NT_TYPE_UNKNOWN
}NT_TYPE_CODE_T;
#endif ** NT_TYPE_CODE_T **
--*/

/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P int tnt_init();
extern CC_P int tnt_start();
extern CC_P int tnt_stop();

#ifndef NOKUIP
extern CC_P void tnt_def_();

extern CC_P void kam_tnt_count_();
extern CC_P void kam_tnt_list_();
extern CC_P void kam_tnt_paw_();
extern CC_P void kam_tnt_share_();
extern CC_P void kam_tnt_newcwntuple_();
extern CC_P void kam_tntcwntuple_hid_();
extern CC_P void kam_tntcwntuple_title_();
extern CC_P void kam_tntcwntuple_entrycount_();
extern CC_P void kam_tntcwntuple_columncount_();
extern CC_P void kam_tntcwntuple_import_();
extern CC_P void kam_tntcwntuple_clear_();
extern CC_P void kam_tntcwntuple_zebradir_();
extern CC_P void kam_tntcwntuple_gettable_();
extern CC_P void kam_tntcwntuple_append_();
extern CC_P void kam_tntcwntuple_puttable_();
extern CC_P void kam_tntcwntuple_show_();
extern CC_P void kam_tntcwntuple_print_();
/*-*/
extern CC_P STAFCV_T tnt_count();
extern CC_P STAFCV_T tnt_list();
extern CC_P STAFCV_T tnt_paw();
extern CC_P STAFCV_T tnt_share();
extern CC_P STAFCV_T tnt_newcwntuple(long hid, char* tname);
extern CC_P STAFCV_T tntcwntuple_hid(long hid);
extern CC_P STAFCV_T tntcwntuple_title(long hid);
extern CC_P STAFCV_T tntcwntuple_entrycount(long hid);
extern CC_P STAFCV_T tntcwntuple_columncount(long hid);
extern CC_P STAFCV_T tntcwntuple_clear(long hid);
extern CC_P STAFCV_T tntcwntuple_import(long hid, char* tname);
extern CC_P STAFCV_T tntcwntuple_zebradir(long hid);
extern CC_P STAFCV_T tntcwntuple_gettable(long hid, char* tname);
extern CC_P STAFCV_T tntcwntuple_append(long hid, char* tname);
extern CC_P STAFCV_T tntcwntuple_puttable(long hid, char* tname);
extern CC_P STAFCV_T tntcwntuple_show(long hid);
extern CC_P STAFCV_T tntcwntuple_print(long hid);
#endif /*NOKUIP*/

#endif /* TNT_TYPES_H */

