/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dui_types.h
*:DESCRIPTION:  Variable types for DUI
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      08dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#ifndef DUI_TYPES_H
#define DUI_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#include "asuLib.h"
#include "dui_macros.h"
#include "dstype.h"

#ifdef CORBA
#include "dui_i.hh"
#endif

/*-------------------------------------------- CORBA                --*/

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P char * dui_dirof(const char* path);
extern CC_P char * dui_notdirof(const char* path);
extern CC_P char * dui_pathof(const char* base,const char* mod);
extern CC_P char * dui_rootof(const char* path);

#ifdef __cplusplus
extern CC_P int duiFindDS(DS_DATASET_T *& node, DS_DATASET_T* root
		, char* path);
extern CC_P int dui_ls_l_Table(DS_DATASET_T *pDS, char*& listing);
extern CC_P int dui_ls_ld_Dataset(DS_DATASET_T *pDS, char*& listing);
extern CC_P int dui_ls_l_Dataset(DS_DATASET_T *pDS, char*& listing);
extern CC_P int dui_ls_l_Header(char*& listing);
#endif /*__cplusplus*/

extern CC_P int dui_init();
extern CC_P int dui_start();
extern CC_P int dui_stop();

#ifndef NOKUIP
extern CC_P void dui_def_();

extern CC_P void kam_dui_precious_();
extern CC_P void kam_dui_rm_nonprecious_();
extern CC_P void kam_dui_count_();
extern CC_P void kam_dui_list_();
extern CC_P void kam_dui_cd_();
extern CC_P void kam_dui_du_();
extern CC_P void kam_dui_df_();
extern CC_P void kam_dui_ln_();
extern CC_P void kam_dui_cp_();
extern CC_P void kam_dui_append_();
extern CC_P void kam_dui_ls_();
extern CC_P void kam_dui_mkdir_();
extern CC_P void kam_dui_mktable_();
extern CC_P void kam_dui_mv_();
extern CC_P void kam_dui_pwd_();
extern CC_P void kam_dui_rm_();
extern CC_P void kam_dui_rmdir_();
#endif /*NOKUIP*/

extern CC_P STAFCV_T dui_precious();
extern CC_P STAFCV_T dui_rm_nonprecious();
extern CC_P STAFCV_T dui_count_();
extern CC_P STAFCV_T dui_list_();
extern CC_P STAFCV_T dui_du();
extern CC_P STAFCV_T dui_df(char *markerString);
extern CC_P STAFCV_T dui_cd(char* path);
extern CC_P STAFCV_T dui_ln(char* fromPath, char* toPath);
extern CC_P STAFCV_T dui_cp(char* fromPath, char* toPath);
extern CC_P STAFCV_T dui_append(char* fromPath, char* toPath);
extern CC_P STAFCV_T dui_ls(char* path);
extern CC_P STAFCV_T dui_mkdir(char* path);
extern CC_P STAFCV_T dui_mktable(char* name,char* spec,long rowcount);
extern CC_P STAFCV_T dui_mv(char* fromPath, char* toPath);
extern CC_P STAFCV_T dui_pwd();
extern CC_P STAFCV_T dui_rm(char* path);
extern CC_P STAFCV_T dui_rmdir(char* path);


#endif /* DUI_TYPES_H */

