/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>-------------------------------------------------------------------
**:FILE:         top_types.h
**:DESCRIPTION:  Variable types for TOP: Table Operators
**:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:         -- STILL IN DEVELOPMENT --
**:HISTORY:      23oct96-v001a-cet- make it work
**:HISTORY:      13jun96-v000a-cet- creation
**:<-------------------------------------------------------------------
*/
#ifndef TOP_TYPES_H
#define TOP_TYPES_H

/*-------------------------------------------- INCLUDES             -*/
#include "top_macros.h"

/*-------------------------------------------- CORBA                -*/
#ifdef CORBA
#include "top_i.hh"
#endif

/*-------------------------------------------- TYPEDEFS             -*/

/*-------------------------------------------- GLOBALS              -*/
/*-------------------------------------------- PROTOTYPES           -*/
extern CC_P int top_init();
extern CC_P int top_start();
extern CC_P int top_stop();

#ifndef NOKUIP
extern CC_P void top_def_();

/*-------------------------------------------- KUIP ACTION MODULES  -*/
extern CC_P void kam_top_count_();
extern CC_P void kam_top_list_();
extern CC_P void kam_top_newproject_();
extern CC_P void kam_top_newjoin_();
extern CC_P void kam_top_newsort_();

extern CC_P void kam_top_newcut_();
extern CC_P void kam_topcut_cut_();
extern CC_P void kam_topcut_filter_();
extern CC_P void kam_topcut_function_();


extern CC_P void kam_topproject_selectspec_();
extern CC_P void kam_topproject_project_();
extern CC_P void kam_topproject_reset_();
extern CC_P void kam_topjoin_selectspec_();
extern CC_P void kam_topjoin_whereclause_();
extern CC_P void kam_topjoin_join_();
extern CC_P void kam_topjoin_reset_();
/*-*/
extern CC_P STAFCV_T top_count();
extern CC_P STAFCV_T top_list();
extern CC_P STAFCV_T top_newproject(char* agent, char* select);
extern CC_P STAFCV_T top_newjoin(char* agent, char* select, char* where);

extern CC_P STAFCV_T top_newcut(char *agent,char *cfunc);
extern CC_P STAFCV_T topcut_cut(char*,char*,char*);
extern CC_P STAFCV_T topcut_filter(char*,char*,char*,char*);
extern CC_P STAFCV_T topcut_function(char*);

extern CC_P STAFCV_T top_newsort();
extern CC_P STAFCV_T topproject_selectspec(char* agent, char* select);
extern CC_P STAFCV_T topproject_project(char* agent, char* table1, char* table2
		, char* select);
extern CC_P STAFCV_T topproject_reset(char* agent);
extern CC_P STAFCV_T topjoin_selectspec(char* agent, char* select);
extern CC_P STAFCV_T topjoin_whereclause(char* agent, char* where);
extern CC_P STAFCV_T topjoin_join(char* agent, char* table1, char* table2
		, char* table3, char* select, char* where);
extern CC_P STAFCV_T topjoin_reset(char* agent);
/*-*/

#endif /*NOKUIP*/

#endif /* TOP_TYPES_H */

