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
extern CC_P void kam_top_newfilter_();
extern CC_P void kam_top_newsort_();
extern CC_P void kam_topproject_selectspec_();
extern CC_P void kam_topproject_project_();
extern CC_P void kam_topproject_reset_();
extern CC_P void kam_topjoin_selectspec_();
extern CC_P void kam_topjoin_whereclause_();
extern CC_P void kam_topjoin_join_();
extern CC_P void kam_topjoin_reset_();
/*-*/
extern CC_P int kam_top_count();
extern CC_P int kam_top_list();
extern CC_P int kam_top_newproject();
extern CC_P int kam_top_newjoin();
extern CC_P int kam_top_newfilter();
extern CC_P int kam_top_newsort();
extern CC_P int kam_topproject_selectspec();
extern CC_P int kam_topproject_project();
extern CC_P int kam_topproject_reset();
extern CC_P int kam_topjoin_selectspec();
extern CC_P int kam_topjoin_whereclause();
extern CC_P int kam_topjoin_join();
extern CC_P int kam_topjoin_reset();

#endif /*NOKUIP*/

#endif /* TOP_TYPES_H */

