/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:         spx_types.h
*:DESCRIPTION:  Variable types for SPX
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      14nov95-v000b-cet- rename from SPX.h
*:HISTORY:      26jul95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#ifndef SPX_TYPES_H
#define SPX_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#include "spx_macros.h"

#ifdef CORBA
#include "spx_i.hh"
#endif

/*-------------------------------------------- CORBA                --*/
/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P int spx_init();
extern CC_P int spx_start();
extern CC_P int spx_stop();

#ifndef NOKUIP
extern CC_P void spx_def_();

extern CC_P void kam_spx_count_();
extern CC_P void kam_spx_list_();
extern CC_P void kam_spx_newdummy_();
extern CC_P void kam_spx_newgrid_();
extern CC_P void kam_spxdummy_ncalls_();
extern CC_P void kam_spxdummy_null_();
extern CC_P void kam_spxdummy_time_();
extern CC_P void kam_spxgrid_get_();
extern CC_P void kam_spxgrid_height_();
extern CC_P void kam_spxgrid_set_();
extern CC_P void kam_spxgrid_width_();
#endif /*NOKUIP*/

extern CC_P STAFCV_T spx_count();
extern CC_P STAFCV_T spx_list();
extern CC_P STAFCV_T spx_newdummy(char* name);
extern CC_P STAFCV_T spx_newgrid(char* name, short height, short width);
extern CC_P STAFCV_T spxdummy_ncalls(char* name);
extern CC_P STAFCV_T spxdummy_null(char* name);
extern CC_P STAFCV_T spxdummy_time(char* name);
extern CC_P STAFCV_T spxgrid_get(char* name,short m,short n);
extern CC_P STAFCV_T spxgrid_height(char* name);
extern CC_P STAFCV_T spxgrid_set(char* name, short m, short n
		, long value);
extern CC_P STAFCV_T spxgrid_width(char* name);

#endif /* SPX_TYPES_H */

