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
extern CC_P void spx_def_();

#ifndef NOKUIP
extern CC_P void kam_spx_deletedummy_();
extern CC_P void kam_spx_deletegrid_();
extern CC_P void kam_spx_finddummy_();
extern CC_P void kam_spx_findgrid_();
extern CC_P void kam_spx_list_();
extern CC_P void kam_spx_listobjects_();
extern CC_P void kam_spx_newdummy_();
extern CC_P void kam_spx_newgrid_();
extern CC_P void kam_spx_count_();
extern CC_P void kam_spxdummy_ncalls_();
extern CC_P void kam_spxdummy_hello_();
extern CC_P void kam_spxdummy_null_();
extern CC_P void kam_spxdummy_time_();
extern CC_P void kam_spxgrid_get_();
extern CC_P void kam_spxgrid_height_();
extern CC_P void kam_spxgrid_set_();
extern CC_P void kam_spxgrid_width_();
extern CC_P int kam_spx_deletedummy();
extern CC_P int kam_spx_deletegrid();
extern CC_P int kam_spx_finddummy();
extern CC_P int kam_spx_findgrid();
extern CC_P int kam_spx_list();
extern CC_P int kam_spx_listobjects();
extern CC_P int kam_spx_newdummy();
extern CC_P int kam_spx_newgrid();
extern CC_P int kam_spx_count();
extern CC_P int kam_spxdummy_ncalls();
extern CC_P int kam_spxdummy_hello();
extern CC_P int kam_spxdummy_null();
extern CC_P int kam_spxdummy_time();
extern CC_P int kam_spxgrid_get();
extern CC_P int kam_spxgrid_height();
extern CC_P int kam_spxgrid_set();
extern CC_P int kam_spxgrid_width();
#endif /*NOKUIP*/

#endif /* SPX_TYPES_H */

