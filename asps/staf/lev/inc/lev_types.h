/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         lev_types.h
*:DESCRIPTION:  Variable types for LEV
*:AUTHOR:       hjw - Herb Ward, ward@physics.utexas.edu
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      01jul96-v000a-hjw- creation
*:<---------------------------------------------------------------------
*/
#ifndef LEV_TYPES_H
#define LEV_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#include "dstype.h"
#include "tdmClasses.hh"
#include "lev_macros.h"

#ifdef CORBA
#include "lev_i.hh"
#endif

/*-------------------------------------------- CORBA                --*/
#ifndef CORBA

#endif /*CORBA*/

/*-------------------------------------------- TYPEDEFS             --*/
#ifndef CORBA

#endif /*CORBA*/

/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P int lev_init();
extern CC_P int lev_start();
extern CC_P int lev_stop();
extern CC_P void lev_def_();

#ifndef NOKUIP
extern CC_P void kam_lev_count_();
extern CC_P void kam_lev_list_();
extern CC_P void kam_lev_update_();
extern CC_P void kam_lev_register_version_();
#endif /*NOKUIP*/

#endif /* LEV_TYPES_H */

