/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tbr_types.h
*:DESCRIPTION:  Variable types for TBR
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      11mar96-v000a-cet,hjw- creation
*:<---------------------------------------------------------------------
*/
#ifndef TBR_TYPES_H
#define TBR_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#include "dstype.h"
#include "tdmClasses.hh"
#include "tbr_macros.h"

#ifdef CORBA
#include "tbr_i.hh"
#endif

/*-------------------------------------------- CORBA                --*/
#ifndef CORBA

#endif /*CORBA*/

/*-------------------------------------------- TYPEDEFS             --*/
#ifndef CORBA

#endif /*CORBA*/

/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P int tbr_init();
extern CC_P int tbr_start();
extern CC_P int tbr_stop();
extern CC_P void tbr_def_();
extern CC_P void tbrProgress(int ,int, char *, char *);
extern CC_P void DoXStuff();
extern CC_P void GetRidOfWindows();
#ifndef NOKUIP
extern CC_P void kam_tbr_count_();
extern CC_P void kam_tbr_list_();
extern CC_P void kam_tbr_viewdataset_();
#endif /*NOKUIP*/

extern CC_P STAFCV_T tbr_count();
extern CC_P STAFCV_T tbr_list();
extern CC_P STAFCV_T tbr_viewdataset();

#endif /* TBR_TYPES_H */

