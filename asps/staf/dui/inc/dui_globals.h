/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dui_globals.h
*:DESCRIPTION:  Global variables for DUI
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      29nov95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#ifndef DUI_GLOBALS_H
#define DUI_GLOBALS_H

/*-------------------------------------------- INCLUDES             --*/
#include "dstype.h"
#include "dui_macros.h"
#include "dui_types.h"
#include "duiClasses.hh"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
#ifdef __cplusplus
   extern duiFactory *dui;
#endif /*__cplusplus*/
/*- This is a global that may not be preserved. -*/
   extern DS_DATASET_T *dui_pDScwd;	/* HACK - REMOVE !!! */

/*-------------------------------------------- PROTOTYPES           --*/

#endif /* DUI_GLOBALS_H */

