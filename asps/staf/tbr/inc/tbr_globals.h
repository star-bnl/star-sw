/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tbr_globals.h
*:DESCRIPTION:  Global variables for TBR
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      11mar96-v000a-cet,hjw- creation
*:<---------------------------------------------------------------------
*/
#ifndef TBR_GLOBALS_H
#define TBR_GLOBALS_H

/*-------------------------------------------- INCLUDES             --*/
#include "tbr_macros.h"
#include "tbr_types.h"
#ifdef __cplusplus
#include "tbrClasses.hh"
#endif /*__cplusplus*/

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
#ifdef __cplusplus
   extern tbrFactory *tbr;
   extern tbrMotifViewer *tbr_MotifViewer;      /* HACK */
#endif /*__cplusplus*/

/*-------------------------------------------- PROTOTYPES           --*/

#endif /* TBR_GLOBALS_H */

