/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:         top_macros.h
**:DESCRIPTION:  General macros for TOP-Table OPerators
**:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:         -- STILL IN DEVELOPMENT --
**:HISTORY:      13jun96-v000a-cet- creation
*:<--------------------------------------------------------------------
*/
#ifndef TOP_MACROS_H
#define TOP_MACROS_H

#define TOP Table OPerators

/*-------------------------------------------- INCLUDES             --*/
#include "asu_macros.h"

/*-------------------------------------------- MACROS               --*/

#define TOPPROJECT(OBJ) ((topProject*)(OBJ->ptr()))
#define TOPJOIN(OBJ) ((topJoin*)(OBJ->ptr()))
#define TOPSORT(OBJ) ((topSort*)(OBJ->ptr()))
#define TOPCUT(OBJ) ((topCut*)(OBJ->ptr()))
#define TOPFACTORY(OBJ) ((topFactory*)(OBJ->ptr()))

#endif /* TOP_MACROS_H */

