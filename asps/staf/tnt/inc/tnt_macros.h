/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:         tnt_macros.h
**:DESCRIPTION:  General macros for TNT-Tables to NTuples
**:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:         -- STILL IN DEVELOPMENT --
**:HISTORY:      13jun96-v000a-cet- creation
*:<--------------------------------------------------------------------
*/
#ifndef TNT_MACROS_H
#define TNT_MACROS_H

#define TNT Tables to NTuples

/*-------------------------------------------- INCLUDES             --*/
#include "asu_macros.h"

/*-------------------------------------------- MACROS               --*/

#define TNTNTUPLE(OBJ) ((tntNtuple*)(OBJ->ptr()))
#define TNTRWNTUPLE(OBJ) ((tntRWNtuple*)(OBJ->ptr()))
#define TNTCWNTUPLE(OBJ) ((tntCWNtuple*)(OBJ->ptr()))
#define TNTFACTORY(OBJ) ((tntFactory*)(OBJ->ptr()))

#endif /* TNT_MACROS_H */

