/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        spx_macros.h
**:DESCRIPTION: Service Package eXample template  header file
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     25nov95-v000b-cet- rename from SPX.h to spx_macros.h
**:HISTORY:     21nov95-v000a-cet- recreation
**:<--------------------------------------------------------------------
*/
#ifndef SPX_MACROS_H
#define SPX_MACROS_H

#define SPX Service Package eXample

/*-------------------------------------------- INCLUDES             --*/
#include "soc_macros.h"

/*-------------------------------------------- MACROS               --*/

#define SPXGRID(OBJ) ((spxGrid*)(OBJ->ptr()))
#define SPXDUMMY(OBJ) ((spxDummy*)(OBJ->ptr()))
#define SPXFACORY(OBJ) ((spxFactory*)(OBJ->ptr()))

#endif /* SPX_MACROS_H */

