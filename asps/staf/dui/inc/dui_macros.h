/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        dui_macros.h
**:DESCRIPTION: Analysis Service Package template  header file
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     29nov95-v000a-cet- creation
**:<--------------------------------------------------------------------
*/
#ifndef DUI_MACROS_H
#define DUI_MACROS_H

#define DUI Table and Dataset Memory

/*-------------------------------------------- INCLUDES             --*/
#include "soc_macros.h"

/*-------------------------------------------- MACROS               --*/

#define DUIFACTORY(OBJ) ((duiFactory*)(OBJ->ptr()))

#endif /* DUI_MACROS_H */

