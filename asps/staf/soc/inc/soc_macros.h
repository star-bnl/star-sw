/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         soc_macros.h
*:DESCRIPTION:  General macros for SOC
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      25nov95-v000c-cet- rename from SOC.h to soc_macros.h
*:HISTORY:      20nov95-v000b-cet- include ASU.h
*:HISTORY:      14nov95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#ifndef SOC_MACROS_H
#define SOC_MACROS_H

#define SOC Service and Object Catalog

/*-------------------------------------------- INCLUDES             --*/
#include "stafGeneric.h"

/*-------------------------------------------- MACROS               --*/
#define FIRST_IDREF 0

#define VALID_IDREF(A) (0 <= A && A < soc->count())

#define SOCOBJECT(OBJ) ((socObject*)(OBJ->ptr()))
#define SOCCATALOG(OBJ) ((socCatalog*)(OBJ->ptr()))

#endif /* SOC_MACROS_H */

