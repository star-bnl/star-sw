/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tdm_macros.h
**:DESCRIPTION: Analysis Service Package template  header file
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     29nov95-v000a-cet- creation
**:<--------------------------------------------------------------------
*/
#ifndef TDM_MACROS_H
#define TDM_MACROS_H

#define TDM Table and Dataset Memory

/*-------------------------------------------- INCLUDES             --*/
#include "soc_macros.h"

/*-------------------------------------------- MACROS               --*/

/*- Maximum number of dataset elements. -*/
#define DSET_DIM 128

#define TDMOBJECT(OBJ) ((tdmObject*)(OBJ->ptr()))
#define TDMTABLE(OBJ) ((tdmTable*)(OBJ->ptr()))
#define TDMDATASET(OBJ) ((tdmDataset*)(OBJ->ptr()))
#define TDMFACTORY(OBJ) ((tdmFactory*)(OBJ->ptr()))

#endif /* TDM_MACROS_H */

