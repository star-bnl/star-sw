/* Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:		dio_macros.h
*:DESCRIPTION:	Analysis Service Package template header file
*:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:		-- STILL IN DEVELOPMENT --
*:HISTORY:	12dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#ifndef DIO_MACROS_H
#define DIO_MACROS_H

#define DIO Dataset Input/Output

/*-------------------------------------------- INCLUDES             --*/
#include "soc_macros.h"

/*-------------------------------------------- MACROS               --*/
/*- Don't use CORBA (by default).
#define CORBA -*/

#define DIOSTREAM(OBJ) ((dioStream*)(OBJ->ptr()))
#define DIOFILESTREAM(OBJ) ((dioFileStream*)(OBJ->ptr()))
#define DIOSOCKSTREAM(OBJ) ((dioSockStream*)(OBJ->ptr()))

#endif /* DIO_MACROS_H */

