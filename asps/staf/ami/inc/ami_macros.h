/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        ami_macros.h
**:DESCRIPTION: AMI macros header file
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     15dec95-v000a-cet- creation
**:<--------------------------------------------------------------------
*/
#ifndef AMI_MACROS_H
#define AMI_MACROS_H

#define AMI Analysis Module Interface

/*-------------------------------------------- INCLUDES             --*/
#include "soc_macros.h"

/*-------------------------------------------- MACROS               --*/
/*- Don't use CORBA (by default).
#define CORBA -*/

#define AMI_MAX_TABLES 32 /* CURRENTLY MAXIMUM OF 32 TABLES */

#define AMIINVOKER(OBJ) ((amiInvoker*)(OBJ->ptr()))
#define AMIBROKER(OBJ) ((amiBroker*)(OBJ->ptr()))

#endif /* AMI_MACROS_H */

