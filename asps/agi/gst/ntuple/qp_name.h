/*
 *  qp_name.h  --
 *	Definitions for all the name lookup resolving
 *
 *  Original: 10-Oct-1994 13:51
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_name.h,v 1.1 1999/02/16 15:45:22 fisyak Exp $
 *
 *  $Log: qp_name.h,v $
 *  Revision 1.1  1999/02/16 15:45:22  fisyak
 *  add kuip stuff
 *
 *  Revision 1.3  1996/04/23 18:38:47  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_NAME
#define CERN_NAME

#include	"qp_tree.h"


STIndex
name_resolve( char * const name, int * r );

#endif	/*	CERN_NAME	*/
