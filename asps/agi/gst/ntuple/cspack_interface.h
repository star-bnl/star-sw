/*
 *  cspack_interface.h  --
 *	Declare the interface to CSPACK
 *
 *  Original: 12-Jan-1996 16:47
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: cspack_interface.h,v 1.1 1999/02/16 15:45:05 fisyak Exp $
 *
 *  $Log: cspack_interface.h,v $
 *  Revision 1.1  1999/02/16 15:45:05  fisyak
 *  add kuip stuff
 *
 *  Revision 1.3  1996/09/18 14:30:25  dinofm
 *  A wrapper for CZGETA has been created as well.
 *
 *  Revision 1.2  1996/04/23 18:37:55  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_CSPACK_INTERFACE
#define CERN_CSPACK_INTERFACE

#include	"cfortran.h"


#define	CZPUTA(CHMAIL,ISTAT)	\
	CCALLSFSUB2(CZPUTA,czputa,STRING,PINT,CHMAIL,ISTAT)

#define	CZGETA(CHMAIL,ISTAT)	\
	CCALLSFSUB2(CZGETA,czgeta,PSTRING,PINT,CHMAIL,ISTAT)


#endif	/*	CERN_CSPACK_INTERFACE	*/
