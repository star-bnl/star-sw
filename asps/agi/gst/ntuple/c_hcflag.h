/*
 *  hcflag.h  --
 *	Map the /HCFLAG/ common block
 *
 *  Original:  7-Nov-1994 16:20
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_hcflag.h,v 1.1 1999/02/16 15:44:59 fisyak Exp $
 *
 *  $Log: c_hcflag.h,v $
 *  Revision 1.1  1999/02/16 15:44:59  fisyak
 *  add kuip stuff
 *
 *  Revision 1.2  1996/04/23 18:37:44  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HCFLAG
#define CERN_HCFLAG

#include	"cfortran.h"


typedef struct {
	int	id;
	int	idbadd;
	int	lid;
	int	idlast;
	int	idhold;
	int	nbit;
	int	nbitch;
	int	nchar;
	int	nrhist;
	int	ierr;
	int	nv;
} hcflag_def;

#define HCFLAG COMMON_BLOCK(HCFLAG,hcflag)

COMMON_BLOCK_DEF(hcflag_def,HCFLAG);


#endif	/*	CERN_HCFLAG	*/
