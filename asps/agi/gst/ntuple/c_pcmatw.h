/*
 *  c_pcmatw.h  --
 *	Map the /PCMATW/ common
 *
 *  Original: 12-Dec-1995 11:27
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_pcmatw.h,v 1.1 1999/02/16 15:45:03 fisyak Exp $
 *
 *  $Log: c_pcmatw.h,v $
 *  Revision 1.1  1999/02/16 15:45:03  fisyak
 *  add kuip stuff
 *
 *  Revision 1.2  1996/04/23 18:37:52  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_C_PCMATW
#define CERN_C_PCMATW

#include	"cfortran.h"

#define	MAX_OUTSTR	512


typedef struct {
	char	outstr[MAX_OUTSTR][32];
} pcmatw_def;


#define PCMATW COMMON_BLOCK(PCMATW,pcmatw)
COMMON_BLOCK_DEF(pcmatw_def,PCMATW);

#endif	/*	CERN_C_PCMATW	*/
