/*
 *  pawcom.h  --
 *	Map the /PAWCOM/ and /PAWCID/ commons
 *
 *  Original: 31-Mar-1995 17:06
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_pawcom.h,v 1.1 1999/02/16 15:45:01 fisyak Exp $
 *
 *  $Log: c_pawcom.h,v $
 *  Revision 1.1  1999/02/16 15:45:01  fisyak
 *  add kuip stuff
 *
 *  Revision 1.2  1996/04/23 18:37:49  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_PAWCOM
#define CERN_PAWCOM

#include	"cfortran.h"


typedef struct {
	int	id;
	int	num;
	int	jofset;
	int	idold;
	int	khrin;
} pawcom_def;

#define PAWCOM COMMON_BLOCK(PAWCOM,pawcom)
COMMON_BLOCK_DEF(pawcom_def,PAWCOM);


typedef struct {
	char	chid[128];
	char	chcase[4];
} pawcid_def;

#define PAWCID COMMON_BLOCK(PAWCID,pawcid)
COMMON_BLOCK_DEF(pawcid_def,PAWCID);


#endif	/*	CERN_PAWCOM	*/
