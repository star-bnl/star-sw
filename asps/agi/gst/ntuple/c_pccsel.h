/*
 *  c_pccsel.h  --
 *	Map the /PCCSEL/ and /PCCSE2/ commonblocks
 *
 *  Original: 15-May-1996 11:06
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_pccsel.h,v 1.1 1999/02/16 15:45:02 fisyak Exp $
 *
 *  $Log: c_pccsel.h,v $
 *  Revision 1.1  1999/02/16 15:45:02  fisyak
 *  add kuip stuff
 *
 *  Revision 1.1  1996/05/15 13:11:37  maartenb
 *  - Fix the CSELECT command.
 *
 *
 *
 */

#ifndef CERN_C_PCCSEL
#define CERN_C_PCCSEL

#include	"cfortran.h"


typedef struct {
	int	ioptcs[5];
	int	csize;
} pccsel_def;

#define	ioptcr	ioptcs[0]
#define	ioptcb	ioptcs[1]
#define	ioptcm	ioptcs[2]
#define	ioptcc	ioptcs[3]
#define	ioptcn	ioptcs[4]

#define	PCCSEL	COMMON_BLOCK(PCCSEL,pccsel)

COMMON_BLOCK_DEF(pccsel_def,PCCSEL);


typedef struct {
	char	chcsel[80];
	char	csopt[8];
} pccse2_def;

#define	PCCSE2	COMMON_BLOCK(PCCSE2,pccse2)

COMMON_BLOCK_DEF(pccse2_def,PCCSE2);


#endif	/*	CERN_C_PCCSEL	*/
