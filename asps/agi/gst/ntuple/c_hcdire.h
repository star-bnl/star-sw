/*
 *  hcdire.h  --
 *	Map the /HCDIRN/, /HCDIRC/ and /HCFILE/ common
 *
 *  Original:  3-Apr-1995 14:05
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_hcdire.h,v 1.1 1999/02/16 15:44:58 fisyak Exp $
 *
 *  $Log: c_hcdire.h,v $
 *  Revision 1.1  1999/02/16 15:44:58  fisyak
 *  add kuip stuff
 *
 *  Revision 1.2  1996/04/23 18:37:43  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HCDIRE
#define CERN_HCDIRE

#include	"cfortran.h"


#define	NLPATM	100
#define	MXFILES	50


typedef struct {
	int	nlcdir;
	int	nlndir;
	int	nlpat;
	int	icdir;
	int	nchtop;
	int	ichtop[MXFILES];
	int	ichtyp[MXFILES];
	int	ichlun[MXFILES];
} hcdirn_def;

#define HCDIRN COMMON_BLOCK(HCDIRN,hcdirn)
COMMON_BLOCK_DEF(hcdirn_def,HCDIRN);


typedef struct {
	char	chcdir[NLPATM][16];
	char	chndir[NLPATM][16];
	char	chpat[NLPATM][16];
	char	chtop[NLPATM][16];
} hcdirc_def;

#define HCDIRC COMMON_BLOCK(HCDIRC,hcdirc)
COMMON_BLOCK_DEF(hcdirc_def,HCDIRC);


typedef struct {
	char	hfname[MXFILES][80];
} hcfile_def;

#define HCFILE COMMON_BLOCK(HCFILE,hcfile)
COMMON_BLOCK_DEF(hcfile_def,HCFILE);


#endif	/*	CERN_HCDIRE	*/
