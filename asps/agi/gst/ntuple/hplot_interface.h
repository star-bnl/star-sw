/*
 *  hplot_interface.h  --
 *
 *  Original: 12-Jan-1996 16:01
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: hplot_interface.h,v 1.1 1999/02/16 15:45:09 fisyak Exp $
 *
 *  $Log: hplot_interface.h,v $
 *  Revision 1.1  1999/02/16 15:45:09  fisyak
 *  add kuip stuff
 *
 *  Revision 1.4  1996/08/21 12:55:30  lecointe
 *  Restore the spider plot in ntuple/scan
 *
 *  Revision 1.3  1996/04/23 18:38:04  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HPLOT_INTERFACE
#define CERN_HPLOT_INTERFACE

#include	"cfortran.h"


#define HPLFR3(A1,A2,A3,A4,A5,A6,A7,A8,A9) \
	CCALLSFSUB9(HPLFR3,hplfr3,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,STRING,A1,A2,A3,A4,A5,A6,A7,A8,A9)


#define	HPLFRA(X1,X2,Y1,Y2,CHOPT) \
	CCALLSFSUB5(HPLFRA,hplfra,FLOAT,FLOAT,FLOAT,FLOAT,STRING,X1,X2,Y1,Y2,CHOPT)

#define HPLOPT(A1,A2) \
	CCALLSFSUB2(HPLOPT,hplopt,STRING,INT,A1,A2)

#define HPLSET(A1,A2) \
	CCALLSFSUB2(HPLSET,hplset,STRING,FLOATV,A1,A2)

#define HPLZON(A1,A2,A3,A4) \
	CCALLSFSUB4(HPLZON,hplzon,INT,INT,INT,STRING,A1,A2,A3,A4)

#define HPLGZO(A1,A2) \
	CCALLSFSUB2(HPLGZO,hplgzo,INTV,INTV,A1,A2)

#define HPLSTA(A1,A2,A3) \
	CCALLSFSUB3(HPLSTA,hplsta,INT,STRING,INT,A1,A2,A3)


#define	HPLTIT(TITLE) \
	CCALLSFSUB1(HPLTIT,hpltit,STRING,TITLE)

#endif	/*	CERN_HPLOT_INTERFACE	*/
