/*
 *  higz_interface.h  --
 *	Declare interface to HIGZ.
 *
 *  Original: 12-Jan-1996 16:09
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: higz_interface.h,v 1.1 1999/02/16 15:45:09 fisyak Exp $
 *
 *  $Log: higz_interface.h,v $
 *  Revision 1.1  1999/02/16 15:45:09  fisyak
 *  add kuip stuff
 *
 *  Revision 1.10  1996/08/21 12:55:29  lecointe
 *  Restore the spider plot in ntuple/scan
 *
 *  Revision 1.9  1996/07/04 13:25:52  couet
 *  - The LOCATOR on ntuples (in PAW and PAW++) was not working.
 *    The calls to IGPID were missing.
 *
 *  Revision 1.8  1996/05/21 15:59:39  couet
 *  - It was not allowed to to the "S"ame option in NT/PLOT if the previous
 *    plot was not done with NT/PLOT. Now, in that case, the coordinates for
 *    the Same are taken from IGQWK.
 *
 *  Revision 1.7  1996/04/23 18:38:03  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HIGZ_INTERFACE
#define CERN_HIGZ_INTERFACE

#include	"cfortran.h"


#define	IGRAPH(N,X,Y,CHOPT) \
	CCALLSFSUB4(IGRAPH,igraph,INT,FLOATV,FLOATV,STRING,N,X,Y,CHOPT)

#define	IGTERM() \
	CCALLSFSUB0(IGTERM,igterm)

#define IGTEXT(X,Y,CHTEXT,SIZE,ANGLE,CHOPT) \
	CCALLSFSUB6(IGTEXT,igtext,FLOAT,FLOAT,STRING,FLOAT,FLOAT,STRING,X,Y,CHTEXT,SIZE,ANGLE,CHOPT)


#define IPL(A1,A2,A3)\
	CCALLSFSUB3(IPL,ipl,INT,FLOATV,FLOATV,A1,A2,A3)


#define IPL3(A1,A2,A3,A4)\
	CCALLSFSUB4(IPL3,ipl3,INT,FLOATV,FLOATV,FLOATV,A1,A2,A3,A4)


#define IPM3ID(A1,A2,A3,A4,A5,A6)\
	CCALLSFSUB6(IPM3ID,ipm3id,INT,FLOATV,FLOATV,FLOATV,INT,INTV,A1,A2,A3,A4,A5,A6)


#define IPM4ID(A1,A2,A3,A4,A5,A6,A7,A8,A9)\
	CCALLSFSUB9(IPM4ID,ipm4id,INT,FLOATV,FLOATV,FLOATV,FLOATV,FLOAT,FLOAT,INT,INTV,A1,A2,A3,A4,A5,A6,A7,A8,A9)


#define IPMID(A1,A2,A3,A4,A5)\
	CCALLSFSUB5(IPMID,ipmid,INT,FLOATV,FLOATV,INT,INTV,A1,A2,A3,A4,A5)


#define IGCOLM(X1,X2,Y1,Y2,IC1,IC2,ZMIN,ZMAX,CHOPT)\
	CCALLSFSUB9(IGCOLM,igcolm,FLOAT,FLOAT,FLOAT,FLOAT,PINT,INTV,FLOAT,FLOAT,STRING,X1,X2,Y1,Y2,IC1,IC2,ZMIN,ZMAX,CHOPT)


#define	IGQWK(IWKID,PNAME,RVAL) \
	CCALLSFSUB3(IGQWK,igqwk,INT,STRING,FLOATV,IWKID,PNAME,RVAL)


#define	IGPID(ILEV,CHPID,IPID,CHOPT) \
	CCALLSFSUB4(IGPID,igpid,INT,STRING,INT,STRING,ILEV,CHPID,IPID,CHOPT)


#endif	/*	CERN_HIGZ_INTERFACE	*/
