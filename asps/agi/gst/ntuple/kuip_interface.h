/*
 *  kuip_interface.h  --
 *	Declare interface to KUIP.
 *
 *  Original: 12-Jan-1996 16:08
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: kuip_interface.h,v 1.1 1999/02/16 15:45:09 fisyak Exp $
 *
 *  $Log: kuip_interface.h,v $
 *  Revision 1.1  1999/02/16 15:45:09  fisyak
 *  add kuip stuff
 *
 *  Revision 1.5  1996/12/05 10:04:19  lecointe
 *  Two completely different things
 *  	. add a _C to "paqcut" in "qp_cuts.c" to avoid duplicate symbol
 *  on VMS
 *  	. Modified "expand_var_list" called by "do_nt_scan" in npantup.c.
 *  Removed the hand-made parser and use "ku_getl" to parse the list of
 *  variables to scan.
 *
 *  Revision 1.4  1996/04/23 18:38:05  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_KUIP_INTERFACE
#define CERN_KUIP_INTERFACE

#include	"paw/pilot.h"

#include	"cfortran.h"


extern void	ku_alfa( void );
extern char *	ku_getc( void );
extern char *	ku_getf( void );
extern int	ku_geti( void );
extern char *	ku_gets( void );
extern char *	ku_getl( void );
extern int	ku_npar( void );
extern char *	ku_path( void );
extern char *	ku_proc( const char* prompt, const char* dfault );


#define KUVECT(VECNAM,LLOW,LHIGH) \
	CCALLSFSUB3(KUVECT,kuvect,STRING,PINT,PINT,VECNAM,LLOW,LHIGH) \


#if defined(CERNLIB_WINNT) || defined(CERNLIB_MSDOS) || defined(CERNLIB_VAXVMS)

extern int	strcasecmp( const char* str1, const char* str2 );
extern int	strncasecmp( const char* str1, const char* str2, size_t n );

#endif

#endif	/*	CERN_KUIP_INTERFACE	*/
