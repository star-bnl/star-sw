*
* $Id: corinc.inc,v 1.1.1.1 2005/05/25 22:37:28 fisyak Exp $
*
* $Log: corinc.inc,v $
* Revision 1.1.1.1  2005/05/25 22:37:28  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:37  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_CORINC_INC
#define CERNLIB_GEANT321_CORINC_INC
*
*
* corinc.inc
*
*                                                                      *
*=== corinc ===========================================================*
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
*     Modified by A. Ferrari: be sure that the parameter Inmax is >=   *
*     of the same parameter in the function Nudist                     *
*----------------------------------------------------------------------*
*
      PARAMETER ( INMAX = 20 )
      COMMON /FKCORI/ XSEA (INMAX),  XASEA (INMAX), FRAINC, ANUAV,
     &                IJTARG (INMAX+1), NSEA
 

#endif
