*
* $Id: xsepro.F,v 1.1.1.2 2009/02/01 17:10:18 fisyak Exp $
*
* $Log: xsepro.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:18  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:46:56  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:24  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:24  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:37  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:05  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.45  by  S.Giani
*-- Author :
*$ CREATE XSEPRO.FOR
*COPY XSEPRO
*
*=== xsepro ===========================================================*
*
      FUNCTION XSEPRO ( E, ZTAR, ATAR )
 
#include "geant321/dblprc.inc"
#include "geant321/dimpar.inc"
#include "geant321/iounit.inc"
*
*----------------------------------------------------------------------*
*                                                                      *
*     Created on 20 september 1991 by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on 20-sep-91     by    Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*
*
#include "geant321/xsepar.inc"
       COMMON / FKNEGX / XSENEG
*
       IZ = NINT ( ZTAR )
       IF ( E .GT. 0.1D+00 ) THEN
          EMEV = E * 1.D+03
          XSEPRO = SITSAO ( EMEV, IZ, ATAR )
       ELSE
          X  = 1.D+01 * E
          X2 = X * X
          XSEPRO = AAPXSE (IZ) * ( X - ZZPXSE (IZ) ) / X * ( 1.D+00
     &           + BBPXSE (IZ) * X + CCPXSE (IZ) * X2 + DDPXSE (IZ)
     &           * X * X2 + EEPXSE (IZ) * X2 * X2 + FFPXSE (IZ) * X2
     &           * X * X2 )
          IF ( XSEPRO .LT. 0.D+00 ) THEN
             XSENEG = XSEPRO
             XSEPRO = 0.D+00
          END IF
       END IF
*=== End of function XSEPRO ===========================================*
       RETURN
       END
