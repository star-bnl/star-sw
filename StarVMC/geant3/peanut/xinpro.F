*
* $Id: xinpro.F,v 1.1.1.2 2009/02/01 17:10:35 fisyak Exp $
*
* $Log: xinpro.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:35  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:49:10  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:58  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:43  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:22  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:22:04  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.46  by  S.Giani
*-- Author :
*$ CREATE XINPRO.FOR
*COPY XINPRO
*
*=== xinpro ===========================================================*
*
      FUNCTION XINPRO ( E, ZTAR, VCOUL )
 
#include "geant321/dblprc.inc"
#include "geant321/dimpar.inc"
#include "geant321/iounit.inc"
*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*
#include "geant321/isotop.inc"
#include "geant321/xsepar.inc"
      COMMON / FKNEGX / XSENEG
*
      IZ = NINT ( ZTAR )
      IF ( E .GT. 0.1D+00 ) THEN
         ATAR = 0.D+00
         DO 25 IS = ISONDX (1,IZ), ISONDX (2,IZ)
             ATAR = ATAR + ISOMNM (IS) * ABUISO (IS)
   25    CONTINUE
         EMEV = E * 1.D+03
         XINPRO = SITSAO ( EMEV, IZ, ATAR )
      ELSE
         ZZCOUL = 1.D+01 * VCOUL
         X  = 1.D+01 * E
         X2 = X * X
         XINPRO = AAPXSE (IZ) * ( X - ZZCOUL ) / X * ( 1.D+00
     &          + BBPXSE (IZ) * X + CCPXSE (IZ) * X2 + DDPXSE (IZ)
     &          * X * X2 + EEPXSE (IZ) * X2 * X2 + FFPXSE (IZ) * X2
     &          * X * X2 )
         IF ( XINPRO .LT. 0.D+00 ) THEN
            XSENEG = XINPRO
            XINPRO = 0.D+00
         END IF
      END IF
*=== End of function XINPRO ===========================================*
       RETURN
       END
