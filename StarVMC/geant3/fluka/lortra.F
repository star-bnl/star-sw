*
* $Id: lortra.F,v 1.1.1.2 2009/02/01 17:10:18 fisyak Exp $
*
* $Log: lortra.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:18  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:46:52  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:24  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:24  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:36  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:01  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.44  by  S.Giani
*-- Author :
*$ CREATE LORTRA.FOR
*COPY LORTRA
*
*=== lortra ===========================================================*
*
      SUBROUTINE LORTRA(N,NAUX,GAM,BGX,BGY,BGZ)
 
#include "geant321/dblprc.inc"
#include "geant321/dimpar.inc"
#include "geant321/iounit.inc"
C
C     LORENTZ TRANSFORMATION OF  N PARTICLES IN  FINPAR  TO BE
C     STORED IN  AUXPAR  STARTING AT NAUX
C
C********************************************************************
C
#include "geant321/finpar.inc"
#include "geant321/auxpar.inc"
      DO 1  I=1,N
         J = NAUX + I - 1
         AMA(J)  = AMF(I)
         ICHA(J) = ICHF(I)
         IBARA(J)= IBARF(I)
         ANA(J)  = ANF(I)
         NREA(J) = NREF(I)
         CALL ALTRA(GAM,BGX,BGY,BGZ,PXF(I),PYF(I),PZF(I),HEP(I),
     &              PPA,PXA(J),PYA(J),PZA(J),HEPA(J))
   1  CONTINUE
      RETURN
      END
