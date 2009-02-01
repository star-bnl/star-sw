*
* $Id: stopar.F,v 1.1.1.2 2009/02/01 17:10:35 fisyak Exp $
*
* $Log: stopar.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:35  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:49:05  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:58  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:43  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:59  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.48  by  S.Giani
*-- Author :
      SUBROUTINE STOPAR(ID,NP)
C store particle in MPSTOR common
#include "geant321/mconst.inc"
#include "geant321/mpstor.inc"
#include "geant321/minput.inc"
C
      NPSTOR = NPSTOR + 1
      IF(NPSTOR.GT.MAXPAR) THEN
         WRITE(IOUT,'('' MICAP :  Cant store particle; bank full'',    '
     +   //'                 '' ID='',I3,'' NPSTOR='',I5)') ID,NPSTOR
         NPSTOR = NPSTOR - 1
      ELSE
         EN(NPSTOR) = EP
         UN(NPSTOR) = UP
         VN(NPSTOR) = VP
         WN(NPSTOR) = WP
         AMN(NPSTOR) = AMP
         ZMN(NPSTOR) = ZMP
         AGEN(NPSTOR) = AGEP
         MTN(NPSTOR) = MTP
         IDN(NPSTOR) = ID
         NP = NP + 1
      ENDIF
      RETURN
      END
