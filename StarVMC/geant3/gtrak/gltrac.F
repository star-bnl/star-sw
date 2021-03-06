*
* $Id: gltrac.F,v 1.1.1.2 2009/02/01 17:10:33 fisyak Exp $
*
* $Log: gltrac.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:33  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:48:51  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:56  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.2  2003/11/28 11:23:56  brun
* New version of geant321 with all geant3 routines renamed from G to G3
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:41  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:21  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:41  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/04 13/12/94  15.36.22  by  S.Giani
*-- Author :
      SUBROUTINE G3LTRAC
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. G3LTRAC                                               *
C.    *                                                                *
C.    *   Extracts next track from stack JSTAK and prepares commons    *
C.    *    /GCTRAK/, /GCKINE/ and /GCVOLU/                             *
C.    *                                                                *
C.    *   Called by : G3TREVE                                          *
C.    *   Authors   : R.Brun, F.Bruyant                                *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gckine.inc"
#include "geant321/gcnum.inc"
#include "geant321/gconsp.inc"
#include "geant321/gcphys.inc"
#include "geant321/gcstak.inc"
#include "geant321/gctmed.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"
      DIMENSION RNDM(5)
#if !defined(CERNLIB_SINGLE)
      DOUBLE PRECISION P2,GETOTD,GEKIND
      DOUBLE PRECISION PXD,PYD,PZD,ONE,HNORM,DAMASS,PP
#endif
      PARAMETER (ONE=1)
C.
C.    ------------------------------------------------------------------
*
* *** Extract next track from stack JSTAK
*
      IF(ISTORD.EQ.1) THEN
*
* *** User ordering of tracks if requested
         CALL G3STORD
      ENDIF
      ISTAK = IQ(JSTAK+1)
      IQ(JSTAK+1) = ISTAK -1
      JST = JSTAK +NWSTAK*IQ(JSTAK+1) +3
      ITRA   = IQ(JST+1)
      IF (ITRA.LT.0) THEN
         ITRA = -ITRA
      ELSE
*
*        This is a new track. We set to zero the stack number and
*        update the vertex number
         ISTAK = 0
         JK=LQ(JKINE-ITRA)
         IVERT=Q(JK+6)
      ENDIF
      IPART  = IQ(JST+2)
      DO 60 I = 1,3
         VERT(I) = Q(JST+3+I)
        PVERT(I) = Q(JST+6+I)
   60 CONTINUE
      TOFG   = Q(JST+10)
      SAFETY = Q(JST+11)
      UPWGHT = Q(JST+12)
*
* *** Prepare tracking parameters
*
      VECT(1) = VERT(1)
      VECT(2) = VERT(2)
      VECT(3) = VERT(3)
      PXD = PVERT(1)
      PYD = PVERT(2)
      PZD = PVERT(3)
      P2 = PXD**2+PYD**2+PZD**2
      IF(P2.GT.0.) THEN
         PP    = SQRT(P2)
         HNORM = ONE/PP
         VECT(4) = PVERT(1)*HNORM
         VECT(5) = PVERT(2)*HNORM
         VECT(6) = PVERT(3)*HNORM
         VECT(7) = PP
      ELSE
         VECT(4) = 0.
         VECT(5) = 0.
         VECT(6) = 1.
         VECT(7) = 0.
      ENDIF
*
*  ** Reload Particle characteristics, if needed
*
      IF (IPART.NE.IPAOLD) THEN
         JPA = LQ(JPART-IPART)
         DO 90 I = 1,5
            NAPART(I) = IQ(JPA+I)
   90    CONTINUE
         ITRTYP = Q(JPA+6)
         AMASS  = Q(JPA+7)
         CHARGE = Q(JPA+8)
         TLIFE  = Q(JPA+9)
         IUPD   = 0
         IPAOLD = IPART
      ENDIF
*
      DAMASS = AMASS
      GETOTD = SQRT(P2+DAMASS**2)
      GEKIND = GETOTD - DAMASS
      GETOT  = GETOTD
      GEKIN  = GEKIND
*
      IF (ITRTYP.EQ.7) THEN
*
* *** Cerenkov photon. Retrieve polarisation
         JPO = LQ(JSTAK-1)+(ISTAK-1)*3
         POLAR(1) = Q(JPO+1)
         POLAR(2) = Q(JPO+2)
         POLAR(3) = Q(JPO+3)
      ELSE
         CALL G3EKBIN
      ENDIF
*
      SLENG  = 0.
      NSTEP  = 0
      NTMSTO = NTMSTO +1
      NTMULT = NTMSTO
      ISTORY = 0
*
*  ** Initialize interaction probabilities
*
      IF (ITRTYP.EQ.1) THEN
*      Gammas
         CALL GRNDM(RNDM,5)
         ZINTPA = -LOG(RNDM(1))
         ZINTCO = -LOG(RNDM(2))
         ZINTPH = -LOG(RNDM(3))
         ZINTPF = -LOG(RNDM(4))
         ZINTRA = -LOG(RNDM(5))
      ELSE IF (ITRTYP.EQ.2) THEN
*       Electrons
         CALL GRNDM(RNDM,3)
         ZINTBR = -LOG(RNDM(1))
         ZINTDR = -LOG(RNDM(2))
         ZINTAN = -LOG(RNDM(3))
      ELSE IF (ITRTYP.EQ.3) THEN
*       Neutral hadrons
         CALL GRNDM(RNDM,2)
         SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
         ZINTHA = -LOG(RNDM(2))
      ELSE IF (ITRTYP.EQ.4) THEN
*       Charged hadrons
         CALL GRNDM(RNDM,3)
         SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
         ZINTHA = -LOG(RNDM(2))
         ZINTDR = -LOG(RNDM(3))
      ELSE IF (ITRTYP.EQ.5) THEN
*       Muons
         CALL GRNDM(RNDM,5)
         SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
         ZINTBR = -LOG(RNDM(2))
         ZINTPA = -LOG(RNDM(3))
         ZINTDR = -LOG(RNDM(4))
         ZINTMU = -LOG(RNDM(5))
      ELSE IF (ITRTYP.EQ.7) THEN
*       Cerenkov photons
         CALL GRNDM(RNDM,1)
         ZINTLA = -LOG(RNDM(1))
      ELSE IF (ITRTYP.EQ.8) THEN
*       Ions
         CALL GRNDM(RNDM,2)
         ZINTHA = -LOG(RNDM(1))
         ZINTDR = -LOG(RNDM(2))
      ENDIF
*
*   * Prepare common /GCVOLU/ and structure JGPAR, if needed
*
c      IF (NJTMAX.LE.0) THEN
        CALL GMEDIA (VECT, NUMED, 1)
c      ENDIF
      INFROM = 0
*                                                             END GLTRAC
      END
 
