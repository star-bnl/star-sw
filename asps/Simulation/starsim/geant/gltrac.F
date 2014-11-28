*
* $Id: gltrac.F,v 1.2 2011/03/30 19:34:08 jwebb Exp $
*
* $Log: gltrac.F,v $
* Revision 1.2  2011/03/30 19:34:08  jwebb
* gthion.F -- modified such that the effective charge used in dE/dx generation
*             depends on |Q| rather than Q to prevent Q**(2/3) generating a
*             NaN condition.
*
*             Also added code to permit unstable (hyper) nuclei to decay.
*
* gltrac.F -- modified to initialize decay of unstable (hyper) nuclei.
*
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
*
* Revision 1.1  2001/02/27 10:15:38  nevski
*  first working release
*
* Revision 1.3  2000/01/06 22:09:16  nevski
* unlimited track/vertex version
*
* Revision 1.2  1998/04/16 16:59:36  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :          25/03/98  00.13.37  by  Pavel Nevski
*CMZ :  1.30/00 03/05/97  16.07.14  by  Pavel Nevski
*CMZ :  3.21/04 13/12/94  15.36.22  by  S.Giani
*-- Author :
      SUBROUTINE GLTRAC
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GLTRAC                                                *
C.    *                                                                *
C.    *   Extracts next track from stack JSTAK and prepares commons    *
C.    *    /GCTRAK/, /GCKINE/ and /GCVOLU/                             *
C.    *                                                                *
C.    *   Called by : GTREVE                                           *
C.    *   Authors   : R.Brun, F.Bruyant                                *
C.    *   Modifications:                                               *
C.    *   1. extend KINE bank with LgKINE                              *
C.    *   2. all unknown particle are treated as geantino              *
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
      COMMON /GCKINE_CONT/ ITRO,TOFO,MECATO
C.
C.    ------------------------------------------------------------------
*
* *** Extract next track from stack JSTAK
*
      IF(ISTORD.EQ.1) THEN
*
* *** User ordering of tracks if requested
         CALL GSTORD
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
         IVERT=Q(LgKINE(JK,ITRA)+6)
      ENDIF
*PN: extract and save ALL from stack, it may be overwritten by interactions
      ITRO   = IQ(JST+1)
      IPART  = IQ(JST+2)
      MECATO = IQ(JST+3)
      DO 60 I = 1,3
         VERT(I) = Q(JST+3+I)
        PVERT(I) = Q(JST+6+I)
   60 CONTINUE
      TOFO   = Q(JST+10)
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
         JPA = 0
         IF (IPART.LE.IQ(JPART-2)) JPA = LQ(JPART-IPART)
         IF (JPA .EQ. 0)           JPA = LQ(JPART-48)

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
         IF (IQ(JSTAK-2).GT.0 .and. LQ(JSTAK-1).GT.0) then
            JPO = LQ(JSTAK-1)+(ISTAK-1)*3
            POLAR(1) = Q(JPO+1)
            POLAR(2) = Q(JPO+2)
            POLAR(3) = Q(JPO+3)
         ELSE
            CALL VZERO(POLAR,3)
         ENDIF
      ELSE
         CALL GEKBIN
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
         CALL GRNDM(RNDM,3)
         SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1)) !-- Added lifetime for unstable nuclei (e.g. hypernuclei)
         ZINTHA = -LOG(RNDM(2))
         ZINTDR = -LOG(RNDM(3))
      ENDIF
*
*   * Prepare common /GCVOLU/ and structure JGPAR, if needed
*
      IF (NJTMAX.LE.0) THEN
        IF (GONLY(NLEVEL).EQ.0.) NLEVEL=0
        CALL GMEDIA (VECT, NUMED)
      ENDIF
      INFROM = 0
*                                                             END GLTRAC
      END


