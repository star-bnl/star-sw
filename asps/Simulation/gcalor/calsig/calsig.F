* $Id: calsig.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALSIG
C************************************************************
C         CALOR-Cross-Section
C
C  INPUT  : material constants and particle type
C  OUTPUT : distance to next hadronic interaction
C
C  Author : Christian Zeitnitz (U of Arizona)
C  Date   : 6-6-92
C************************************************************
C
C GEANT Commons
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gcbank.inc"
#include "geant321/gconsp.inc"
#include "geant321/gcphys.inc"
C
C CALOR common
#include "gcalor/calgea.inc"
C
C  Avogadro number multiplied by 1.E-24
      PARAMETER(XNAVO = 0.60221367)
CC
      LOGICAL INIT,GOFLUK,DOSKAL,SKALEF
C
      DATA INIT/.TRUE./
C
      IF(INIT) THEN
C
C Initialize CALOR at first call
         INIT = .FALSE.
         CALL CALINI
      ENDIF
C
      DOSKAL = .FALSE.
      IF(Z+0.5.GE.1.0 ) THEN
         IPINC = -1
         IF(IPART .LE. 48) IPINC = IGECAL(IPART)
C
C ------------- check if FLUKA have to be called ---------
C ------------------------------------------------- Goto FLUKA ?
C
C particle type not implemented in CALOR
#if defined(ATLAS_CDEBUG)
         PRINT '('' IPgeant='',I4,'' IPcalor='',I4,'' Ekin='',F18.5)',
     +   IPART,IPINC,GEKIN
#endif
         GOFLUK = IPINC .EQ. -1 .OR. GEKIN .GE. ESKALE
         DOSKAL = (IPINC.EQ.0 .OR. IPINC.EQ.1) .AND. GEKIN.GT.EMAXP
         DOSKAL = DOSKAL .OR. (GEKIN .GT. EMAXPI .AND. (IPINC .GT. 1))
         DOSKAL = DOSKAL .AND. .NOT.GOFLUK
         GOFLUK = GOFLUK .OR. (DOSKAL.AND.SKALEF(IPINC,GEKIN,ESKALE))
C
         IF(GOFLUK) THEN
            ICPROC = 3
            CALL FLDIST
            RETURN
         ENDIF
C
C --------- material information for CALOR --------------------------
C
         EINC = GEKIN * 1000.0
         IF(IPINC .EQ. 1 .AND. EINC .LT. 20.0 ) THEN
C MICAP needs only the GEANT material number !
           NCEL = NMAT
         ELSE
           NCEL = 1
           AMED(1) = A
           ZMED(1) = Z
           DMED(1) = DENS/A*XNAVO
           IF(A.GT.1.0 .AND. A.LT.1.1) THEN
              HDEN = DMED(1)
           ELSE
              HDEN = 0.0
           ENDIF
C ------- get material parameter for a mixture---------------------
           KK=MIN(Q(JMA+11),10.)
           NCEL = 1
           IF(KK.GT.1) THEN
              NCEL = 0
              HDEN = 0.0
              AMOL = Q(LQ(JMIXT-1)+2)
              DO 10 K=1,KK
                 IF(NINT(Q(JMIXT+K)).EQ.1) THEN
                    XMOLCM = DENS/AMOL*XNAVO
                    WI = Q(JMIXT+K+2*KK)*AMOL/Q(JMIXT+K)
                    HDEN = HDEN + XMOLCM*WI
                 ELSE
                    NCEL = NCEL + 1
                    AMED(NCEL)= Q(JMIXT+K)
                    ZMED(NCEL) = Q(JMIXT+K+KK)
                    XMOLCM = DENS/AMOL*XNAVO
                    WI = Q(JMIXT+K+2*KK)*AMOL/AMED(NCEL)
                    DMED(NCEL) = XMOLCM*WI
                 ENDIF
   10         CONTINUE
           ENDIF
         ENDIF
C
         CALL GETXSC
         IF( SIG .GT. 0.0) THEN
            SHADR = ZINTHA/SIG
         ELSE
            SHADR = BIG
         ENDIF
      ELSE
         SHADR = BIG
#if defined(ATLAS_CDEBUG)
         PRINT *,' CALSIG: Z < 1 -> No interaction'
#endif
      ENDIF
#if defined(ATLAS_CDEBUG)
      PRINT *,' CALSIG: IP=',IPART,' Ekin=',GEKIN, ' A,Z=',A,Z,' Shadr='
     +,SHADR
#endif
      IF(DOSKAL) ICPROC = 2
      RETURN
      END
