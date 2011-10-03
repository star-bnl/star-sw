* $Id: getxsc.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE GETXSC
C***************************************************
C    Get x-section for hadronic interaction
C
C  INPUT: material and particle parameters
C  OUTPUT: SIG = x-section
C  Author: C.Zeitnitz
C
C**************************************************
C GEANT-CALOR interface common
#include "gcalor/calgea.inc"
C HETC-COMMON
#include "gcalor/ccomon.inc"
C
C MICAP or HETC ?
      IF(IPINC.EQ.1.AND.EINC.LT.20.0) THEN
C set MICAP parameter
         EK = EINC * 1.0E6
         SIG = SIGMOR(EK,NCEL)
#if defined(ATLAS_CDEBUG)
         PRINT '('' MICAP XS: Ek='',F12.0,'' Sig='',F12.7,             '
     +   //'            '' Med='',I5)',EK,SIG,NCEL
#endif
         ICPROC = 1
      ELSE
C copy parameter to HETC common
         NO = 1
         ITYP = IPINC + 1
         TIP(1) = FLOAT(IPINC)
         EC(1) = EINC
         IF(IPINC.LE.1.AND.EINC.GT.EMAXP) EC(1) = EMAXP*1000.
         IF(IPINC.GT.1.AND.EINC.GT.EMAXPI) EC(1) = EMAXPI*1000.
         MAT = 1
         MXMAT = 1
         NEL(1) = NCEL
C   copy material data to HETC
         DO 10 I=1,NCEL
            DEN(I,1) = DMED(I)
            ZZ(I,1) = ZMED(I)
            A(I,1) = AMED(I)
            DENH(1) = HDEN
   10    CONTINUE
C calcutlate x-section for particle type IPINC and material
         CALL CALCXS
         SIG = SIGMX(ITYP,MAT)
         ICPROC = 0
      ENDIF
      RETURN
      END
