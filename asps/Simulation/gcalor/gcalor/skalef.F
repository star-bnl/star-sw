* $Id: skalef.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      LOGICAL FUNCTION SKALEF(IP,EIP,ESKALE)
C*************************************************************
C
C  Called by: GCALOR
C  Purpose :  function is true, when scaling applies to FLUKA
C             linear transition from NMTC to FLUKA
C  Author : C.Zeitnitz
C
C*************************************************************
#include "geant321/crandm.inc"
C
      SKALEF = .TRUE.
      ENMTC = 3.495
      IF(IP.GT.1) ENMTC = 2.495
      IF(EIP.LE.ENMTC) SKALEF = .FALSE.
      IF(EIP.LT.ESKALE.AND.EIP.GT.ENMTC) THEN
         X1 = (EIP - ENMTC) / (ESKALE - ENMTC)
         X2 = SNGL(RANDC(ISEED))
         IF(X2.GT.X1) SKALEF = .FALSE.
      ENDIF
#if defined(ATLAS_CDEBUG)
      PRINT *,' SKALEF: E=',EIP,' ESKALE=',ESKALE,' X1=',X1, ' SKALE_'
     +//'FLUKA=',SKALEF
#endif
      RETURN
      END
