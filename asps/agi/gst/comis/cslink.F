*
* $Id: cslink.F,v 1.2 1998/04/16 17:02:20 fisyak Exp $
*
* $Log: cslink.F,v $
* Revision 1.2  1998/04/16 17:02:20  fisyak
* 2nd pass with gstar
*
*
* -----------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          23/05/97  18.43.27  by  Pavel Nevski
*CMZ :  1.30/00 22/04/97  15.56.16  by  Pavel Nevski
*CMZ :  1.18/14 16/01/95  11.45.29  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSLINK
***------------------------
*     this is comis linker
***------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cslun.inc"
#if defined(CERNLIB_SHL)
      CHARACTER*(KLENID) FNNAME
      INTEGER CS_GET_FUNC
#endif
      INTEGER CSPMFS,CSLLIB
      COMMON /CSTBCS/  ITBC,ITBS
      COMMON /CSDEBUG/ ICSDEBUG
      COMMON /CSGSCM/  IGS,JGS,NGS,CSJUNK(3)
*
      if (ICSDEBUG.gt.0) print *,' ===> in CSLINK <=== '
      IF(ISTLIB.EQ.0 .AND. ITBS.EQ.0)RETURN
   2  I=IPGP
   3  IF(I.EQ.0)GO TO  5
      CALL CCOPYA(IQ(I+1),NCIDGP,KSIDP-1)
      IF(IADGP.EQ.0)THEN
        IF(ISTLIB.NE.0)THEN
          JGP=MJCHAR(IQ(I+KSIDP))
          IP=CSLLIB(JGP,NCIDGP)
          IF(IP.NE.0)THEN
             LIBRD=1
             NWIDEN=(NCIDGP+3)/4
             NCIDEN=NCIDGP
             CALL CCOPYA(IQ(I+KSIDP),IDEN(1),NWIDEN)
             CALL CSRD(JGS,NGS)
             IF (NGS .LT. 0) GO TO 5
             KPRO=CSPMFS(IBC)
             LIBRD=0
             KEYRD=0
             GO TO 4
          ENDIF
        ENDIF
*MAP-file
        IF(ITBS.NE.0)THEN
#if defined(CERNLIB_VAX)
          IP=MLSEAR(ITBS,IQ(I+KSIDP))
          IF(IP.GT.0)THEN
            IADGP=IQ(IP)
            IFCS=-1
            CALL CSRTGP(I)
          ENDIF
#endif
#if defined(CERNLIB_SHL)
          CALL CSGTIDP(I,FNNAME,NC)
          CALL CUTOL(FNNAME(1:NC))
          IADGP=CS_GET_FUNC(FNNAME(1:NC)//'_')
          IF(IADGP.NE.0)THEN
            IFCS=-2
            CALL CSRTGP(I)
          ENDIF
#endif
          IP=0
        ENDIF
      ENDIF
  4   I=IQ(I)
      GO TO 3
  5   END
 
