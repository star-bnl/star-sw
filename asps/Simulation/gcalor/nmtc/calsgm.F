* $Id: calsgm.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALSGM(NSGNL,IT,IS,DDE,EM,I,E,S)
C
#include "gcalor/cinout.inc"
#include "gcalor/cxpd.inc"
#include "gcalor/cbert2.inc"
C
      DIMENSION PPNP(9,2),ENRGY(9)
      DATA ENRGY/1.,  3.,  5.,  8.,  10., 12.5,15., 17.5,20./
      DATA PPNP /1.20,.890,.620,.392,.310,.250,.208,.178,.155,
     +           4.25,2.28,1.62,1.14,.940,.765,.645,.555,.480/
      DATA IEMX /9/
      SAVE
C
      I= IFIX(EM/DDE + 1.)
      E= FLOAT(I-1)*DDE
      IF(EM.LT.20.0.AND.IT.LE.2) GO TO 70
      GO TO (10,30),NSGNL
   10 TEMP = SNGL(CS(I+IS))
      S = TEMP + (EM-E)/DDE*(SNGL(CS(I+1+IS))- TEMP)
   20 RETURN
   30 IF(IT.GT.2) GO TO 60
      IF(I.LT.176) GO TO 50
   40 CALL CERROR('CALSGM1$')
   50 S=  NPSG(IT,I) +(EM-E)/DDE*(NPSG(IT,I+1) - NPSG(IT,I) )
      GO TO 20
   60 IF(I.GT.125) GO TO 40
      IT = IT - 2
      S=  PIPSG(IT,I) + (EM-E)/DDE *(PIPSG(IT,I+1) - PIPSG(IT,I) )
      GO TO 20
C** LOW ENERGY (.LT. 20 MEV) P-P OF N-P XSECTS FOR ELAST. SCATT. WITH H.
   70 CONTINUE
      IF(EM.LT.ENRGY(1)) CALL CERROR('CALSGM2$')
      DO 80  IE = 2,IEMX
         IF(EM.LE.ENRGY(IE)) GO TO 90
   80 CONTINUE
      CALL CERROR('CALSGM3$')
   90 S = ALOG(PPNP(IE-1,IT)) + (EM - ENRGY(IE-1)) /
     +    (ENRGY(IE) - ENRGY(IE-1)) *
     +    (ALOG(PPNP(IE,IT)) - ALOG(PPNP(IE-1,IT)))
      S = EXP(S) * 1.0E-24
      GO TO 20
      END
