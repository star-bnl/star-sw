* $Id: gthsig.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE GTHSIG(ISGNL)
C
#include "gcalor/cjoint.inc"
#include "gcalor/ccomon.inc"
#include "gcalor/cxpd.inc"
#include "gcalor/chie.inc"
#include "gcalor/cbert2.inc"
C
C
C ** DE IS THE CONST. ENERGY SPACING COMMON T0 P-P,N-P,PI+P,PI-P XSECTS
      DIMENSION ISA(4),EMX(4)
      SAVE ISA,EMX
C
      GO TO (10,180),ISGNL
   10 CONTINUE
CZ BERT dataset already read by CRBERT called by CALINI 19.june 92
      CALL CSHXD
      DO 20 I=1,4
   20 ISA(I) = LOCX(3,I)
C**** COMPUTE TOTAL P-P AND N-P XSECTS (SINGLE PRODUCTION + DOUBLE PRODU
C****  + ELASTIC) AND STORE P-P BEGINNING AT CS(3794) ANP N-P BEGINNING
C****  CS(3970).
      ISP=995
      IDP=1153
      DO 50 IT=1,2
         IS= ISA(IT)
         DO 30 I=1,158
            TEMP= CS(IS+18+I)
   30    CS(IS+18+I) = TEMP + CS(ISP+I)
         ISP = ISP + 288
         DO 40 I=1,130
            TEMP = CS(IS+46+I)
   40    CS(IS+46+I) = TEMP + CS(IDP+I)
   50 IDP = IDP + 288
C**** COMPUTE TOTAL PI+(SNGL PROD.+ELAS) AND PI-(SNGL PROD.+EXCHNG +ELAS
C**** AND STORE PI+-P BEGINNING AT CS(3668) AND PI--P BEGINNING AT CS(35
      ISP =2009
      DO 70 IT =3,4
         IS= ISA(IT)
         DO 60 I=1,117
            TEMP = CS(IS +9+I)
   60    CS(IS+9+I) = TEMP + CS(ISP+I)
   70 ISP = ISP +234
      IEX =3415
      IS = ISA(4)
      DO 80 I=1,126
         TEMP = CS(IS+I)
   80 CS(IS+I) = TEMP + CS(IEX+I)
      DO 100 IT = 1,2
         IS = ISA(IT)
         DO 90 I=1,176
   90    NPSG(IT,I) = SNGL( CS(IS+I) )
  100 CONTINUE
      DO 120 IT = 1,2
         IS = ISA(IT+2)
         DO 110 I=1,126
  110    PIPSG(IT,I) = SNGL( CS(IS+I) )
  120 CONTINUE
C *****
C **** SELECT MAX. TOT.XSECTS FOR X ON PROT. IN ENERGY RANGE EMIN(X) TO
C **** WHERE X = PROT.,NEUT.,PI+,PI-.
      IF(EMAX.LT.EHIN)GO TO 130
      EMX(1)=EHIN
      EMX(2)=EHIN
      EMX(3)=EHIPI
      EMX(4)=EHIPI
      GO TO 140
  130 CONTINUE
      EMX(1)= EMAX
      EMX(2)= EMAX
      EMX(3)=  2500.
      EMX(4)=  2500.
  140 CONTINUE
      DO 160 ITP =1,4
         IS= ISA(ITP)
         IT = ITP + ITP/4
         CALL CALSGM(1,ITP,IS,DE,EMIN(IT),IL,EL,SL)
         CALL CALSGM(1,ITP,IS,DE,EMX(ITP),IH,EH,SH)
         HSIGMX(IT) = AMAX1(SL,SH)
         IF(IL.GE.IH) GO TO 160
         IL = IL +1
         DO 150 I= IL,IH
            SIG = SNGL(CS(I+IS))
            IF(SIG. LE. HSIGMX(IT)) GO TO 150
            HSIGMX(IT) = SIG
  150    CONTINUE
  160 CONTINUE
      HSIGMX(4)=0.0
#if defined(ATLAS_CDEBUG)
      WRITE(6,10000)(HSIGMX(I),I=1,5)
#endif
10000 FORMAT(1H0,7HHSIGMX ,5E15.5)
  170 RETURN
  180 IT = ITYP - ITYP/5
      IS = ISA(IT)
      CALL CALSGM(2,IT,IS,DE,EC(NO),I,EI,HSIG)
      GO TO 170
      END
