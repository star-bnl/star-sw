* $Id: cqene.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CQENE(Z)
      IMPLICIT REAL *8  (A-H,O-Z)
#include "gcalor/cbert3.inc"
#include "gcalor/cisob2.inc"
      DIMENSION Z(380)
      SAVE
C
      CD=COM*1.0D2
      I=IDINT(CD+1.00D0)
      AZ=Z(I)
      IF(I.EQ.1)GOTO150
   10 BZ=Z(I+1)
      IF(101-(I+1))70,20,30
   20 CZ=BZ+5.0D-1*(BZ-AZ)
      GOTO40
   30 CZ=Z(I+2)
   40 XZ=CD-DFLOAT(I-1)
      SCA=CZ-AZ
C F(2)-F(0)
   50 SBA=BZ-AZ
C F(1)-F(0)
      SQA=AZ*AZ
C F(0)**2
      SQAC=SQA-CZ*CZ
C F(0)**2-F(2)**2
      SQBA=BZ*BZ-SQA
C F(1)**2-F(0)**2
      RB=SQAC+SQBA+SQBA
C (ASQ-CSQ)+2(BSQ-ASQ)
CZ
CZ  changed in order to keep exponent small 5/21/92
      RC=AZ*1.0D-20*CZ*SCA-SBA*1.0D-20*(2.0D0*AZ*BZ+XZ*(BZ-CZ)*SCA)
CZ   RC is 1E-20 smaller than it supposed to be !!!!
      RA=SCA-SBA-SBA
C (C-A)-2(B-A)
      IF(RA.NE.0.0)GOTO60
      COM=AZ+XZ*SBA
      GOTO80
   60 CONTINUE
CZ                               \/  factor 1E-20 in RC !!
      DISC=RB*1.0D-20*RB-4.0D0*RA*RC
      IF(DISC)70,90,90
C B**2-4AC
   70 CALL CERROR('CQENE1$')
   80 RETURN
CZ                      \/ correct for factor 1E-20
   90 DISC=DSQRT(DISC)*1.0D10
CZ   end of change
CZ
      PLUS=(DISC-RB)/(RA+RA)
      AMINUS=(-RB-DISC)/(RA+RA)
      IF(I.EQ.1)GOTO160
  100 IF(PLUS.GT.BZ)GOTO120
      IF(PLUS.LT.AZ)GOTO120
      IF(AMINUS.GT.BZ)GOTO110
      IF(AMINUS.GE.AZ)GOTO140
  110 COM=PLUS
      GOTO80
  120 IF(AMINUS.GT.BZ)GOTO70
      IF(AMINUS.LT.AZ)GOTO70
  130 COM=AMINUS
      GOTO80
  140 RA=XZ*SBA+AZ
      RB=DABS(RA-AMINUS)
      RC=DABS(RA-PLUS)
      IF(RB.GT.RC)GOTO110
      GOTO130
  150 CZ=Z(I+1)
      SCA=CZ-AZ
      BZ=AZ+SCA*7.071067812D-1
      XZ=CD+CD
      GOTO50
C (CZ-AZ)(CZ-AZ)=C,CZ=MASS FOR R=1,AZ=MASS FOR R=0, C=CONST.FOR PARABOLA
C (M-AZ)(M-AZ)=0.5*C,DETERMINES MASS,BZ,FOR R=1/2
  160 BZ=CZ
      XZ=XZ-CD
      SBA=CZ-AZ
      GOTO100
      END
