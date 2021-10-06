* $Id: crbert.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CRBERT
C*********************************************************
C
C  Read BERT Data into commons used by BERT,PCOL,GTHSIG
C  DRES
C
C*********************************************************
C
#include "gcalor/cjoint.inc"
#include "gcalor/cbert.inc"
#include "gcalor/cdresc.inc"
#include "gcalor/cevcm.inc"
C
      REWIND NBERTP
C ---- read cascade data --------------
      I1 = 1
      I2 = 600
      DO 10 J=1,4
         READ(NBERTP,10000) (CRSC(I),I=I1,I2)
         I1 = I1 + 600
         I2 = I2 + 600
   10 CONTINUE
      READ(NBERTP,10000) (TAPCRS(I),I=1,29849)
C ----- read evaporation data -----------
      DO 30 K=1,250
         DO 20 J=1,20
            WAPS(K,J) = 0.0
   20    CONTINUE
   30 CONTINUE
      READ(NBERTP,10000) (P0(J),P1(J),P22(J),J=1,1001)
      READ(NBERTP,10100) (IA(J),J=1,6),(IZ(J),J=1,6)
      READ(NBERTP,10000) (RHO(J),J=1,6),(OMEGA(J),J=1,6)
      READ(NBERTP,10000) (EXMASS(J),J=1,6)
      READ(NBERTP,10000) (CAM2(J),J=1,130)
      READ(NBERTP,10000) (CAM3(J),J=1,200)
      READ(NBERTP,10000) (CAM4(J),J=1,130)
      READ(NBERTP,10000) (CAM5(J),J=1,200)
      READ(NBERTP,10000) ((T(I,J),J=1,7),I=1,3)
      READ(NBERTP,10000) (RMASS(J),J=1,297)
      READ(NBERTP,10000) (ALPH(J),J=1,297)
      READ(NBERTP,10000) (BET(J),J=1,297)
      READ(NBERTP,10000) ((WAPS(I,J),I=1,250),J=1,20)
10000 FORMAT(5E16.8)
10100 FORMAT(6I10)
      RETURN
      END
