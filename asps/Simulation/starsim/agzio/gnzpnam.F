* $Id: gnzpnam.F,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: gnzpnam.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.4  2002/10/12 21:57:55  nevski
* decode more names
*
* Revision 1.2  2001/11/24 17:46:15  nevski
* more known particles
*
* Revision 1.1  2001/02/27 10:14:18  nevski
*  first working release
*
      SUBROUTINE GNZPNAM(IDPDG,CPNAM)
C---------------------------------------------------------------------
C GNPNAM
C Return the particle name CPNAM, given the Particle Data Group
C particle code IDPDG. This routine is faster for the particle codes
C under 30.
C
C Input : IDPDG - PD Group code for particle
C
C Output: CPNAM - Name of particle
C
C Original Version.                                 R. DeWolf   12/03/91
C---------------------------------------------------------------------
#include "commons/typing.inc"
      CHARACTER*(*) CPNAM
      INTEGER IDPDG
C
      INTEGER JDPDG,I,IER
      REAL PMASS,PCHAR
      CHARACTER*8 CNAM1,CNAM2
      CHARACTER*8 CQ(10),CQA(10),CL(10),CLA(10)
      CHARACTER*8 CB(10),CBA(10)
      CHARACTER*8 CMIS(4),CMISA(4)
      SAVE CL,CLA,CB,CBA,CQ,CQA,CMIS,CMISA
C
      DATA (CQ(I),I=1,4 ) /'d       ','u       ','s       ','c       '/
      DATA (CQ(I),I=5,8 ) /'b       ','t       ','l       ','h       '/
      DATA (CQ(I),I=9,10) /'g       ','????????'/
      DATA (CQA(I),I=1,4) /'d-bar   ','u-bar   ','s-bar   ','c-bar   '/
      DATA (CQA(I),I=5,8) /'b-bar   ','t-bar   ','l-bar   ','h-bar   '/
      DATA (CQA(I),I=9,10)/'g       ','????????'/
      DATA (CL(I),I=1,4 ) /'e-      ','nu_e    ','mu-     ','nu_mu   '/
      DATA (CL(I),I=5,8 ) /'tau-    ','nu_tau  ','hl-     ','nu_hlep '/
      DATA (CL(I),I=9,10) /'????????','????????'/
      DATA (CLA(I),I=1,4) /'e+      ','nu_e-bar','mu+     ','nu_mu-b '/
      DATA (CLA(I),I=5,8) /'tau+    ','nu_tau-b','hl+     ','nu-hl-b '/
      DATA (CLA(I),I=9,10)/'????????','????????'/
      DATA (CB(I),I=1,4)  /'gluon   ','photon  ','Z0      ','W+      '/
      DATA (CB(I),I=5,8)  /'H0      ','A0      ','H+      ','????????'/
      DATA (CB(I),I=9,10) /'graviton','????????'/
      DATA (CBA(I),I=1,4) /'gluon   ','photon  ','Z0      ','W-      '/
      DATA (CBA(I),I=5,8) /'H0      ','A0      ','H+      ','????????'/
      DATA (CBA(I),I=9,10)/'graviton','????????'/
C
      DATA (CMIS(I),I=1,4) /'pi +    ','pi 0    ','eta     ','K +     '/
      DATA (CMISA(I),I=1,4)/'pi -    ','pi 0    ','eta     ','K -     '/
      Character*8 C5(8)
      Integer     K5(8)
      data C5/'B0','B*0','B +','B*+','B_s0','B*s0','etaB','Upsilon'/
      data K5/ 511,  513,  521,  523,   531,   533,   551,   553/
C
C By default, write in the code itself:
      CPNAM = '?ERROR!        '
      CNAM1 = '????????'
C
      JDPDG=ABS(IDPDG)
C photons most common?
      IF (JDPDG.EQ.22) THEN
         WRITE(CNAM1,10000)CB(2)
C special treatment for pi and pi0,too:
      ELSE IF (JDPDG.EQ.211) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) CMIS(1)
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) CMISA(1)
      ELSE IF (JDPDG.EQ.111) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) CMIS(2)
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) CMISA(2)
      ELSE IF (JDPDG.EQ.221) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) CMIS(3)
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) CMISA(3)
      ELSE IF (JDPDG.EQ.213) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) 'rho+    '
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) 'rho-    '
      ELSE IF (JDPDG.EQ.321) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) CMIS(4)
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) CMISA(4)
      ELSE IF (JDPDG.EQ.311) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) 'K0      '
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) 'Kbar0   '
      ELSE IF (JDPDG.EQ.313) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) 'K*0     '
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) 'Kbar*0  '
      ELSE IF(JDPDG.EQ.0)THEN
         CNAM1 = '?      0'
      ELSE IF(JDPDG.LE.100)THEN
         IF(JDPDG.LT.11)THEN
            IF(IDPDG.GE.0) wRITE(CNAM1,10000)CQ(JDPDG)
            IF(IDPDG.LT.0) wRITE(CNAM1,10000)CQA(JDPDG)
         ELSE IF(JDPDG.LT.20)THEN
            IF(IDPDG.GE.0) wRITE(CNAM1,10000)CL(JDPDG-10)
            IF(IDPDG.LT.0) wRITE(CNAM1,10000)CLA(JDPDG-10)
         ELSE IF(JDPDG.LT.30)THEN
            IF(IDPDG.GE.0) wRITE(CNAM1,10000)CB(JDPDG-20)
            IF(IDPDG.LT.0) wRITE(CNAM1,10000)CBA(JDPDG-20)
         ELSE
            IF(IDPDG.EQ.36) wRITE(CNAM1,10000) 'A0      '
            IF(IDPDG.EQ.91) wRITE(CNAM1,10000) 'cluster '
            IF(IDPDG.EQ.92) wRITE(CNAM1,10000) 'string  '
         ENDIF
C     ELSE IF (JDPDG.GT.500 .and. JDPDG.LT.555)
C        do more 
C
      ENDIF
C
C If this did not work do other things:
C
      IF(CNAM1(1:1).EQ.'?')THEN
         CALL GNZPDAT(IDPDG,CNAM2,PMASS,PCHAR,IER)
         IF(IER.LT.0) GOTO 10
         IF(IER.EQ.0) CNAM1 = CNAM2
         IF(IER.GT.0) THEN
* try anti-particle
            CALL GNZPDAT(-IDPDG,CNAM2,PMASS,PCHAR,IER)
            IF(IER.EQ.0) CNAM1 = '-'//CNAM2
            IF(IER.GT.0) WRITE(CNAM1,10100)IDPDG
         ENDIF
      ENDIF
C
      CPNAM = CNAM1
C
   10 CONTINUE
10000 FORMAT(A8)
10100 FORMAT('?',I7)
      END
