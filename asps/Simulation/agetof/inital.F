*
* $Id: inital.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: inital.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:13  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:06  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:43  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE INITAL(I1,I2,I3,I4,I5,I6,I7)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER I1,I2,I3,I4,I5,I6,I7
      INTEGER B,E,R,S,V,U,W,X
      EQUIVALENCE (O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U)
     *,(O(7),W),(O(8),X)
      INTEGER J/0/,K/0/,L/0/,N/0/,IUNI/0/
      CHARACTER*256 FILE,FF,FFO,FFL,FFE,FFD,FL
      INTEGER LENOCC,NN
      COMMON /NAMEF/ NN,FILE
      INTEGER LLIS,DDEB
      COMMON /MTRAC/ LLIS,DDEB
      CHARACTER VERSION*20,DVERS*4
      COMMON /MVERS/ VERSION,DVERS
      CHARACTER CL*2,CN*2,CF*2/'f '/
      INTEGER ICSDEBUG  
      COMMON /CSDEBUG/ ICSDEBUG 
C PLN - 20-mar-93 - this was for writing binary out - not used in my version
C IF I3<0 {MODE=1;SEQN=5;KOUNT=FREE+1;FRT=0;UNIT=5;
C
C           .(1)=0;.(2)=KOUNT; WRITE(MINP) (O(K),K=1,KOUNT);  QUIT;}
      DO 21 K = 8,500000
         O(K)=0
21    CONTINUE
      O(9)=I1
      IUNI=I1
      O(56)=I2
      O(57)=I3
      O(58)=I4
      O(14)=I5
      O(16)=I6
      O(37)=I7
      O(41)=O(37)
      O(38)=404+O(37)
      O(36)=536+O(37)
      O(43)=536+O(41)
      O(42)=404+O(41)
C+SELF,IF=UNIX.
      J=0
      VERSION='0 '
      DVERS='0 '
      FFD='  '
30    CONTINUE
         J=J+1
         CL=CN
         FF='   '
         CALLGETARG(J,FF)
         L=LENOCC(FF)
         IF(L.EQ.0)GOTO31
         N=L
         DO 41 K = 1,L
            IF (FF(K:K).EQ.'. ') N=K-1
41       CONTINUE
         FL=FF
         CALLCUTOL(FL)
         CN=FL
         IF (CN(1:1).EQ.'- ') THEN
            IF (CN.EQ.'-h') THEN
               print *, 'Parser usage: [-i] file[.g] [-o,e,l,d file]',
     >                  ' [-V version] [-t]'
               stop
            ELSE IF (CN.EQ.'-o') THEN
               O(57)=-6
            ELSE IF (CN.EQ.'-l') THEN
               LLIS=1
               O(56)=6
            ELSE IF (CN.EQ.'-e') THEN
               DDEB=1
               O(58)=6
            ELSE IF (CN.EQ.'-t') THEN
               DDEB=1
               ICSDEBUG=1
            ELSE IF (CN.EQ.'-v') THEN
               VERSION=VERSION(1:LENOCC(VERSION))//FL(3:3)
            END IF
         ELSE
            IF (CL.EQ.'-v') THEN
               VERSION=VERSION(1:LENOCC(VERSION))//FL(1:1)
            ELSE IF (CL.EQ.'-o') THEN
               O(57)=27
               FFO=FF(1:L)
               FFL=FF(1:N)//'.l'
               FFE=FF(1:N)//'.e'
            ELSE IF (CL.EQ.'-l') THEN
               O(56)=28
               FFL=FF(1:L)
            ELSE IF (CL.EQ.'-e') THEN
               O(58)=29
               FFE=FF(1:L)
            ELSE IF (CL.EQ.'-d') THEN
               FFD=FF
C         CL==-i
            ELSE
               FILE=FF
               NN=N
               IF (FF(N-1:N).EQ.'.o') VERSION=VERSION(1:LENOCC(VERSION))
     *         //CF
               FFO=FF(1:N)//'.f'
               FFL=FF(1:N)//'.l'
               FFE=FF(1:N)//'.e'
            END IF
         END IF
      GOTO 30
31    CONTINUE
      if (FFD.eq.'  ') CALL AFNAME(FFD,'.def')
      CALLCUTOL(VERSION)
      IF (INDEX(VERSION,CF(1:1)).GT.0) CALLFILTER
      OPEN (O(9),FILE=FFD, STATUS='OLD')
      IF ((0.LT.O(57)).AND.(O(57).NE.6)) OPEN (O(57),FILE=FFO, STATUS='U
     *NKNOWN')
      IF ((0.LT.O(56)).AND.(O(56).NE.6)) OPEN (O(56),FILE=FFL, STATUS='U
     *NKNOWN')
      IF ((0.LT.O(58)).AND.(O(58).NE.6)) OPEN (O(58),FILE=FFE, STATUS='U
     *NKNOWN')
      IUNI=ABS(O(57))
      WRITE(IUNI,'(''C: definitions from '',a)') FFD(1:LENOCC(FFD))
C+SELF.
      CALLRAW
      O(32)=1705
      DO 51 K = 1,6
         O(K)=438021
51    CONTINUE
      O(40)=438021
      O(55)=O(76)
      O(113)=113
      O(75)=-13
      O(70)=-14
      O(U)=O(79)
      W=376043-1
      O(W)=O(79)
      CALLDUMDUM(44,3625,3675,3725,3775,3825,3875,3975,4025,0,0)
      CALLNXTCRD
99999 RETURN
      END
