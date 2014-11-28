*
* $Id: raw.F,v 1.2 2009/08/12 14:42:24 jeromel Exp $
*
* $Log: raw.F,v $
* Revision 1.2  2009/08/12 14:42:24  jeromel
* ICHAR of expected size 1 was of size 2 to 3
*
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:17  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:32  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:46  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE RAW
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER B,E,R,S,V,U,W,X
      EQUIVALENCE (O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U)
     *,(O(7),W),(O(8),X)
      INTEGER K,J,P(100),BOO1(0:26),BOO2(0:26)
      EQUIVALENCE(P(1),O(4075))
      INTEGER LINE,K1/24000/
      COMMON /LINENUM/ LINE(100)
      DATA BOO1/26,24,24,24,24,8,0,3,38,107,1,1,0,46,107,1,1,0,38,107,1,
     *1,0,46,107,24,0/
      DATA BOO2/26,24,24,24,24,8,0,3,-1,107,0,0,0,-2,107,0,0,0,-1,107,0,
     *0,0,-2,107,24,0/
20    CALLRW(0,O(9),769,769+114)
      DO 31 K = 0,9
         IF(O(769+K).NE.(ICHAR('0')+K))GOTO20
31    CONTINUE
      LINE(5)=1
CMAKE INPUT AND OUTPUT SAME
      DO 41 K = 769,868
         O(K-100)=O(K)
41    CONTINUE
      DO 51 K = 1,100
         J=O(669+K-1)
         O(969+J) = K-1
51    CONTINUE
      O(31)=4
      O(34)=4175
      DO 61 K = 1,10
         O(K+100)=K+100
         O(K+110)=K+110
         O(100-K)=-K
61    CONTINUE
C              BLANK BAT BLB TRM LPR RPR LSB RSB LCB RCB
      CALLDUMDUM(76, 47, 81, 80, 79, 38, 46, 78, 84, 85, 86)
      O(86)=48
      O(87)=82
      DO 71 K = 0,5
         BOO1(K) = BOO1(K)+K1
         BOO2(K) = BOO2(K)+K1+100
71    CONTINUE
      DO 81 K = 0,26
         O(K+K1) = BOO1(K)
         O(K+100+K1) = BOO2(K)
81    CONTINUE
      CALLDEFINE(K1+8)
      CALLDEFINE(K1+108)
      P(46)=101
      P(38)=202
      P(89)=303
      P(90)=404
      P(91)=505
      P(42)=706
      P(43)=707
      P(44)=808
      P(45)=809
      P(47)=910
      P( 32)=303
      P( 12)=404
      P( 29)=505
      P( 11)=611
      P( 14)=612
      P( 16)=613
      P( 18)=614
      P( 23)=615
      P( 30)=616
      O(31)=6
99999 RETURN
      END
