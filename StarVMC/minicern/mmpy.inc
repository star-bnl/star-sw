*
* $Id: mmpy.inc,v 1.1 2005/06/09 00:05:16 fisyak Exp $
*
* $Log: mmpy.inc,v $
* Revision 1.1  2005/06/09 00:05:16  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:11  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mmpy.inc
*
          LXI1  =  1
          LZI   =  1
          DO 12     I  =  1, M
             LXIJ  =  LXI1
             LYJ   =  1
             SUM   =  ZERO
             DO 11  J  =  1, N
                SUM  =  F(X(LXIJ),Y(LYJ),SUM)
                LXIJ =  LXIJ + JX
                LYJ  =  LYJ + JY
  11            CONTINUE
             Z(LZI)  =  SUM
             LXI1    =  LXI1 + IX
             LZI     =  LZI + IZ
  12         CONTINUE
          RETURN
          END
