*
* $Id: vdiv.inc,v 1.1 2009/02/25 00:08:29 fisyak Exp $
*
* $Log: vdiv.inc,v $
* Revision 1.1  2009/02/25 00:08:29  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:20  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:21  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vdiv.inc
*
          LXJ  =  1
          LYJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             IF(REALF(Y(LYJ)) .EQ. 0.)  GOTO 20
             Z(LZJ)  =  X(LXJ) / Y(LYJ)
             LXJ     =  LXJ + JX
             LYJ     =  LYJ + JY
             LZJ     =  LZJ + JZ
  10      CONTINUE
          J  =  0
  20      IFAIL  =  J
          RETURN
          END
