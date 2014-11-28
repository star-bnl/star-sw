*
* $Id: vmpa.inc,v 1.1 2009/02/25 00:08:30 fisyak Exp $
*
* $Log: vmpa.inc,v $
* Revision 1.1  2009/02/25 00:08:30  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:20  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:22  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vmpa.inc
*
          LXJ  =  1
          LYJ  =  1
          DO 10     J  =  1, N
             SUM  =  MPA( X(LXJ),Y(LYJ), SUM)
             LXJ  =  LXJ + JX
             LYJ  =  LYJ + JY
  10         CONTINUE
