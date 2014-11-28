*
* $Id: vset.inc,v 1.1 2009/02/25 00:08:30 fisyak Exp $
*
* $Log: vset.inc,v $
* Revision 1.1  2009/02/25 00:08:30  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:21  fisyak
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
* vset.inc
*
          LZJ  =  1
          DO 10     J  =  1, N
             Z(LZJ)  =  S
             LZJ     =  LZJ + JZ
  10         CONTINUE
          RETURN
          END
