*
* $Id: vcpy.inc,v 1.1 2005/05/26 22:28:21 fisyak Exp $
*
* $Log: vcpy.inc,v $
* Revision 1.1  2005/05/26 22:28:21  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* vcpy.inc
*
          LXJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             Z(LZJ)  =  FUNCT( X(LXJ) )
             LXJ     =  LXJ + JX
             LZJ     =  LZJ + JZ
  10         CONTINUE
          RETURN
          END
