*
* $Id: mutl.inc,v 1.1 2009/02/25 00:08:27 fisyak Exp $
*
* $Log: mutl.inc,v $
* Revision 1.1  2009/02/25 00:08:27  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:17  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:12  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mutl.inc
*
          LXII  =  1
          DO 12     IP1  =  2, N
             LXIJ  =  LXII
             LXJI  =  LXII
             DO 11  J  =  IP1, N
                LXIJ  =  LXIJ + JX
                LXJI  =  LXJI + IX
                X(LXJI)  =  X(LXIJ)
  11            CONTINUE
             LXII  =  LXII + IX + JX
  12         CONTINUE
          RETURN
          END
