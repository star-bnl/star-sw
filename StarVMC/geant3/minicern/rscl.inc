*
* $Id: rscl.inc,v 1.1 2009/02/25 00:08:28 fisyak Exp $
*
* $Log: rscl.inc,v $
* Revision 1.1  2009/02/25 00:08:28  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:18  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:18  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* rscl.inc
*
          MM  =  M
          NN  =  N
          LDI   =  1
          LXI1  =  1
          LZI1  =  1
          DO 12     I  =  1, MM
             LXIJ  =  LXI1
             LZIJ  =  LZI1
             DO 11     J  =  1, NN
                Z(LZIJ)  =  D(LDI)*X(LXIJ)
                LXIJ  =  LXIJ + JX
                LZIJ  =  LZIJ + JZ
  11         CONTINUE
             LDI   =  LDI + ID
             LXI1  =  LXI1 + IX
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
