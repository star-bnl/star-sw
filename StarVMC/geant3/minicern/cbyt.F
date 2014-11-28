*
* $Id: cbyt.F,v 1.1 2009/02/25 00:08:24 fisyak Exp $
*
* $Log: cbyt.F,v $
* Revision 1.1  2009/02/25 00:08:24  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:09  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:27:59  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:19  mclareni
* Kernlib
*
*
      SUBROUTINE CBYT (M,LM,IX,LX,NB)
C
C CERN PROGLIB# M421    CBYT            .VERSION KERNLNX  1.00  930507
C ORIG. 14/08/70 CL
C
      IX = OR ( AND (IX,
     +    NOT(LSHIFT (ISHFTR(NOT(0),32-NB), LX-1))),
     +    ISHFTR(LSHIFT(ISHFTR(M,LM-1),32-NB),33-LX-NB))
      RETURN
      END
