*
* $Id: pydat1.inc,v 1.1 1999/10/10 21:30:40 fisyak Exp $
*
* $Log: pydat1.inc,v $
* Revision 1.1  1999/10/10 21:30:40  fisyak
* Add pythia includes
*
* Revision 1.2  1998/02/17 18:14:41  fisyak
* Add variable definitions
*
* Revision 1.1  1998/02/05 22:56:31  fisyak
* Import from d0gstar
*
*Revision 1.1  1997/09/02  17:35:39  fisyak
*Add pythia version 6.103
*
*
*
* pydat1.inc
*
      INTEGER       MSTU,               MSTJ
      DOUBLE PRECISION        PARU,               PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /PYDAT1/
