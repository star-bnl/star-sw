*
* $Id: agcstep.inc,v 1.1 1998/04/10 15:30:46 fisyak Exp $
*
* $Log: agcstep.inc,v $
* Revision 1.1  1998/04/10 15:30:46  fisyak
* Move gstar into CVS
*
*
* -----------------------------------------------------------------
*
* agcstep.inc
*
*CMZ :  1.30/00 30/07/96  19.09.55  by  Pavel Nevski
*CMZ :  1.00/01 29/01/96  13.28.10  by  Pavel Nevski
*-- Author :    Pavel Nevski   12/12/94
* Connection between AgGSTEP, AgGHIT and a user step routine.
      Real              vect0,vloc0,vloc,xloc,Astep,Adestep,Hits
      COMMON /AGCSTEP/  vect0(7),vloc0(7),vloc(7),xloc(7),
     +                  Astep,Adestep
      COMMON /AGCSTEPH/ Hits(15)
      Integer           IhaveMore
      COMMON /AGCSTEPI/ IhaveMore
*
