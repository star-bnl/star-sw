*
* $Id: agcstep.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Name:  $
*
* -----------------------------------------------------------------
*
* agcstep.inc
*
* Connection between AgGSTEP, AgGHIT and a user step routine.
      Real              vect0,vloc0,vloc,xloc,Astep,Adestep,Hits
      COMMON /AGCSTEP/  vect0(7),vloc0(7),vloc(7),xloc(7),
     +                  Astep,Adestep
      COMMON /AGCSTEPH/ Hits(15)
      Integer           IhaveMore
      COMMON /AGCSTEPI/ IhaveMore
*
