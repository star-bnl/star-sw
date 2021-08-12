* $Id: fumuse.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: fumuse.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:55  nevski
*  first working release
*
*   fumili users commons
      INTEGER         NA, NS, NP, NN, MM
      PARAMETER       (NA=10, NS=8, NP=400)
      REAL            EX,YY
      COMMON /FUMNED/ NN,MM
      COMMON /FEXDA/  EX(NS,NP)
      COMMON /FUMYY/  YY(0:NA,NP)
*     - - - - - - - - - - - - - - - - - - - -
