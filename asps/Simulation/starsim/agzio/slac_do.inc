* $Id: slac_do.inc,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: slac_do.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.1  2001/02/27 10:14:19  nevski
*  first working release
*
C Data offsets for standard SLAC words
      INTEGER    NHDWAC,NPAKAC,JOSYS1,JOSYS2,JOSYS3
      common /slac_do_com/ NHDWAC
*     PARAMETER (NHDWAC = 3)
      PARAMETER (NPAKAC = 100000)
      PARAMETER (JOSYS1 = 1)
      PARAMETER (JOSYS2 = 2)
      PARAMETER (JOSYS3 = 3)

