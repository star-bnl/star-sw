* $Id: event.inc,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
* $Log: event.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.1  2001/02/27 10:15:33  nevski
*  first working release
*
      PARAMETER (MXEVEN=12*MXGKGH)
      COMMON/EVENT / NSIZE,NCUR,NEXT,NTOT,EVE(MXEVEN)
C
