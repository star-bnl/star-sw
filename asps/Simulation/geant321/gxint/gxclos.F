*
* $Id: gxclos.F,v 1.1.1.1 2004/01/15 00:12:18 potekhin Exp $
*
* $Log: gxclos.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:18  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:50  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:49  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.33  by  S.Giani
*-- Author :
      SUBROUTINE GXCLOS(LUN)
*
*                To close unit LUN
*
#if defined(CERNLIB_IBM)
      CHARACTER*80 CHTEMP
      CHARACTER*3 CHLUN
#endif
*________________________________________________________
*
         CLOSE (LUN)
#if defined(CERNLIB_IBM)
         WRITE(CHLUN,'(I3)')LUN
         CHTEMP='FILEDEF '//CHLUN//'  CLEAR'
         CALL VMCMS(CHTEMP,ISTAT)
#endif
*
      END
 
