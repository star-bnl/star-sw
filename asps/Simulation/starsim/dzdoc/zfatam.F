*
* $Id: zfatam.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: zfatam.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.2  2001/03/25 17:50:32  nevski
* mzca splitted
*
* Revision 1.1  2001/02/27 10:15:29  nevski
*  first working release
*
#include "zebra/pilot.h"
*
*-- Author :    Pavel Nevski   16/03/97
      subroutine ZFATAM  (message)
      character  message*(*)
#include "zebra/mzca.inc"
#include "zebra/zfatalch.inc"
#include "zebra/zfatalre.inc"
      print *,' ********** ZFATAM message: ',message,' **********'
      end
