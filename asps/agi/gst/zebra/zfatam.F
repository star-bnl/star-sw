*
* $Id: zfatam.F,v 1.4 1998/06/05 20:55:44 perev Exp $
*
* $Log: zfatam.F,v $
* Revision 1.4  1998/06/05 20:55:44  perev
* AGI commit
*
* Revision 1.3  1998/04/16 16:58:12  fisyak
* 2nd pass with gstar
*
*
#include "zebra/pilot.h"
*CMZ :  1.30/00 19/03/97  21.37.20  by  Pavel Nevski
*-- Author :    Pavel Nevski   16/03/97
      subroutine ZFATAM  (message)
      character  message*(*)
#include "zebra/mzca.inc"
#include "zebra/zfatalch.inc"
#include "zebra/zfatalre.inc"
      print *,' ********** ZFATAM message: ',message,' **********'
      end
