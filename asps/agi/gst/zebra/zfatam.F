*
* $Id: zfatam.F,v 1.2 1998/04/16 16:11:50 fisyak Exp $
*
* $Log: zfatam.F,v $
* Revision 1.2  1998/04/16 16:11:50  fisyak
* put gstar in cvs
*
* Revision 1.1  1998/04/10 15:31:14  fisyak
* Move gstar into CVS
*
*
#include "packlib/zebra/zebra/pilot.h
*CMZ :  1.30/00 19/03/97  21.37.20  by  Pavel Nevski
*-- Author :    Pavel Nevski   16/03/97
      subroutine ZFATAM  (message)
      character  message*(*)
#include "packlib/zebra/zebra/mzca.inc"
#include "packlib/zebra/zebra/zfatalch.inc"
#include "packlib/zebra/zebra/zfatalre.inc"
      print *,' ********** ZFATAM message: ',message,' **********'
      end
