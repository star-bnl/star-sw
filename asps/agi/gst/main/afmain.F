*
* $Id: afmain.F,v 1.1 1998/04/16 16:59:18 fisyak Exp $
*
* $Log: afmain.F,v $
* Revision 1.1  1998/04/16 16:59:18  fisyak
* 2nd pass with gstar
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if !defined(CERNLIB_CC)
*CMZ :          06/12/97  11.46.00  by  Pavel Nevski
*-- Author :    Pavel Nevski   06/12/97
       program afmain
       call agmain
       end
#endif
