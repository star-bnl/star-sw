*
* $Id: aversion.F,v 1.1.1.1 2004/01/12 23:49:36 potekhin Exp $
* $Name:  $
* $Log: aversion.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:36  potekhin
*
* Revision 1.9  2002/02/22 15:20:19  nevski
* move pointers to 3.0.0
*
* Revision 1.8  2001/06/15 00:02:16  nevski
* merging differences between private cmz and this version
*
* Revision 1.7  2001/06/14 17:52:14  nevski
* bug fix
*
* Revision 1.6  2001/06/13 23:39:32  nevski
* compilation date/time inserted
*
*
      SUBROUTINE AVERSION
      implicit   none
      character  cversqq*80
      integer    LENOCC,i,j
#include "atlsim/agcvers.inc"
#include "atlsim/vidqq.inc"
      QFTITLCH=VIDQQ(5:60)
#include "atlsim/dateqq.inc"
#include "atlsim/timeqq.inc"
#include "atlsim/versqq.inc"
      END

