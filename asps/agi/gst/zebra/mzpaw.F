*
* $Id: mzpaw.F,v 1.5 1998/07/20 20:17:32 perev Exp $
*
* $Log: mzpaw.F,v $
* Revision 1.5  1998/07/20 20:17:32  perev
* Mods for exe SGI64 and HP rubber Zebra
*
* Revision 1.4  1998/06/05 20:55:36  perev
* AGI commit
*
* Revision 1.3  1998/04/16 16:58:07  fisyak
* 2nd pass with gstar
*
*
#include "zebra/pilot.h"
*CMZ :          06/12/97  11.36.55  by  Pavel Nevski
*-- Author :
      SUBROUTINE MZPAW (NWORDS,CHOPT)
 
C-    Initialize the store /PAWC/ of at least 10000 words;
 
      COMMON /PAWC/  NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKU,IFENCE(5)
     +,              LMAIN, IPAW(9989)
C--------------------------------------------------------------------
      CHARACTER*(*) CHOPT
 
      NW   = abs (NWORDS)
      NW   = max (NW,100000)
      I1   = LOCF(IPAW(1))
      I2   = memget (NW*4+1000)
      I2   = iudivd (i2,4)
      if (i2 .le. i1) then
        write(*,*) 'MZPAW. Error: No or Wrong space ',i1,i2,NW
        stop 
      endif
      LL   = max    (I2-I1,0)
      NW   = NW + LL
      KWW  = LL + 100
C
      CALL MZSTOR (IXPAWC,'/PAWC/',' ',IFENCE,LMAIN,IPAW,IPAW,
     +                                 IPAW(KWW+100),IPAW(NW-100))
      CALL MZWORK (IXPAWC,IPAW(9989),IPAW(KWW),5)
      CALL MZWORK (IXPAWC,IPAW(50),IPAW(5000),0)
c
      NWPAW  = NW
      IHBOOK = 0
      IXHIGZ = 0
      IXKU   = 0
      RETURN
      END
