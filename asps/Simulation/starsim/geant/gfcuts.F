* $Id: gfcuts.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
* $Name:  $
* $Log: gfcuts.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.1  2001/06/14 00:11:06  nevski
* return cuts in a material
*
************************************************************************
      SUBROUTINE GFCUTS(MNAME,CUTS)
*                                                                      *
* Description : given the medium name extract its physics cuts         *
*                                                                      *
************************************************************************
#include "commons/typing.inc"
#include "geant321/gcbank.inc"
*
	character        MNAME*(*),CNAME*20
	Integer          i,Li,Lj
	real             CUTS(10)
*
 	If (JTMED.le.0) Return
	Call UCopy(Q(JTMED+1),Cuts,10)
	Do i=1,IQ(JTMED-2)
	   Li=LQ(JTMED-i)
	   If (Li .le. 0) Go to 101
	   Call UHToC(IQ(Li+1),4,CNAME,20)
	   If (CNAME.ne.MNAME) Go to 101
	   Lj=LQ(Li)
	   If (Lj .le. 0) Go to 101
           Call UCopy(Q(Lj+1),Cuts,10)
	   return
101        continue
	enddo
*
	END
