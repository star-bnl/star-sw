*
* $Id: qnatchdata.inc,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: qnatchdata.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:46  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:29  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:49:17  mclareni
* Kernlib
*
*
*
* qnatchdata.inc
*
#if defined(CERNLIB_QMIBM)
      DATA NATCH / 64*0,   1, 9*0,  0,  6*1, 9*0, 8*1, 8*0, 6*1, 9*0
     +,             7*1,   0, 9*3,  0,    1, 5*0, 9*3,   0,   1, 5*0
     +,               0, 8*3, 3*0,  1, 15*0,   1, 2*0,   0, 9*4, 6*0
     +,               0, 9*4, 6*0,  1,    0, 8*4, 6*0, 10*2, 6*0  /
C-    Nature : 0 unseen, 1 others, 2 numeric, 3 lower, 4 upper case
#endif
