*
* $Id: rzcstr.inc,v 1.1.1.1 2004/07/17 20:01:58 perev Exp $
*
* $Log: rzcstr.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:58  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:50  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:28  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 10:46:55  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_RZCSTR_INC
#define CERNLIB_ZEBRA_RZCSTR_INC
*
*
* rzcstr.inc
*
      PARAMETER     (MAXFILES=128, MAXSTRIP=21)
      CHARACTER*128  RZNAMES(MAXFILES),RZSFILE(MAXSTRIP)
      COMMON/RZCSTRC/RZNAMES,RZSFILE
      COMMON/RZCSTRI/ISLAST,ISTRIP(MAXFILES),NSTRIP(MAXFILES),
     +                      NRSTRIP(MAXFILES)
C

#endif
