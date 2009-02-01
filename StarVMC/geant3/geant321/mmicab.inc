*
* $Id: mmicab.inc,v 1.1.1.2 2009/02/01 17:10:22 fisyak Exp $
*
* $Log: mmicab.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:22  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:27  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:34  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:45  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_MMICAB_INC
#define CERNLIB_GEANT321_MMICAB_INC
*
*
* mmicab.inc
*
C
      COMMON/ MMICAP / LMAG2,LCISO,LGE2MO,LMOMA,LMOX1,LMOX2,
     +                 LMOX3,LMOX4
      COMMON/ MICPAR / LMIST
      COMMON/ MICFIL / LMIFIL
      COMMON/ MICTMP / LTEMP,NTUNIT,NTNAME,NTMPNI,NTCOMM,NTDATS,NTLIST

#endif
