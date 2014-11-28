*
* $Id: s_curpar.inc,v 1.1.1.3 2009/02/18 20:33:08 fisyak Exp $
*
* $Log: s_curpar.inc,v $
* Revision 1.1.1.3  2009/02/18 20:33:08  fisyak
* *** empty log message ***
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:17  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:43  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_S_CURPAR_INC
#define CERNLIB_GEANT321_S_CURPAR_INC
*
*
* s_curpar.inc
*
      PARAMETER (MXGKCU=MXGKGH)
      COMMON/CURPAR/WEIGHT(10),DDELTN,IFILE,IRUN,NEVT,NEVENT,SHFLAG,
     *              ITHST,ITTOT,ITLST,IFRND,TOFCUT,CMOM(5),CENG(5),
     *              RS,S,ENP(10),NP,NM,NN,NR,NO,NZ,IPA(MXGKCU),
     *              ATNO2,ZNO2
C

#endif
