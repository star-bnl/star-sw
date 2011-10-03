*
* $Id: hzc.inc,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: hzc.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:49  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:27  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 10:46:55  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_HZC_INC
#define CERNLIB_ZEBRA_HZC_INC
*
*     HISTOGRAM MANIPULATION
*
* hzc.inc
*
#if !defined(CERNLIB_QCDE)
      COMMON /HZC/   NQHNH,NQHPAK(3),NQHGW,NQHNX,QHDX,QHXL,QHSTEP,NQHAND
     +,              LQHH,LQHE,LQHC,LQHL,LQHLB,LQHLC,LQHD,LQHX,LQHT,LQHW
     +,              IQHCUT,IQHXXX,IQHINT,NQHLAB,IQHFMT,IQHW(8)
     +,              QHSMA,QHBIG,QHTOT,NQHNX2
     +,              LQHCUT,NQHEXP,IQHOPT(7)
*  -------------   for defaults   ------------------------------
#endif

#endif
