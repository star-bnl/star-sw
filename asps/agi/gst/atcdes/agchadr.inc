*
* $Id: agchadr.inc,v 1.1 1998/04/10 15:30:44 fisyak Exp $
*
* $Log: agchadr.inc,v $
* Revision 1.1  1998/04/10 15:30:44  fisyak
* Move gstar into CVS
*
*
*
* agchadr.inc
*
*CMZ :          15/12/97  23.04.02  by  Pavel Nevski
*-- Author :    Pavel Nevski   15/12/97
* sequence to let hadronic package produce more then MXGKIN secondaries
*     geant  message - NDONE positive
*     guhadr message - NREST positive
*
      INTEGER          NDONE,NREST
      COMMON /AGCHADR/ NDONE,NREST
 
