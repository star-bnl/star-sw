*
* $Id: agcrdig.inc,v 1.1 1998/04/10 15:30:45 fisyak Exp $
*
* $Log: agcrdig.inc,v $
* Revision 1.1  1998/04/10 15:30:45  fisyak
* Move gstar into CVS
*
*
*
* agcrdig.inc
*
*CMZ :  1.00/00 04/06/95  23.12.34  by  Pavel Nevski
*-- Author :    Pavel Nevski   12/12/94
*  AGCRDIG is communication between AgFHIT0/1, AgFDIGI, AgFPATH, AGFDPAR.
      Integer          IWA,   JS,JD,JX,JXD,JDS,JDU
      COMMON /AGCRDIG/ IWA(2),JS,JD,JX,JXD,JDS,JDU
      Integer          Iprin,Nvb,Nw,Last,Mb,Nc1,Nc2,Iv,Ia
      Character*4                                         cs,cd
      COMMON /AGCHITV/ Iprin,Nvb,Nw,Last,Mb,Nc1,Nc2,Iv,Ia,cs,cd
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
