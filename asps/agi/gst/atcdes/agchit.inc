*
* $Id: agchit.inc,v 1.1 1998/04/10 15:30:44 fisyak Exp $
*
* $Log: agchit.inc,v $
* Revision 1.1  1998/04/10 15:30:44  fisyak
* Move gstar into CVS
*
*
*  -----------------------------------------------------------------
*
* agchit.inc
*
*CMZ :  1.00/00 03/06/95  13.31.56  by  Pavel Nevski
*-- Author :    Pavel Nevski   12/12/94
*  AGCHITL and AGCHITV are communication between AgFHIT0/1/AggetDIG,AgFPATH.
      Integer          IWA,   JS,JD,JX,JDX,JXD,JDS,JDU
      COMMON /AGCHDIG/ IWA(2),JS,JD,JX,JDX,JXD,JDS,JDU
      Integer          Iprin,Iv,Ia,Nv,Mb,Nc1,Nc2,NumMX,Lp
      Parameter        (Lp=15)
      Real             Org,Fct
      Character*4      cs,cd
      COMMON /AGCHITV/ Iprin,Iv,Ia,Nv,Mb,Nc1,Nc2,cs,cd,
     >                 Org(Lp),Fct(Lp),NumMX(Lp)
*
