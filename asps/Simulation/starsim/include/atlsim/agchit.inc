*
* $Id: agchit.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Name:  $
*
*  -----------------------------------------------------------------
*
* agchit.inc
*
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
