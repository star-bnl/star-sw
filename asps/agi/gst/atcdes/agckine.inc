*
* $Id: agckine.inc,v 1.1 1998/04/10 15:30:45 fisyak Exp $
*
* $Log: agckine.inc,v $
* Revision 1.1  1998/04/10 15:30:45  fisyak
* Move gstar into CVS
*
*
*T=geant. ---------------------------------------------------------
*
* agckine.inc
*
*CMZ :          02/07/97  12.34.36  by  Pavel Nevski
*CMZ :  1.30/00 17/04/97  21.01.16  by  Pavel Nevski
*CMZU:  1.00/01 14/01/96  17.46.29  by  Pavel Nevski
*-- Author :    Pavel Nevski   12/01/96
*    AGI general data card information
      Integer          IKineOld,IdInp,Kevent,
     >                 Iback,IbackOld,IbMode,IbBefor,IbAfter,
     >                 IbCurrent,IvCurrent,Ioutp,IoutpOld
      Real             AVflag,AVcoor,AVsigm,Ptype,PTmin,PTmax,
     >                 Etamin,Etamax,PhiMin,PhiMax,Ptflag,
     >                 Zmin,Zmax,BgMult,BgTime,BgSkip,
     >                 Pxmin,Pxmax,Pymin,Pymax,Pzmin,Pzmax
      COMMON /AgCKINE/ IKineOld,IdInp,Kevent(3),
     >                 AVflag,AVcoor(3),AVsigm(3),
     >                 Ptype,PTmin,PTmax,Etamin,Etamax,
     >                 PhiMin,PhiMax,Ptflag,Zmin,Zmax,
     >                 Pxmin,Pxmax,Pymin,Pymax,Pzmin,Pzmax
      COMMON /AgCKINB/ Iback,IbackOld,IbMode,IbBefor,IbAfter,
     >                 BgMult,BgTime,BgSkip,IbCurrent,IvCurrent
      COMMON /AgCKINO/ Ioutp,IoutpOld
      Character*20     CoptKine,CoptBack,CoptOutp
      COMMON /AgCKINC/ CoptKine,CoptBack,CoptOutp
      Character*20     CrunType
      COMMON /AgCKINR/ CrunType
      Integer          Ncommand
      Character*20     Ccommand
      COMMON /AgCCOMD/ Ncommand,Ccommand
      Integer          IUHIST
      Character*80            CFHIST,CDHIST
      COMMON /AgCHIST/ IUHIST,CFHIST,CDHIST
*
      Integer          NtrSubEV,NkineMax,NhitsMax,NtoSkip,NsubToSkip,
     >                 Nsubran,ItrigStat,NsubEvnt,IsubEvnt,
     >                 Make_Shadow,Flag_Secondaries
      Real             Cutele_Gas,VertexNow
      COMMON /AgCSUBE/ NtrSubEV,NkineMax,NhitsMax,
     >                 NtoSkip,NsubToSkip,Nsubran(2)
      COMMON /AgCSTAR/ Make_Shadow,Cutele_Gas,Flag_Secondaries
      COMMON /AgCstat/ ItrigSTAT,NsubEvnt,IsubEvnt,VertexNow(3)
*
*    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
