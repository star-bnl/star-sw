* $Id: agckine.inc,v 1.2 2011/10/20 23:07:20 jwebb Exp $
* $Log: agckine.inc,v $
* Revision 1.2  2011/10/20 23:07:20  jwebb
* Added capability to provide a slope in the x,y vertex.
*
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
*
* Revision 1.1  2001/02/23 21:37:46  nevski
* new atlsim keeps
*
*    AGI general data card information
      Integer          IKineOld,IdInp,Kevent,
     >                 Iback,IbackOld,IbMode,IbBefor,IbAfter,
     >                 IbCurrent,IvCurrent,Ioutp,IoutpOld
      Real             AVflag,AVcoor,AVsigm,AVslope,Ptype,PTmin,PTmax,
     >                 Etamin,Etamax,PhiMin,PhiMax,Ptflag,
     >                 Zmin,Zmax,BgMult,BgTime,BgSkip,
     >                 Pxmin,Pxmax,Pymin,Pymax,Pzmin,Pzmax
      COMMON /AgCKINE/ IKineOld,IdInp,Kevent(3),
     >                 AVflag,AVcoor(3),AVsigm(3),AvSlope(3),
     >                 Ptype,PTmin,PTmax,Etamin,Etamax,
     >                 PhiMin,PhiMax,Ptflag,Zmin,Zmax,
     >                 Pxmin,Pxmax,Pymin,Pymax,Pzmin,Pzmax
      COMMON /AgCKINB/ Iback,IbackOld,IbMode,IbBefor,IbAfter,
     >                 BgMult,BgTime,BgSkip,IbCurrent,IvCurrent
      COMMON /AgCKINO/ Ioutp,IoutpOld
      Character*20     CoptKine,CoptBack,CoptOutp
      COMMON /AgCKINC/ CoptKine,CoptBack,CoptOutp
      Character*20     StrmKine,StrmBack,StrmOutp
      COMMON /AgCKINS/ StrmKine,StrmBack,StrmOutp
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
