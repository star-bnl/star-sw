* $Id: agcrtab.inc,v 1.1.1.1 2004/01/12 23:49:49 potekhin Exp $
* $Name:  $
      Integer           Lstack,     Lr,   Lhead,   Lpack
      Parameter        (Lstack=2000,Lr=15,Lhead=52,Lpack=512)
*
*                                            subsystem descriptor
      Integer          Isys,Nplane,Itp,Iset,Idet,kkk,kfi,krz,mrz,NumPmx,
     +                 Nadr,Istart,Inamd,Iaxr,Iaxt,Iaxz,Ires, nam,nmx
      Real             Rstack,drzm,RRmin,RRmax,ZZmin,ZZmax
*
      Common /AgCRtab/ Isys,Nplane,Itp,Iset,Idet,kkk,kfi,krz,mrz,drzm,
     +                 RRmin,RRmax,ZZmin,ZZmax,
     +                 NumPmx,Nadr,Istart,Inamd,Iaxr,Iaxt,Iaxz,Ires,
     +                 Nam(15),Nmx(15),  Rstack(Lr,Lstack)
      Integer                            Istack(Lr,Lstack)
      Equivalence      (Rstack,Istack)
*                                            plane descriptor
      Real             rzp,RZmin,RZmax,Fmin,Fmax,Dfi,Dzr,tilt,sas,Fcent
      Integer          Nfi,Nzr,Nump,Iadr,Jres,Lpacku
      Common /AgCRta1/ rzp,RZmin,RZmax,Fmin,Fmax,Dfi,Dzr,tilt,sas,Fcent,
     +                 Nfi,Nzr,Nump,Iadr,Jres,Lpacku
*                                            detector size
      Integer          Ishape,Npar
      Real                         AgTran,   AgRmat,    Par
      Common /ARCplan/ Ishape,Npar,AgTran(3),AgRmat(10),Par(50)
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

