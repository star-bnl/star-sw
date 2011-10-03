* $Id: agcrfun.inc,v 1.1.1.1 2004/01/12 23:49:38 potekhin Exp $
* $Log: agcrfun.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
*
* Revision 1.1  2001/02/27 10:14:58  nevski
*  first working release
*
*     Road definition function and its parameters
      Integer          IFunAdr,IfunFlag
      Real             XXf(6),YYf(6),   Xopnt,Xrz,Xsas, Xzrc,Xphic,Xres,
     >                                  Yphi, Yzr,Ydphi,Ydzr,Ysign,Yres
      Common /AgCRFun/ IFunAdr,Ifunflag,Xopnt,Xrz,Xsas, Xzrc,Xphic,Xres,
     >                                  Yphi, Yzr,Ydphi,Ydzr,Ysign,Yres
      Equivalence      (XXf,Xopnt), (YYf,Yphi)
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
