* $Id: genhit.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: genhit.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/12/14 23:11:39  nevski
* upgrade atlsim library to atlsim main level
*
* Revision 1.1  2001/02/27 10:15:55  nevski
*  first working release
*
* a generic common for any hits
*     - - - - - - - - -
      real             QQ(100)
      Equivalence     (QQ(1),id)
      Integer         id,trac,next,volume
      Real            xhloc,xhglb,chloc,pmomg,radi,rrad,phi,theta,eta,
     >                tdr,tof,Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn
      common /genhit/ id,trac,next,volume,xhloc(3),xhglb(3),chloc(3),pmomg(4),
     >                radi,rrad,phi,theta,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
*     - - - - - - - - -
*
