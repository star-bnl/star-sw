* $Id: genhit.inc,v 1.3 2004/03/01 17:26:33 fisyak Exp $
* $Log: genhit.inc,v $
* Revision 1.3  2004/03/01 17:26:33  fisyak
* Get rid of staf
*
* Revision 1.2  2004/01/15 02:39:23  fisyak
* Bug fixes
*
* Revision 1.1  2001/02/27 10:15:55  nevski
*  first working release
*
* a generic common for any hits
*     - - - - - - - - -
      real             QQ(100)
      Equivalence     (QQ(1),id)
      Integer         id,trac,next,volume
      Real            xhloc,xhglb,chloc,pmomg(4),radi,rrad,phi,theta,
     >                eta,tdr,tof,Slen,Step,Etot,Lgam,Lpto,Elos,User,
     >	              Unkn
      common /genhit/ id,trac,next,volume,xhloc(3),xhglb(3),chloc(3),
     >                pmomg,radi,rrad,phi,theta,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
*     - - - - - - - - -
*
