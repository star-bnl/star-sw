      Subroutine  G2R_GET_SYS (Cset,Cdet,iprin,idigi)
      IMPLICIT    NONE
      character*4     Cset, Cdet, Cseta
      Integer         ISLFLAG,AgHITSET,Iprin,Idigi,Nhits
*     - - - - - - - - - - - - - - -    

      Cseta=Cset(1:3)//'H'
      Iprin = ISLFLAG (Cset,'PRIN')
      Idigi = ISLFLAG (Cset,'DIGI')
      Nhits = AgHITSET(Cseta,Cdet)
      If (Nhits .le. 0) Iprin = -1

      END
*
* --------------------------------------------------------------------
*
      Function   G2R_GET_HIT (Csys)
      IMPLICIT   NONE
#include "geant321/gcnum.inc"
#include "geant321/gcflag.inc"
*KEEP,genhit.
      Integer         Lnmax, Nmh
      Parameter       (Lnmax=50,Nmh=15)
      Integer         id,trac,next,volume,IQQ(Lnmax)
      Real            x,   xx,   c,   p   ,r,rr,phi,the,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn,QQ(Lnmax)
      common /genhit/ id,trac,next,volume,
     >                x(3),xx(3),c(3),p(4),r,rr,phi,the,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
      Equivalence     (QQ(1),IQQ(1),id)
*     - - - - - - - - - - - - - -
      character*4     Cset,Cdet,Chit
      Real            FHmin,FHmax,FHbin,Hits
      Integer         NVL,NumBV
      common /gensit/ Cset,Cdet, NVL(Nmh),NumBV(Nmh),hits(Nmh),
     >                FHmin(Nmh),FHmax(Nmh),FHbin(Nmh),chit(Nmh)
*     - - - - - - - - - - - - - -
*KEND.
      Integer         G2R_GET_HIT,AgHITGET,G2T_VOLUME_ID,i
      real            XYZTr(nmh)
      character*3     Csys
*
         i = AgHITGET (0,XYZTr)
         if (abs(trac).gt.NTRACK) then
           i       = -1
           IEOTRI  =  1
           if (Idebug.ne.0) then
              print *,' G2T_GET_HIT error in ',Csys,', event will be skipped'
           endif     
         endif     
         G2R_GET_HIT = i
         if (i.eq.0) volume = g2t_volume_id (Csys,NUMBV)
      end




