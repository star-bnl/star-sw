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
      character*4     Cset,Cdet
      Integer                   nvl,     numbv
      common /gensit/ Cset,Cdet,nvl(Nmh),numbv(Nmh)
*     - - - - - - - - - - - - - -
*KEND.
      Integer         G2R_GET_HIT,AgHITGET,G2T_VOLUME_ID,i
      real            XYZTr(nmh)
      character*3     Csys
*
         i = AgHITGET (0,XYZTr)
         G2R_GET_HIT = i
         if (i.eq.0) volume = g2t_volume_id (Csys,NUMBV)
      end




