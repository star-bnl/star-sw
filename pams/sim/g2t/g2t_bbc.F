      FUNCTION  G2T_BBC (g2t_track_h,   g2t_track , 
     >                   g2t_ctf_hit_h, g2t_ctf_hit ) 
      IMPLICIT  NONE
#include "g2t_bbc.inc"
*    - - - - - - - - - 
      Integer         Lnmax
      Parameter       (Lnmax=50)
      Integer         id,trac,next,volume,IQQ(Lnmax)
      Real            x,   xx,   c,   p   ,r,rr,phi,the,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn,QQ(Lnmax)
      common /genhit/ id,trac,next,volume,
     >                x(3),xx(3),c(3),p(4),r,rr,phi,the,eta,tdr,tof,
     >                Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)
      Equivalence     (QQ(1),IQQ(1),id)
*    - - - - - - - - -
      INTEGER   G2T_BBC,G2R_GET_HIT,Iprin/0/,Idigi,i
      logical   Local/.true./
*
      G2T_BBC=STAFCV_OK
      if (g2t_ctf_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('BBCM','BPOL',Iprin,Idigi)
      if (Iprin.lt.0) return
      Local  = Idigi.ge.2
*
      i=0
      g2t_ctf_hit_h.nok = 0
      Do While (G2R_GET_HIT('bbc') .eq. 0)
         i=i+1
         g2t_ctf_hit_h.nok            = i
         g2t_ctf_hit(i).id            = i
         g2t_ctf_hit(i).de            = Elos
         g2t_ctf_hit(i).track_p       = trac
         g2t_ctf_hit(i).volume_id     = volume
         g2t_ctf_hit(i).tof = tof
*
      enddo    

      RETURN
      END



