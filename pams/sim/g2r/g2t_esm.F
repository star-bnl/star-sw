      FUNCTION  G2T_ESM (g2t_track_h,   g2t_track , 
     >                   g2t_emc_hit_h, g2t_emc_hit ) 
      IMPLICIT  NONE
#include "g2t_esm.inc"
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
      INTEGER   G2T_ESM,G2R_GET_HIT,ISLFLAG,Iprin/0/,Idigi,i
      logical   Local/.true./
*
      G2T_ESM=STAFCV_OK
      if (g2t_emc_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('CALB','CSDA',Iprin,Idigi)
      if (Iprin.lt.0) return
      Local  = Idigi.ge.2
*
      i=0
      g2t_emc_hit_h.nok = 0
      Do While (G2R_GET_HIT('esm') .eq. 0)
         i=i+1
         g2t_emc_hit_h.nok            = i
         g2t_emc_hit(i).id            = i
         g2t_emc_hit(i).de            = Elos
         g2t_emc_hit(i).track_p       = trac
         g2t_emc_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
*        g2t_emc_hit(i).next_tr_hit_p = g2t_track(trac).hit_esm_p
         g2t_track(trac).hit_esm_p    = i
         g2t_track(trac).n_esm_hit    = g2t_track(trac).n_esm_hit + 1

      enddo    

      RETURN
      END
