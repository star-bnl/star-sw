      FUNCTION  G2T_MWC (g2t_track_h,   g2t_track , 
     >                   g2t_mwc_hit_h, g2t_mwc_hit ) 
      IMPLICIT  NONE
#include "g2t_mwc.inc"
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
      INTEGER   G2T_MWC,G2R_GET_HIT,ISLFLAG,Iprin/0/,Idigi,i
      logical   Local/.false./
*
      G2T_MWC=STAFCV_OK
      if (g2t_mwc_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('TPCE','TMSE',Iprin,Idigi)
***     Local  = Idigi.ge.2  *** for the moment
*
      i=0
      g2t_mwc_hit_h.nok = 0
      Do While (G2R_GET_HIT('mwc') .eq. 0)
         i=i+1
         g2t_mwc_hit_h.nok            = i
         g2t_mwc_hit(i).id            = i
         If (local) then
           g2t_mwc_hit(i).x(1)        = x(1)
           g2t_mwc_hit(i).x(2)        = x(2)
           g2t_mwc_hit(i).x(3)        = x(3)
           g2t_mwc_hit(i).p(1)        = c(1)*p(4)
           g2t_mwc_hit(i).p(2)        = c(2)*p(4)
           g2t_mwc_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_mwc_hit(i).x(1)        = xx(1)
           g2t_mwc_hit(i).x(2)        = xx(2)
           g2t_mwc_hit(i).x(3)        = xx(3)
           g2t_mwc_hit(i).p(1)        = p(1)
           g2t_mwc_hit(i).p(2)        = p(2)
           g2t_mwc_hit(i).p(3)        = p(3)
         endif
         g2t_mwc_hit(i).tof           = tof
         g2t_mwc_hit(i).de            = Elos
         g2t_mwc_hit(i).ds            = Step
         g2t_mwc_hit(i).s_track       = Slen
         g2t_mwc_hit(i).track_p       = trac
         g2t_mwc_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
         g2t_mwc_hit(i).next_tr_hit_p = g2t_track(trac).hit_mwc_p
         g2t_track(trac).hit_mwc_p    = i
         g2t_track(trac).n_mwc_hit    = g2t_track(trac).n_mwc_hit + 1

      enddo    

      RETURN
      END
