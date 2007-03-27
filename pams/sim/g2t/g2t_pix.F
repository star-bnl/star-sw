      FUNCTION  G2T_PIX (g2t_track_h,   g2t_track , 
     >                   g2t_pix_hit_h, g2t_pix_hit ) 
      IMPLICIT  NONE
#include "g2t_pix.inc"
#include "genhit.h"
      INTEGER   G2T_PIX,G2R_GET_HIT,Iprin/0/,Idigi,i,isys
      logical   Local/.false./
*
      G2T_PIX=STAFCV_OK
      if (g2t_pix_hit_h.maxlen.le.0) return
*
      i=0
      g2t_pix_hit_h.nok = 0


      call G2R_GET_SYS ('PIXL','PLAC',Iprin,Idigi)
       if (Iprin.lt.0) go to 99
       Local  = Idigi.ge.2

       Do While (G2R_GET_HIT('pix') .eq. 0)
*          print *,i,id,trac,next,volume,
*     >            x(1),x(2),x(3),xx(1),xx(2),xx(3),c(1),c(2),c(3),
*     >            p(1),p(2),p(3),p(4),r,rr,phi,the,eta,tdr,tof,
*     >            Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)

         i=i+1
         g2t_pix_hit_h.nok            = i
         g2t_pix_hit(i).id            = i

         If (local) then
           g2t_pix_hit(i).x(1)        = x(1)
           g2t_pix_hit(i).x(2)        = x(2)
           g2t_pix_hit(i).x(3)        = x(3)
           g2t_pix_hit(i).p(1)        = c(1)*p(4)
           g2t_pix_hit(i).p(2)        = c(2)*p(4)
           g2t_pix_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_pix_hit(i).x(1)        = xx(1)!rr*cos(phi)
           g2t_pix_hit(i).x(2)        = xx(2)!rr*sin(phi)
           g2t_pix_hit(i).x(3)        = xx(3)
           g2t_pix_hit(i).p(1)        = p(1)
           g2t_pix_hit(i).p(2)        = p(2)
           g2t_pix_hit(i).p(3)        = p(3)
         endif
         g2t_pix_hit(i).tof           = tof
         g2t_pix_hit(i).de            = Elos
         g2t_pix_hit(i).ds            = Step
         g2t_pix_hit(i).track_p       = trac
         g2t_pix_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
         g2t_pix_hit(i).next_tr_hit_p = g2t_track(trac).hit_pix_p
         g2t_track(trac).hit_pix_p    = i
         g2t_track(trac).n_pix_hit    = g2t_track(trac).n_pix_hit + 1
       enddo    
    
 99   RETURN
      END


