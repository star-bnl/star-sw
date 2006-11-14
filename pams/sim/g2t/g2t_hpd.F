* $Id: g2t_hpd.F,v 1.3 2006/11/14 17:22:32 potekhin Exp $
* $Log: g2t_hpd.F,v $
* Revision 1.3  2006/11/14 17:22:32  potekhin
* Removed a gratuitously rich print statement
* which was clogging the log files. Commented out,
* moved to the end of source.
*
* Revision 1.2  2006/09/22 21:40:11  potekhin
* Fixed a typo
*
* Revision 1.1  2006/09/22 21:12:17  potekhin
* Need tp add the HPD
*
* Added CVS tags
*
      FUNCTION  G2T_HPD (g2t_track_h,   g2t_track , 
     >                   g2t_hpd_hit_h, g2t_hpd_hit )
      IMPLICIT  NONE
#include "g2t_hpd.inc"
#include "genhit.h"
      INTEGER   G2T_HPD,G2R_GET_HIT,Iprin/0/,Idigi,i,isys
      logical   Local/.false./
*
      G2T_HPD=STAFCV_OK
      if (g2t_hpd_hit_h.maxlen.le.0) return
*
      i=0
      g2t_hpd_hit_h.nok = 0


      call G2R_GET_SYS ('HPDT','YPLA',Iprin,Idigi)
       if (Iprin.lt.0) go to 99
       Local  = Idigi.ge.2

       Do While (G2R_GET_HIT('hpd') .eq. 0)
         i=i+1
         g2t_hpd_hit_h.nok            = i
         g2t_hpd_hit(i).id            = i

         If (local) then
           g2t_hpd_hit(i).x(1)        = x(1)
           g2t_hpd_hit(i).x(2)        = x(2)
           g2t_hpd_hit(i).x(3)        = x(3)
           g2t_hpd_hit(i).p(1)        = c(1)*p(4)
           g2t_hpd_hit(i).p(2)        = c(2)*p(4)
           g2t_hpd_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_hpd_hit(i).x(1)        = xx(1)!rr*cos(phi)
           g2t_hpd_hit(i).x(2)        = xx(2)!rr*sin(phi)
           g2t_hpd_hit(i).x(3)        = xx(3)
           g2t_hpd_hit(i).p(1)        = p(1)
           g2t_hpd_hit(i).p(2)        = p(2)
           g2t_hpd_hit(i).p(3)        = p(3)
         endif
         g2t_hpd_hit(i).tof           = tof
         g2t_hpd_hit(i).de            = Elos
         g2t_hpd_hit(i).ds            = Step
         g2t_hpd_hit(i).track_p       = trac
         g2t_hpd_hit(i).volume_id     = volume
*
*                  add to track linked list
*
         g2t_hpd_hit(i).next_tr_hit_p = g2t_track(trac).hit_hpd_p
         g2t_track(trac).hit_hpd_p    = i
         g2t_track(trac).n_hpd_hit    = g2t_track(trac).n_hpd_hit + 1
       enddo    
    
 99   RETURN
      END
************************************************************************
*          print *,i,id,trac,next,volume,
*                 x(1),x(2),x(3),xx(1),xx(2),xx(3),c(1),c(2),c(3),
*                 p(1),p(2),p(3),p(4),r,rr,phi,the,eta,tdr,tof,
*                 Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)


