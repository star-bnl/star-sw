      FUNCTION  G2T_EPD (g2t_track_h,   g2t_track , 
     >                   g2t_epd_hit_h, g2t_epd_hit ) 
      IMPLICIT  NONE
#include "g2t_epd.inc"
#include "genhit.h"
      INTEGER   G2T_EPD,G2R_GET_HIT,Iprin/0/,Idigi,i
      logical   Local/.false./ ! true=loca, false=global coordinates
*
      G2T_EPD=STAFCV_OK
      if (g2t_epd_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('EPDM','BPOL',Iprin,Idigi)
      if (Iprin.lt.0) return
c$$$  Local  = Idigi.ge.2
*
      i=0
      g2t_epd_hit_h.nok = 0
      Do While (G2R_GET_HIT('epd') .eq. 0)
         i=i+1
         g2t_epd_hit_h.nok            = i
         g2t_epd_hit(i).id            = i
         g2t_epd_hit(i).de            = Elos

         g2t_epd_hit(i).track_p       = trac
         g2t_epd_hit(i).volume_id     = volume
         g2t_epd_hit(i).tof = tof

         If (local) then
           g2t_epd_hit(i).x(1)        = x(1)
           g2t_epd_hit(i).x(2)        = x(2)
           g2t_epd_hit(i).x(3)        = x(3)
           g2t_epd_hit(i).p(1)        = c(1)*p(4)
           g2t_epd_hit(i).p(2)        = c(2)*p(4)
           g2t_epd_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_epd_hit(i).x(1)        = xx(1)!rr*cos(phi)
           g2t_epd_hit(i).x(2)        = xx(2)!rr*sin(phi)
           g2t_epd_hit(i).x(3)        = xx(3)
           g2t_epd_hit(i).p(1)        = p(1)
           g2t_epd_hit(i).p(2)        = p(2)
           g2t_epd_hit(i).p(3)        = p(3)
         endif

*
      enddo    

      RETURN
      END

