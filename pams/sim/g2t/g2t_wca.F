      FUNCTION  G2T_WCA (g2t_track_h,   g2t_track , 
     >                   g2t_emc_hit_h, g2t_emc_hit ) 
      IMPLICIT  NONE
#include "g2t_emc.inc"
#include "genhit.h"
#include "atlsim/gentit.inc"
      INTEGER   G2T_WCA,G2R_GET_HIT,Iprin/0/,Idigi,i
      logical   Local/.true./
*
      G2T_WCA=STAFCV_OK
      if (g2t_emc_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('WCAL','WSCI',Iprin,Idigi)
      if (Iprin.lt.0) return
      Local  = Idigi.ge.2
*
      i=0
      g2t_emc_hit_h.nok = 0
      Do While (G2R_GET_HIT('wca') .eq. 0)
         i=i+1
         g2t_emc_hit_h.nok            = i
         g2t_emc_hit(i).id            = i
         g2t_emc_hit(i).de            = Elos
         g2t_emc_hit(i).track_p       = trac
         g2t_emc_hit(i).volume_id     = volume

         g2t_emc_hit(i).x             = xx(1)
         g2t_emc_hit(i).y             = xx(2)
         g2t_emc_hit(i).z             = xx(3)
*

         g2t_track(trac).hit_wca_p = i
         g2t_track(trac).n_wca_hit = g2t_track(trac).n_wca_hit + 1

      enddo    

      RETURN
      END



