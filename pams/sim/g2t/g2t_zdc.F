      FUNCTION  G2T_ZDC (g2t_track_h,   g2t_track , 
     >                   g2t_emc_hit_h, g2t_emc_hit ) 
      IMPLICIT  NONE
#include "g2t_zdc.inc"
#include "genhit.h"
      INTEGER   G2T_ZDC,G2R_GET_HIT,Iprin/0/,Idigi,i
      logical   Local/.true./
*
      G2T_ZDC=STAFCV_OK
      if (g2t_emc_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('ZCAL','QSCI',Iprin,Idigi)
      if (Iprin.lt.0) return
      Local  = Idigi.ge.2
*
      i=0
      g2t_emc_hit_h.nok = 0
      Do While (G2R_GET_HIT('zdc') .eq. 0)
         i=i+1
         g2t_emc_hit_h.nok            = i
         g2t_emc_hit(i).id            = i
         g2t_emc_hit(i).de            = Elos
         g2t_emc_hit(i).track_p       = trac
         g2t_emc_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
*        g2t_emc_hit(i).next_tr_hit_p = g2t_track(trac).hit_zdc_p
*        g2t_track(trac).hit_zdc_p    = i
*        g2t_track(trac).n_zdc_hit    = g2t_track(trac).n_zdc_hit + 1

      enddo    

      RETURN
      END
