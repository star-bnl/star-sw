      FUNCTION  G2T_FPD (g2t_track_h,   g2t_track , 
     >                   g2t_emc_hit_h, g2t_emc_hit ) 
      IMPLICIT  NONE
#include "g2t_emc.inc"
#include "genhit.h"
      INTEGER   G2T_FPD,G2R_GET_HIT,Iprin/0/,Idigi,i,Isys
      logical   Local/.true./
*
      G2T_FPD=STAFCV_OK
      if (g2t_emc_hit_h.maxlen.le.0) return
*
      i=0
      g2t_emc_hit_h.nok = 0
      do Isys=1,3
         if (Isys.eq.1) call G2R_GET_SYS ('FPDM','FLXF',Iprin,Idigi)
         if (Isys.eq.2) call G2R_GET_SYS ('FPDM','FLGR',Iprin,Idigi)
         if (Isys.eq.3) call G2R_GET_SYS ('FPDM','FPSC',Iprin,Idigi)
         if (Iprin.lt.0) goto 777
         Local  = Idigi.ge.2
*
*         i=0
*         g2t_emc_hit_h.nok = 0
*
         Do While (G2R_GET_HIT('fpd') .eq. 0)
            i=i+1
            g2t_emc_hit_h.nok            = i
            g2t_emc_hit(i).id            = i
            g2t_emc_hit(i).de            = Elos
            g2t_emc_hit(i).track_p       = trac
            g2t_emc_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
*           g2t_emc_hit(i).next_tr_hit_p = g2t_track(trac).hit_fpd_p
            g2t_track(trac).hit_fpd_p    = i
            g2t_track(trac).n_fpd_hit    = g2t_track(trac).n_fpd_hit + 1

         enddo
 777  enddo
      RETURN
      END
