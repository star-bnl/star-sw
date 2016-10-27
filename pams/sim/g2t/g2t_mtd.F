      FUNCTION  G2T_MTD (g2t_track_h,   g2t_track , 
     >                   g2t_mtd_hit_h, g2t_mtd_hit ) 
      IMPLICIT  NONE
#include "g2t_mtd.inc"
#include "genhit.h"
      INTEGER   G2T_MTD,G2R_GET_HIT,Iprin/0/,Idigi,i, Isys
      logical   Local/.true./
*
      G2T_MTD=STAFCV_OK
      if (g2t_mtd_hit_h.maxlen.le.0) return
*
      i=0
      g2t_mtd_hit_h.nok = 0
*
      DO isys=1,3

IF (isys==1) call G2R_GET_SYS ('MUTD','MIGG',Iprin,Idigi)
IF (isys==2) call G2R_GET_SYS ('MUTD','MTTT',Iprin,Idigi)
if (isys==3) call G2R_GET_SYS ('MUTD','MTTF',Iprin,Idigi)

      if (Iprin.lt.0) goto 99
      Local  = Idigi.ge.2
*
      Do While (G2R_GET_HIT('mtd') .eq. 0)
         i=i+1
         g2t_mtd_hit_h.nok            = i
         g2t_mtd_hit(i).id            = i
*
* always fill the local position of hit, regardless of the value of "Idigi"
*
         g2t_mtd_hit(i).x(1)        = x(1)
         g2t_mtd_hit(i).x(2)        = x(2)
         g2t_mtd_hit(i).x(3)        = x(3)

         g2t_mtd_hit(i).xglobal(1)  = xx(1)
         g2t_mtd_hit(i).xglobal(2)  = xx(2)
         g2t_mtd_hit(i).xglobal(3)  = xx(3)

         If (local) then
           g2t_mtd_hit(i).p(1)        = c(1)*p(4)
           g2t_mtd_hit(i).p(2)        = c(2)*p(4)
           g2t_mtd_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_mtd_hit(i).p(1)        = p(1)
           g2t_mtd_hit(i).p(2)        = p(2)
           g2t_mtd_hit(i).p(3)        = p(3)
         endif
         g2t_mtd_hit(i).tof           = tof
         g2t_mtd_hit(i).de            = Elos
         g2t_mtd_hit(i).ds            = Step
         g2t_mtd_hit(i).s_track       = Slen
         g2t_mtd_hit(i).track_p       = trac
         g2t_mtd_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
         g2t_mtd_hit(i).next_tr_hit_p = g2t_track(trac).hit_mtd_p
         g2t_track(trac).hit_mtd_p    = i
         g2t_track(trac).n_mtd_hit    = g2t_track(trac).n_mtd_hit + 1

      enddo    
 99   enddo

      RETURN
      END
