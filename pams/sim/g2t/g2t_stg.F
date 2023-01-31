      FUNCTION  G2T_STG (g2t_track_h,   g2t_track , 
     >                   g2t_fts_hit_h, g2t_fts_hit ) 
      IMPLICIT  NONE
#include "g2t_fts.inc"
#include "genhit.h"
      INTEGER   G2T_STG,G2R_GET_HIT,Iprin/0/,Idigi,i,isys
      logical,parameter :: Local = .false.


      G2T_STG=STAFCV_OK
      if (g2t_fts_hit_h.maxlen.le.0) return
*
      i=0
      g2t_fts_hit_h.nok = 0


      "Setup readout for the GAS volumes STG*"

      Do Isys=1,3
           if (isys.eq.1) CALL g2r_get_sys('STGM','STGP',Iprin,Idigi)
           if (isys.eq.2) CALL g2r_get_sys('STGM','STGL',Iprin,Idigi)
           if (isys.eq.3) CALL g2r_get_sys('STGM','STGS',Iprin,Idigi)
           IF (Iprin.lt.0) go to 99

           

           Do While (G2R_GET_HIT('stg') .eq. 0)

                 i=i+1
                 g2t_fts_hit_h.nok            = i
                 g2t_fts_hit(i).id            = i

                 g2t_fts_hit(i).x(1)        = xx(1)!rr*cos(phi)
                 g2t_fts_hit(i).x(2)        = xx(2)!rr*sin(phi)
                 g2t_fts_hit(i).x(3)        = xx(3)
                 g2t_fts_hit(i).p(1)        = p(1)
                 g2t_fts_hit(i).p(2)        = p(2)
                 g2t_fts_hit(i).p(3)        = p(3)

                 g2t_fts_hit(i).tof           = tof
                 g2t_fts_hit(i).de            = Elos
                 g2t_fts_hit(i).ds            = Step
                 g2t_fts_hit(i).track_p       = trac
                 g2t_fts_hit(i).volume_id     = volume
                 *
                 *                  add to track linked list 
                 *
                 g2t_fts_hit(i).next_tr_hit_p = g2t_track(trac).hit_fts_p
                 g2t_track(trac).hit_fts_p    = i
                 g2t_track(trac).n_stg_hit    = g2t_track(trac).n_stg_hit + 1

           enddo * G2R_GET_HIT
      enddo * Isys
    
 99   RETURN
      END


