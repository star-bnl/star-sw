      FUNCTION  G2T_ETR (g2t_track_h,   g2t_track , 
     >                   g2t_etr_hit_h, g2t_etr_hit ) 
      IMPLICIT  NONE
#include "g2t_etr.inc"
#include "geant321/gckine.inc"
#include "genhit.h"
      INTEGER         G2T_ETR,G2R_GET_HIT,Iprin,Idigi,Ip0,N,ish,npar,n0,i,jtra
      REAL            G2T_FIELD,par(50),U(100),Pt,Vr,dphi,dtr,FieldZ
      logical         Local
      Character       Cp*20
      Equivalence     (NAPART,cp)
*
      G2T_ETR=STAFCV_OK
      if (g2t_etr_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('EIDD','TABD',Iprin,Idigi)
      if (Iprin.lt.0) return
      FieldZ = G2T_FIELD (0.)
      Local  = Idigi.ge.2
*
      i    = 0
      g2t_etr_hit_h.nok = 0
*
      Do While (G2R_GET_HIT('etr') .eq. 0)
            i=i+1
            g2t_etr_hit_h.nok            = i
            g2t_etr_hit(i).id            = i

            ! Copy observables into the hit
            If (local) then
              g2t_etr_hit(i).x(1)        = x(1)
              g2t_etr_hit(i).x(2)        = x(2)
              g2t_etr_hit(i).x(3)        = x(3)
              g2t_etr_hit(i).p(1)        = c(1)*p(4)
              g2t_etr_hit(i).p(2)        = c(2)*p(4)
              g2t_etr_hit(i).p(3)        = c(3)*p(4)
            else
              g2t_etr_hit(i).x(1)        = xx(1)
              g2t_etr_hit(i).x(2)        = xx(2)
              g2t_etr_hit(i).x(3)        = xx(3)
              g2t_etr_hit(i).p(1)        = p(1)
              g2t_etr_hit(i).p(2)        = p(2)
              g2t_etr_hit(i).p(3)        = p(3)
            endif
            g2t_etr_hit(i).tof           = tof
            g2t_etr_hit(i).de            = Elos
            g2t_etr_hit(i).ds            = Step
            g2t_etr_hit(i).track_p       = trac
            g2t_etr_hit(i).volume_id     = volume
            g2t_etr_hit(i).lgam          = lgam
            g2t_etr_hit(i).length        = Slen

            ! Add the hit to the linked list of hits on the trakc
            g2t_etr_hit(i).next_tr_hit_p = g2t_track(trac).hit_etr_p
            g2t_track(trac).hit_etr_p    = i
            g2t_track(trac).n_etr_hit    = g2t_track(trac).n_etr_hit + 1

 90   enddo
      RETURN
      END

* $Log: g2t_etr.F,v $
* Revision 1.1  2012/01/24 03:32:30  perev
* Add Etr
*
