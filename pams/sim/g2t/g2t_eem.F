      FUNCTION  G2T_EEM (g2t_track_h,   g2t_track , 
     >                   g2t_emc_hit_h, g2t_emc_hit ) 
      IMPLICIT  NONE
#include "g2t_eem.inc"
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
      INTEGER   G2T_EEM,G2R_GET_HIT,Iprin/0/,Idigi,Isys,i
      logical   Local/.true./
*
      G2T_EEM=STAFCV_OK
      if (g2t_emc_hit_h.maxlen.le.0) return
*
      i=0
      g2t_emc_hit_h.nok = 0
*
      Do Isys=1,3
        if (isys.eq.1) call G2R_GET_SYS ('ECAL','ESCI',Iprin,Idigi)
        if (isys.eq.2) call G2R_GET_SYS ('ECAL','ELGR',Iprin,Idigi)
        if (isys.eq.3) call G2R_GET_SYS ('ECAL','EPCT',Iprin,Idigi)
        if (Iprin.lt.0) go to 99
        Local  = Idigi.ge.2
*
        Do While (G2R_GET_HIT('eem') .eq. 0)
          i=i+1
          if (i.gt.g2t_emc_hit_h.maxlen) return

          g2t_emc_hit_h.nok            = i
          g2t_emc_hit(i).id            = i
          g2t_emc_hit(i).de            = Elos
          g2t_emc_hit(i).track_p       = trac
          g2t_emc_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
          if (isys.eq.1) then
*           g2t_emc_hit(i).next_tr_hit_p = g2t_track(trac).hit_eem_p
            g2t_track(trac).hit_eem_p    = i
            g2t_track(trac).n_eem_hit    = g2t_track(trac).n_eem_hit + 1
          endif
        print *,' isys, i = ',isys,i
        enddo    
 99   enddo
      RETURN
      END
