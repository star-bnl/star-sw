      FUNCTION  G2T_RCH (g2t_track_h,   g2t_track , 
     >                   g2t_rch_hit_h, g2t_rch_hit ) 
      IMPLICIT  NONE
#include "g2t_rch.inc"
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
      INTEGER   G2T_RCH,G2R_GET_HIT,Iprin/0/,Idigi,Isys/0/,i
      logical   Local/.true./
*
      G2T_RCH=STAFCV_OK

      if (g2t_rch_hit_h.maxlen.le.0) return
*
      i=0
      g2t_rch_hit_h.nok = 0

*      open(unit=21,file='a.txt',status='unknown')
      do Isys=1,4

       if (isys.eq.1) call G2R_GET_SYS ('RICH','RGAP',Iprin,Idigi)
       if (isys.eq.2) call G2R_GET_SYS ('RICH','RCSI',Iprin,Idigi)
       if (isys.eq.3) call G2R_GET_SYS ('RICH','QUAR',Iprin,Idigi)
       if (isys.eq.4) call G2R_GET_SYS ('RICH','FREO',Iprin,Idigi)
       if (Iprin.lt.0) goto 99
       Local  = Idigi.ge.2
*
       Do While (G2R_GET_HIT('rch') .eq. 0)
         i=i+1
         if (i.gt.g2t_rch_hit_h.maxlen) return
         g2t_rch_hit_h.nok            = i
         g2t_rch_hit(i).id            = i

         If (local) then
           g2t_rch_hit(i).x(1)        = x(1)
           g2t_rch_hit(i).x(2)        = x(2)
           g2t_rch_hit(i).x(3)        = x(3)
           g2t_rch_hit(i).p(1)        = c(1)*p(4)
           g2t_rch_hit(i).p(2)        = c(2)*p(4)
           g2t_rch_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_rch_hit(i).x(1)        = xx(1)
           g2t_rch_hit(i).x(2)        = xx(2)
           g2t_rch_hit(i).x(3)        = xx(3)
           g2t_rch_hit(i).p(1)        = p(1)
           g2t_rch_hit(i).p(2)        = p(2)
           g2t_rch_hit(i).p(3)        = p(3)
         endif
         g2t_rch_hit(i).tof           = tof
         g2t_rch_hit(i).de            = Elos
         g2t_rch_hit(i).ds            = Step
         g2t_rch_hit(i).track_p       = trac
         g2t_rch_hit(i).volume_id     = volume

*         write(21,*) x(1),x(2),x(3),xx(1),xx(2),xx(3),
*     >   p(1),p(2),p(3),tof,Elos,Step,
*     >   (volume+Isys*1000),trac,i,g2t_track(trac).ge_pid,
*     >   g2t_track(trac).ptot,g2t_track(trac).n_tpc_hit,
*     >   Lgam, Lpto, User, Etot
*
*                  add to track linked list 
*
*         g2t_rch_hit(i).next_tr_hit_p = g2t_track(trac).hit_rch_p
*         g2t_track(trac).hit_rch_p    = i
*         g2t_track(trac).n_rch_hit    = g2t_track(trac).n_rch_hit + 1

       enddo    
 99   enddo
*      close (21)
      RETURN
      END
