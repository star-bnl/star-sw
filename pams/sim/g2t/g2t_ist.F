* $Id: g2t_ist.F,v 1.5 2014/01/28 14:33:52 jwebb Exp $
* $Log: g2t_ist.F,v $
* Revision 1.5  2014/01/28 14:33:52  jwebb
* IST group prefers local over global coordinates to be pushed into the g2t
* structures.
*
* Revision 1.4  2006/12/17 22:45:10  potekhin
* Commented out gratuitous print statements
* left over from
* debugging.
*
* Revision 1.3  2006/06/26 17:56:21  potekhin
* Added CVS tags
*
      FUNCTION  G2T_IST (g2t_track_h,   g2t_track , 
     >                   g2t_ist_hit_h, g2t_ist_hit )
      IMPLICIT  NONE
#include "g2t_ist.inc"
#include "genhit.h"
      INTEGER   G2T_IST,G2R_GET_HIT,Iprin/0/,Idigi,i,isys
      logical   Local/.true./ !changed by YPWANG Dec. 9, 2013
*
      G2T_IST=STAFCV_OK
      if (g2t_ist_hit_h.maxlen.le.0) return
*
      i=0
      g2t_ist_hit_h.nok = 0


      call G2R_GET_SYS ('ISTB','IBSS',Iprin,Idigi)
       if (Iprin.lt.0) go to 99
!       Local  = Idigi.ge.2
! -- commented out by YPWANG Dec. 9, 2013

       Do While (G2R_GET_HIT('ist') .eq. 0)
*          print *,i,id,trac,next,volume,
*     >            x(1),x(2),x(3),xx(1),xx(2),xx(3),c(1),c(2),c(3),
*     >            p(1),p(2),p(3),p(4),r,rr,phi,the,eta,tdr,tof,
*     >            Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)

         i=i+1
         g2t_ist_hit_h.nok            = i
         g2t_ist_hit(i).id            = i

         If (local) then
           g2t_ist_hit(i).x(1)        = x(1)
           g2t_ist_hit(i).x(2)        = x(2)
           g2t_ist_hit(i).x(3)        = x(3)
           g2t_ist_hit(i).p(1)        = c(1)*p(4)
           g2t_ist_hit(i).p(2)        = c(2)*p(4)
           g2t_ist_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_ist_hit(i).x(1)        = xx(1)!rr*cos(phi)
           g2t_ist_hit(i).x(2)        = xx(2)!rr*sin(phi)
           g2t_ist_hit(i).x(3)        = xx(3)
           g2t_ist_hit(i).p(1)        = p(1)
           g2t_ist_hit(i).p(2)        = p(2)
           g2t_ist_hit(i).p(3)        = p(3)
         endif
         g2t_ist_hit(i).tof           = tof
         g2t_ist_hit(i).de            = Elos
         g2t_ist_hit(i).ds            = Step
         g2t_ist_hit(i).track_p       = trac
         g2t_ist_hit(i).volume_id     = volume
*
*                  add to track linked list 
*
         g2t_ist_hit(i).next_tr_hit_p = g2t_track(trac).hit_ist_p
         g2t_track(trac).hit_ist_p    = i
         g2t_track(trac).n_ist_hit    = g2t_track(trac).n_ist_hit + 1
       enddo    
    
 99   RETURN
      END


