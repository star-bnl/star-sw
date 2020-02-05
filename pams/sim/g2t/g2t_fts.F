      FUNCTION  G2T_FTS (g2t_track_h,   g2t_track , 
     >                   g2t_fts_hit_h, g2t_fts_hit ) 
      IMPLICIT  NONE
#include "g2t_fts.inc"
#include "genhit.h"
      INTEGER   G2T_FTS,G2R_GET_HIT,Iprin/0/,Idigi,i,isys
      logical   Local/.false./

! NOTE: manages several hit collections/subsystems from deprecated geometries... should be
!       revised when we release y2021  
      CHARACTER (len=4), dimension(*), parameter :: 
     _             hitcol  = (/'FTSD', 'FTSD', 'FTSD', 'FTSD', 'FSTM', 'FSTM', 'FSTM' /)
      CHARACTER (len=4), dimension(*), parameter :: 
     _             subsys  = (/'FTSA', 'FSIA', 'FSIB', 'FSIC', 'FTOS', 'FTIS', 'FTUS' /)
      LOGICAL,           dimension(*), parameter :: 
     _             islocal = (/ .false., .true., .true., .true., .false., .false., .false. /)

*
      G2T_FTS=STAFCV_OK
      if (g2t_fts_hit_h.maxlen.le.0) return
*
      i=0
      g2t_fts_hit_h.nok = 0
       DO isys=1, size(subsys)

       CALL g2r_get_sys( hitcol(isys), subsys(isys), Iprin, Idigi )

       if (Iprin.lt.0) goto 123 ! negative print flag indicates no hits

*      Local  = Idigi.ge.2
*      local  = .false. ! Ensure global coordinates

       local  = islocal( isys )


       Do While (G2R_GET_HIT('fts') .eq. 0)
*         print *,i,id,trac,next,volume,
*    >            x(1),x(2),x(3),xx(1),xx(2),xx(3),c(1),c(2),c(3),
*    >            p(1),p(2),p(3),p(4),r,rr,phi,the,eta,tdr,tof,
*    >            Slen,Step,Etot,Lgam,Lpto,Elos,User,Unkn(3)

         i=i+1
         g2t_fts_hit_h.nok            = i
         g2t_fts_hit(i).id            = i

         If (local) then
           g2t_fts_hit(i).x(1)        = x(1)
           g2t_fts_hit(i).x(2)        = x(2)
           g2t_fts_hit(i).x(3)        = x(3)
           g2t_fts_hit(i).p(1)        = c(1)*p(4)
           g2t_fts_hit(i).p(2)        = c(2)*p(4)
           g2t_fts_hit(i).p(3)        = c(3)*p(4)
         else
           g2t_fts_hit(i).x(1)        = xx(1)!rr*cos(phi)
           g2t_fts_hit(i).x(2)        = xx(2)!rr*sin(phi)
           g2t_fts_hit(i).x(3)        = xx(3)
           g2t_fts_hit(i).p(1)        = p(1)
           g2t_fts_hit(i).p(2)        = p(2)
           g2t_fts_hit(i).p(3)        = p(3)
         endif
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
         g2t_track(trac).n_fts_hit    = g2t_track(trac).n_fts_hit + 1
       enddo    

 123  continue
      ENDDO
 99   RETURN    

      END


