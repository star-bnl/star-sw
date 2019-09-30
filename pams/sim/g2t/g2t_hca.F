      FUNCTION  G2T_HCA (g2t_track_h,   g2t_track , 
     >                   g2t_hca_hit_h, g2t_hca_hit ) 
      IMPLICIT  NONE
#include "g2t_hca.inc"
#include "genhit.h"
#include "atlsim/gentit.inc"
      INTEGER   G2T_HCA,G2R_GET_HIT,Iprin/0/,Idigi,i,j,k
*      Real d1,d2,d3,d4,sum,diff
      logical   Local/.true./
      CHARACTER*4,PARAMETER :: HSCA='HSCA', HSCB='HSCB'
      CHARACTER*4,PARAMETER :: HSCC='HSCC', HSCD='HSCD'
*
      G2T_HCA=STAFCV_OK
      if (g2t_hca_hit_h.maxlen.le.0) return
*
      call G2R_GET_SYS ('HCAL','HSCI',Iprin,Idigi)
      if (Iprin.lt.0) return
      Local  = Idigi.ge.2
*
      i=0
      g2t_hca_hit_h.nok = 0
      Do While (G2R_GET_HIT('hca') .eq. 0)
         i=i+1
         g2t_hca_hit_h.nok            = i
         g2t_hca_hit(i).id            = i
         g2t_hca_hit(i).de            = Elos
         g2t_hca_hit(i).track_p       = trac
         g2t_hca_hit(i).volume_id     = volume

         g2t_hca_hit(i).x             = xx(1)
         g2t_hca_hit(i).y             = xx(2)
         g2t_hca_hit(i).z             = xx(3)
*
         g2t_track(trac).hit_hca_p = i
         g2t_track(trac).n_hca_hit = g2t_track(trac).n_hca_hit + 1

         j=0
         DO k=1,15
           IF (chit(k)==HSCA) {g2t_hca_hit(i).deA = hits(k); j=j+1; continue;}  
           IF (chit(k)==HSCB) {g2t_hca_hit(i).deB = hits(k); j=j+1; continue;}  
           IF (chit(k)==HSCC) {g2t_hca_hit(i).deC = hits(k); j=j+1; continue;}  
           IF (chit(k)==HSCD) {g2t_hca_hit(i).deD = hits(k); j=j+1; continue;}  
           if (j.eq.4) break;
         ENDDO            

*         d1=g2t_hca_hit(i).deA
*         d2=g2t_hca_hit(i).deB
*         d3=g2t_hca_hit(i).deC
*         d4=g2t_hca_hit(i).deD
*         sum=d1+d2+d3+d4
*         diff=sum-g2t_hca_hit(i).de
*         write(*,*) 'BBB',g2t_hca_hit(i).de,d1,d2,d3,d4,sum,diff
         
      enddo    

      RETURN
      END
