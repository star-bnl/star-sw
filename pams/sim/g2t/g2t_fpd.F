      FUNCTION  G2T_FPD (g2t_track_h,   g2t_track , 
     >                   g2t_emc_hit_h, g2t_emc_hit ) 
      IMPLICIT  NONE
#include "g2t_emc.inc"
#include "genhit.h"
#include "atlsim/gentit.inc"
      INTEGER   G2T_FPD,G2R_GET_HIT,Iprin/0/,Idigi,i,Isys
      logical   Local/.true./

      """ System number """
      INTEGER,PARAMETER :: kFLXF=1, kFLGR=2, kFPSC=3
      """ Volume names """
      CHARACTER*4,PARAMETER :: FLGR = 'FLGR', FLXF = 'FLXF', FPSC='FPSC'
      """ Loopers """
      Integer :: khit

*
      G2T_FPD=STAFCV_OK
      if (g2t_emc_hit_h.maxlen.le.0) return
*
      i=0
      g2t_emc_hit_h.nok = 0
      do Isys=1,3

         if (Isys.eq.kFLXF) call G2R_GET_SYS ('FPDM',FLXF,Iprin,Idigi)
         if (Isys.eq.kFLGR) call G2R_GET_SYS ('FPDM',FLGR,Iprin,Idigi)
         if (Isys.eq.kFPSC) call G2R_GET_SYS ('FPDM',FPSC,Iprin,Idigi)

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

            IF      (isys==kFLGR) THEN
                DO khit=1,15
                IF (chit(khit)==FLGR) { G2T_EMC_HIT(i).de = hits(khit); break; } 
                ENDDO 
            ELSE IF (isys==kFLXF) THEN
                DO khit=1,15
                IF (chit(khit)==FLXF) { G2T_EMC_HIT(i).de = hits(khit); break; }
                ENDDO 
            ELSE
            ENDIF

         enddo
 777  enddo
      RETURN
      END
