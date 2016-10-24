      FUNCTION  G2T_HCA (g2t_track_h,   g2t_track , 
     >                   g2t_emc_hit_h, g2t_emc_hit ) 
      IMPLICIT  NONE
#include "g2t_emc.inc"
#include "genhit.h"
#include "atlsim/gentit.inc"
+cde,gcunit.

      INTEGER   G2T_HCA,G2R_GET_HIT,Iprin/0/,Idigi,i,isys, khit
      logical   Local/.true./
*
      CHARACTER *4 myhit/'HCAL'/
*
      G2T_HCA=STAFCV_OK
      if (g2t_emc_hit_h.maxlen.le.0) return

      i=0
      g2t_emc_hit_h.nok = 0

      DO isys=1,7 """ Loop over all sensitive fibers """ 

         " STAR implementation HcalGeo, HcalGeo1 "
         IF (isys==1) { call G2R_GET_SYS ('HCAL','HCEL',Iprin,Idigi) }
         IF (isys==2) { call G2R_GET_SYS ('HCAL','HCES',Iprin,Idigi) }
         IF (isys==3) { call G2R_GET_SYS ('HCAL','FPSC',Iprin,Idigi) }

         " Test beam implementation, HcalGeoF "
         IF (isys==4) { call G2R_GET_SYS ('HCAL','BBCF',Iprin,Idigi) } ! Front scint paddle 
         IF (isys==5) { call G2R_GET_SYS ('HCAL','BBCB',Iprin,Idigi) } ! Back scint paddle 
         if (isys==6) { call G2R_GET_SYS ('HCAL','LEDG',Iprin,Idigi) } ! Lead glass

         "For Preshower HcalGeo"
         if (isys==7) { call G2R_GET_SYS ('HCAL','HSTP',Iprin,Idigi) } !  preshower 

         if (Iprin.lt.0) { 
            cycle " Next iteration of the loop "
         }
         Local  = Idigi.ge.2

         Do While (G2R_GET_HIT('hca') .eq. 0)

            i=i+1
            g2t_emc_hit_h.nok            = i
            g2t_emc_hit(i).id            = i
            g2t_emc_hit(i).de            = 0   ! Use attenuated hcal value
            g2t_emc_hit(i).track_p       = trac
            g2t_emc_hit(i).volume_id     = volume
            g2t_emc_hit(i).x             = xx(1) 
            g2t_emc_hit(i).y             = xx(2)
            g2t_emc_hit(i).z             = xx(3)

            if isys .lt. 3 { """Attenuated hit for HCA"""
              do khit=1,15
                 IF (chit(khit)=myhit) { 
                 g2t_emc_hit(i).de = hits(khit); 
                 break; 
                 }
            enddo
            }

            if isys .eq. 3 { """Unattenuated hit for preshower"""
            g2t_emc_hit(i).de = elos
            }

            if isys .eq. 4 { """Scintillator paddle """
            g2t_emc_hit(i).de = elos
            }

            if isys .eq. 5 { """Scintillator paddle """
            g2t_emc_hit(i).de = elos
            }

            if isys .eq. 6 { """Unattenuated hit for pbglass"""
            g2t_emc_hit(i).de = elos
            }

            if isys .eq. 7 { """2014 preshower"""
            g2t_emc_hit(i).de = hits(khit); 
            }


*        <W> i,trac,volume,hits(khit);( 'g2t_hca: ', I4, I4, I8, F8.4);
*
*                  add to track linked list 
*
*        g2t_emc_hit(i).next_tr_hit_p = g2t_track(trac).hit_emc_p
            g2t_track(trac).hit_hca_p    = i
            g2t_track(trac).n_hca_hit    = g2t_track(trac).n_hca_hit + 1

         enddo """Loop over hits"""

      enddo """Loop over fiber types"""

      RETURN
      END
