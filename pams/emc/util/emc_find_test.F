c:>------------------------------------------------------------------
C:FILE:         emc_find_test.F
C:DESCRIPTION:  EMC Analysis Module, package emc, STAF compliant.
c:AUTHOR:       OGAWA, Akio
c:AUTHOR:       Originaly by W.J. Llope, Rice University
c:AUTHOR:       TAS Version by B. Christie
C:BUGS:         None known.
C:HISTORY:      3Sep97-v1.0-akio- Creation.
c:>------------------------------------------------------------------
      INTEGER*4 FUNCTION EMC_FIND_TEST(
     1       ems_control_h,       ems_control )
      IMPLICIT NONE
#include "emc_def.h"
#include "math_constants.inc"
c:>------------------------------------------------------------------
C:ROUTINE:      INTEGER*4 EMC_FIND_TEST
C:DESCRIPTION:  test find_pos and find_bin
C:ARGUMENTS:    ems_control
C:ARGUMENTS:    
C:RETURN VALUE: STAFCV_OK on successful completion
c:>------------------------------------------------------------------
#include "emc_find_test.inc"
      integer det, mod, ebin, sub, i
      real   phi, eta

      mod=1
      sub=1
      do ebin=1,20
         call find_pos_emc(ems_control,BEMC,mod,ebin,sub,phi,eta)
         write(*,*) 'find_pos/bemc/eta(z+); ', ebin, eta 
      enddo
      mod=61
      sub=1
      do ebin=1,20
         call find_pos_emc(ems_control,BEMC,mod,ebin,sub,phi,eta)
         write(*,*) 'find_pos/bemc/eta(z-); ', ebin, eta 
      enddo

      mod=1
      sub=1
      do ebin=1,12
         call find_pos_emc(ems_control,EEMC,mod,ebin,sub,phi,eta)
         write(*,*) 'find_pos/eemc/eta(z+); ', ebin, eta 
      enddo
      mod=13
      sub=1
      do ebin=1,12
         call find_pos_emc(ems_control,EEMC,mod,ebin,sub,phi,eta)
         write(*,*) 'find_pos/eemc/eta(z-); ', ebin, eta 
      enddo

      ebin=1
      do mod=1,120,10
         do sub=1,2
            call find_pos_emc(ems_control,BEMC,mod,ebin,sub,phi,eta)
            write(*,*) 'find_pos/bemc/phi; ', mod,sub,phi
         enddo
      enddo
      ebin=1
      do mod=1,23,5
         do sub=1,5,2
            call find_pos_emc(ems_control,EEMC,mod,ebin,sub,phi,eta)
            write(*,*) 'find_pos/eemc/phi; ', mod,sub,phi
         enddo
      enddo

      phi=0.0
      do i=1,220
         eta=float(i)/220.0*4.4-2.2
         call find_bin_emc(ems_control,phi,eta,det,mod,ebin,sub)
         write(*,*)'find_bin/eta; ', eta,' /',det,mod,ebin,sub
      enddo

      eta=0.5
      do i=1,120
         phi=(float(i)-0.5)/120.0*C_2PI
         call find_bin_emc(ems_control,phi,eta,det,mod,ebin,sub)
         write(*,*)'find_bin/barrel/phi(z+);',phi,' /',det,mod,ebin,sub
      enddo
      eta=-0.5
      do i=1,120
         phi=(float(i)-0.5)/120.0*C_2PI
         call find_bin_emc(ems_control,phi,eta,det,mod,ebin,sub)
         write(*,*)'find_bin/barrel/phi(z-);',phi,' /',det,mod,ebin,sub
      enddo

      eta=1.5
      do i=1,60
         phi=float(i-1)/60.0*C_2PI
         call find_bin_emc(ems_control,phi,eta,det,mod,ebin,sub)
         write(*,*)'find_bin/endcap/phi(z+);',phi,' /',det,mod,ebin,sub
      enddo
      eta=-1.5
      do i=1,60
         phi=float(i-1)/60.0*C_2PI
         call find_bin_emc(ems_control,phi,eta,det,mod,ebin,sub)
         write(*,*)'find_bin/endcap/phi(z-);',phi,' /',det,mod,ebin,sub
      enddo

      EMC_FIND_TEST=STAFCV_OK
999   RETURN
      END



