      program main_UpUp_TauTau_NLO
      implicit none!
!#include "s2n_declare.h"

      integer*4  l1,l2,l3,l4,iz
      integer*4 iqed,iew,iborn,gfscheme,ifgg
      real*8     sum,sig(2),har,hai,nc,fc,tlmu2,cosf
      real*8     s,t,u,betaf,sigma,mta,conhc,pi
      complex*16 UpUp_TauTau_HA_NLO,ha
      external   UpUp_TauTau_HA_NLO

**********************************************************
*   iqed     = 0   without QED corrections
*              1   with QED corrections
*              2   ISR
*              3   IFI
*              4   FSR
*              5   [IFI+FSR]
*
*   iew      = 0   without Weak corrections
*              1   with Weak corrections
*
*   iborn    = 0   Born + 1-loop
*              1   Born
*
*   gfscheme = 0   alpha(0) calculation scheme
*              1   GF
*              2   GF prime
*                
*   ifgg    = -1   ffgg = 0
*              0          zoro
*              1          [zoro+coeff*ff_gg]
*              2          [zoro/(1d0 - coeff*ff_gg)]
*
********************************************************

*///////////////////////
*-- Flag settings 
      iqed     = 0
      iew      = 1
      iborn    = 0
      gfscheme = 0
      ifgg     = 1
*///////////////////////
      nc = 1d0
      fc = 3d0
*///////////////////////
* 'tlmu2' is IR singularity regulator
*(photon mass sqared). Affects NLO
* amlitude in case QED is on(iqed=1)
      tlmu2 = 1d-5
      call flagset(iqed,iew,iborn,gfscheme,ifgg,nc,fc,tlmu2)
*///////////////////////

      call paraget(mta,conhc,pi)

*///////////////////////
      call PrintConsts(0)
      
*///////////////////////
* Mandelstam  variables
      s = 4900.d0
*- banchmark values for sigma
      cosf = 0.896067987d0 ! sigma = 0.314137724
      cosf = 0.515891036d0 ! sigma = 0.563049574
      cosf = 0.103059961d0 ! sigma = 1.37836685
      cosf = 0.000969684d0 ! sigma = 1.66750761
      cosf =-0.255370913d0 ! sigma = 2.54648731
*-
      betaf = sqrt(1d0-4d0*mta**2/s)
      t = mta**2 - s/2*(1d0-betaf*cosf)
      u = mta**2 - s/2*(1d0+betaf*cosf)
*//////////////////////
      
      do iz = 0,1
         sum = 0d0
         do l1 = 1,2
            do l2 = 1,2
               do l3 = 1,2
                do l4 = 1,2
                 call UpUp(l1,l2,l3,l4,s,t,u,iz,har,hai);
                 sum = sum +har*har+hai*hai! ha*dconjg(ha)
                enddo
               enddo
            enddo
         enddo
         sig(iz+1) = sum
      enddo
      sigma = conhc*                  ! to pbarn
     &         nc/fc*1d0/2/s * 
     &         1d0/4 *                ! spin sum
     &         (sig(2) - sig(1)) *    ! |Amp|^2 - linearized
     &         betaf/16/pi            ! phase_space/dcos{theta}
      print*,''
      print*, "d_sigma/d_cos{theta} = ",sigma
      print*,''

      stop
      end
