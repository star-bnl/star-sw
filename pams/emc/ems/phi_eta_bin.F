      subroutine phi_eta_bin (eta, phi, eta_bin,phi_bin)
CC:>--------------------------------------------------------------------
CC: AUTHOR:      Pavlinov Aleksei, IHEP => 11-Aug-1998
CC:  Tranzition from phi,eta to phi_bin and eta_bin as output from 
cc:  ems_interface2 
CC: ARGUMENTS: 
CC:           IN:
CC:         phi     - azimuthal angle 
CC:         eta     - polar     angle 
CC:          OUT:
CC:         phi_bin - azimuthal bin   
CC:         phi_bin - polar     bin   
CC:<--------------------------------------------------------------------
      IMPLICIT NONE
#include "math_constants.inc"
      real    phi, eta, sphi
      integer phi_bin, eta_bin, iphi

      if(eta.gt.0) then        ! Positive eta's bins  1 ->  20
        eta_bin = int(20.0*eta) + 1
        if(eta_bin.gt.20) eta_bin = 20
      else                         ! Negative eta's bins -1 -> -20
        eta_bin  = -(int(20.0*abs(eta)) + 1)
        if(eta_bin.lt.-20) eta_bin = -20
      endif

      if(phi .gt. C_PI) then 
         phi = phi - C_2PI  ! See programm eut_extra_rad
      elseif(phi .lt. -C_PI) then
         phi = phi + C_2PI  ! See programm eut_extra_rad
      endif
      if(abs(phi) .gt. C_PI) write(6,*) ' <I> phi ', phi ! Only for testing 

      sphi = C_2PI / 120.   ! Step on phi
      iphi = int((phi+C_PI)/sphi) + 1
      if(iphi.lt.86) then 
        phi_bin = 86-iphi
      else
        phi_bin = 206-iphi
      endif

      if(eta_bin .lt. 0) phi_bin = 241 - phi_bin 
      return
      end

      subroutine tower_theta_phi(eta_bin,phi_bin,  theta, phi)
CC:>--------------------------------------------------------------------
CC:   IN:      as ems_interface2
CC:            phi_eta - polar     bin
CC:            phi_bin - azimuthal bin   
CC:   OUT:
CC             phi     - azimuthal angle of center of tower
CC             theta   - polar     angle of center of tower
CC:<--------------------------------------------------------------------
      IMPLICIT NONE
#include "math_constants.inc"
** some approximation for phi angle
      real phi, eta, theta, sphi
      integer phi_bin, eta_bin, iphi

      if(eta_bin .lt. 0) then
        eta =  0.025 + 0.05*eta_bin 
      else
        eta = -0.025 + 0.05*eta_bin 
      endif
      theta =  2.*atan(exp(-eta))
 
      sphi = C_2PI / 120.   ! Step on phi
      iphi = phi_bin
      if(eta_bin .lt. 0) iphi = 241 - iphi 
      if(iphi.lt.86) then
        phi = sphi*(26-iphi)  - sphi/2.
      else
        phi = sphi * (146-iphi) - sphi/2.
      endif

cc      write(6,1) eta_bin,phi_bin, eta,phi,theta
      return
 1    format(' Tower ieta ',i3,' iphi ', i3,' eta, phi, theta ', 3f7.4)
      end

      subroutine turn_to_tower(phit,tht,rt, x,y,z, xt,yt,zt)
      implicit none
      real phit,rt,tht, x,y,z, xt,yt,zt, st,ct, xo,yo,zo, xw,yw,zw
** 
      st = sin(phit)
      ct = cos(phit)
* Transition to center of new system
      xo = rt*ct
      yo = rt*st
      zo = rt*cos(tht)/sin(tht)

      xw = x - xo
      yw = y - yo
      zw = z - zo
* Turn
      xt =  xw*ct + yw*st
      yt = -xw*st + yw*ct
      zt = zw

      return
      end

