CC:>--------------------------------------------------------------------
CC: HISTORY: Created by Pavlinov Aleksei 10-aug-1998
CC:<--------------------------------------------------------------------
      subroutine prepare_track(tpt_track, track, qq, p)
CC:>--------------------------------------------------------------------
CC: Routine:       prepare_track 
CC: DESCRIPTION:   Service routine. Prepare information 
CC:                for subroutine eut_extra_rad.
CC: AUTHOR:     PAI - Pavlinov Aleksei, IHEP, Protvino
CC: ARGUMENTS:
CC:        IN: tpt_track  - row from tpt_track table;
CC:       OUT: track(1)   - x-coord. of circle (xc);
CC:            track(2)   - y-coord. of circle (yc);
CC:            track(3)   - z-coord. of the first point (z0);
CC:            track(4)   - radius of circle in cm (Rh);
CC:            track(5)   - tan of the dip angle at r,phi,z (tanl);
CC:            track(6)   - azimuthal angle of starting point of the circle
CC:                         in cylindrical coordinate.
CC:            qq         - charge of particle
CC:            p          - moment of particle
CC:<--------------------------------------------------------------------
      implicit none
#include "tpt_track.inc"
#include "math_constants.inc"

      RECORD /tpt_track_st/ tpt_track
      real qq, pt, pz, p, rad, phi0, psi0, xst, yst, xc, yc
     +   , track
      dimension track(*)   

      qq     = float(tpt_track.q)

      pt    = 1. / tpt_track.invp
      pz    = pt * tpt_track.tanl
      p     = sqrt(pt**2 + pz**2)
      rad   = pt/abs(qq*0.001499)               ! See SN0190
      phi0  = tpt_track.phi0*C_RAD_PER_DEG      ! From degree to rad
      xst   = tpt_track.r0*cos(phi0)
      yst   = tpt_track.r0*sin(phi0)
      psi0  = tpt_track.psi*C_RAD_PER_DEG + qq*C_PI_2
      xc    = xst - rad*cos(psi0)
      yc    = yst - rad*sin(psi0)

      track(1) = xc
      track(2) = yc
      track(3) = tpt_track.z0
      track(4) = rad
      track(5) = tpt_track.tanl
      track(6) = psi0
      track(7) = p

      return
      end





