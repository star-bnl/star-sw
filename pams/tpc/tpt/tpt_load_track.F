* $Id: tpt_load_track.F,v 1.3 1998/07/02 19:17:34 sakrejda Exp $
* $Log: tpt_load_track.F,v $
* Revision 1.3  1998/07/02 19:17:34  sakrejda
* B field obtained via gufld call
*
* Revision 1.2  1998/01/27 00:35:49  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_LOAD_TRACK(ipt,track_h,track,trk)
C>--------------------------------------------------------------------
C
C TPT_LOAD_TRACK - calculate track parameters required in tptrack table
C
C DESCRIPTION:
C
C INPUT ARGUMENTS:
C  ipt -    track id
C  trk -    results of the fit
C OUTPUT ARGUMENTS:
C  track_h - header of the track table
C  track  - track table
C AUTHOR:
C<--------------------------------------------------------------------

      IMPLICIT NONE

#include "PAM.inc"

#include "tpt_track.inc"
#include "tpt_pointers.inc"
#include "phys_constants.inc"

C___________________________________________________________________
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /track_pointers/   trk(*)

C___________________________________________________________________
      INTEGER ipt
      REAL    x0, y0, sgn, sign
      REAL   dx0,dy0
      real   twopi,radtodeg
      real   xlocal(3),bfield(3)
      data   xlocal /3*0/
C___________________________________________________________________

      twopi=8.0*atan(1.0)
      radtodeg=45.0/atan(1.0)
      track(ipt).flag= trk(ipt).flag   ! good momentum fit
C
C     number of points to fit
      track(ipt).nfit = trk(ipt).nfit
      track(ipt).id = ipt
C
C     charge
      track(ipt).q=sign(1.0,trk(ipt).p(2))
C
C     chisq from the linear fit
      track(ipt).chisq(1) = trk(ipt).p(8)
      track(ipt).chisq(2) = trk(ipt).p(9)
C     
C     inverse of the pt
      call gufld(xlocal,bfield)
      track(ipt).invp =  1.0/(abs(trk(ipt).p(2))*bfield(3)*C_D_CURVATURE)
      track(ipt).cov(1) = abs(trk(ipt).dpar(6))/
     >                    ((trk(ipt).p(2)**2*bfield(3)*C_D_CURVATURE)**2)
C
C     coordinates of the point where track parameters are given
      x0 = abs(trk(ipt).p(2))*cos(trk(ipt).p(5))+trk(ipt).p(3)
      dx0= sqrt( cos(trk(ipt).p(5))**2*abs(trk(ipt).dpar(6)) +
     >          (trk(ipt).p(2)*sin(trk(ipt).p(5))*trk(ipt).dpar(7))**2 +
     >           abs(trk(ipt).dpar(1)) )
      y0 = abs(trk(ipt).p(2))*sin(trk(ipt).p(5))+trk(ipt).p(4)
      dy0= sqrt( sin(trk(ipt).p(5))**2*abs(trk(ipt).dpar(6)) +
     >          (trk(ipt).p(2)*cos(trk(ipt).p(5))*trk(ipt).dpar(7))**2 +
     >           abs(trk(ipt).dpar(4)) )
C
C     azimuthal angle for this point
      track(ipt).phi0 = atan2(y0,x0)
      if(track(ipt).phi0.lt.0) track(ipt).phi0=
     >track(ipt).phi0+twopi
      track(ipt).phi0 = track(ipt).phi0*radtodeg  ! convert to degrees
**cc      if (track(ipt).phi0.gt.180.0) track(ipt).phi0 = 
**cc     +     track(ipt).phi0 - 360.0
      track(ipt).cov(3)  = ((y0*dx0/(x0*x0))**2 + (dy0/x0)**2) /
     >                  ((1 + (y0/x0)**2)**2)
C
C     radius for this point
      track(ipt).r0 = sqrt(x0**2+y0**2)
      track(ipt).cov(6) = ((x0*dx0)**2 + (y0*dy0)**2)/(track(ipt).r0**2)
C
C     tangent of the dip angle
      track(ipt).tanl = trk(ipt).p(7)
      track(ipt).cov(10) = trk(ipt).dpar(8)
C     
C     z coordinate of the point where track parameters are given
      track(ipt).z0 = trk(ipt).p(6)
      track(ipt).cov(15) = trk(ipt).dpar(9)
C
      track(ipt).nrec = trk(ipt).nhit
C
C     azimuthal angle for the momentum at the first point
      sgn = sign(1.0,trk(ipt).p(2))
      track(ipt).psi = trk(ipt).p(5)-sgn*0.25*twopi
      if(track(ipt).psi.lt.0) track(ipt).psi=track(ipt).psi+
     >                        twopi
      track(ipt).psi = track(ipt).psi*radtodeg  ! convert to degrees
**cc      if (track(ipt).psi.gt.180.0) track(ipt).psi = 
**cc     +     track(ipt).psi - 360.0
C
      tpt_load_track = STAFCV_OK
      end
