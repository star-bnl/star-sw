* $Id: tpt_load_track.F,v 1.17 2000/03/10 06:23:02 sakrejda Exp $
* $Log: tpt_load_track.F,v $
* Revision 1.17  2000/03/10 06:23:02  sakrejda
* Track length calculation fixed.
*
* Revision 1.16  2000/03/03 23:46:01  sakrejda
* Loading track for no field
*
* Revision 1.15  2000/02/23 04:07:08  sakrejda
* Cleard id_globtrk - requested by Helen Caines - in the track table
*
* Revision 1.14  1999/11/08 00:51:39  fisyak
* Add atand description for g77
*
* Revision 1.13  1999/11/08 00:48:54  sakrejda
* A bug in track length calculation fixed - tanl was missing.
*
* Revision 1.12  1999/11/07 05:54:48  sakrejda
* Calculation of dip and crossing angle added (moved here from two other places)
* Track length calculation added
*
* Revision 1.11  1999/09/30 00:22:06  sakrejda
* twopi and radtodeg were replaced by C_2PI and C_DEG_PER_RAD
* (Thanks Spiros for pointing it out....)
*
* Revision 1.10  1999/09/14 00:55:48  liq
* use tpt_track_pointers.inc instead of tpt_pointers.inc in order to used by C++ codes
*
* Revision 1.9  1999/09/08 19:39:43  sakrejda
* Calculation for possible number of points on track activated again
* Spiros fixed problems for zero field.
*
* Revision 1.8  1999/09/06 02:18:17  sakrejda
* Calculation of the inverse transverse momentum protected against
* z component of the field being 0.
*
* Revision 1.7  1999/08/25 21:06:26  sakrejda
* calcualtion of possible number of points deactivated
*
* Revision 1.6  1999/08/23 01:05:10  sakrejda
* curvature and npos filled
*
* Revision 1.5  1999/06/24 15:14:37  sakrejda
* possible maximum number of points on track estimate added (Spiros)
*
* Revision 1.4  1999/04/21 23:41:45  sakrejda
* possible number of points on track added to the tpt_track table (Spirose's request
*
* Revision 1.3  1998/07/02 19:17:34  sakrejda
* B field obtained via gufld call
*
* Revision 1.2  1998/01/27 00:35:49  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_LOAD_TRACK(ipt,track_h,track,trk,hit_h,hit)
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
#include "tcl_tphit.inc"
#include "tpt_track_pointers.inc"
#include "phys_constants.inc"
#include "math_constants.inc"

C___________________________________________________________________
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /tpt_track_pointers_st/   trk(*)

C___________________________________________________________________
      INTEGER ipt
      REAL    x0, y0, sgn, sign
      REAL   dx0,dy0
      real   xlocal(3),bfield(3)
      real   atand
      data   xlocal /3*0./

C*** This part if for n_pos (n_max) calculation
C     REAL     xlocal(3), bfield(3)                  !ALREADY EXISTS
      REAL     trakl(6), rr, xc(2), xold, yold, cf, ds
      INTEGER  nmax, ld, ient, iret
      INTEGER  tpt_cross_fact
      data nmax /0/

      LOGICAL nposOn
      DATA nposOn /.TRUE./


C___________________________________________________________________

      track(ipt).flag= trk(ipt).flag   ! good momentum fit
      track(ipt).id_globtrk=0 ! clear the globtrk id
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
      if(abs(bfield(3)).lt.0.1)then
        track(ipt).invp = 0.001
        track(ipt).cov(1) = 0.0
        track(ipt).cov(3) = 0.0
        track(ipt).cov(6) = 0.0
        track(ipt).cov(10) = 0.0
        track(ipt).cov(15) = 0.0
C       curvature
        track(ipt).curvature = 0.0  
        track(ipt).r0 = 0.0  
        track(ipt).phi0=trk(ipt).p(4)
        track(ipt).r0=trk(ipt).p(2)
        track(ipt).psi = trk(ipt).p(5)
      else
        track(ipt).invp =  1.0/(abs(trk(ipt).p(2))*bfield(3)*C_D_CURVATURE)
        track(ipt).cov(1) = abs(trk(ipt).dpar(6))/
     >                    ((trk(ipt).p(2)**2*bfield(3)*C_D_CURVATURE)**2)
C       curvature
        track(ipt).curvature = 1.0/abs(trk(ipt).p(2))    
C       coordinates of the point where track parameters are given
        x0 = abs(trk(ipt).p(2))*cos(trk(ipt).p(5))+trk(ipt).p(3)
        dx0= sqrt( cos(trk(ipt).p(5))**2*abs(trk(ipt).dpar(6)) +
     >          (trk(ipt).p(2)*sin(trk(ipt).p(5))*trk(ipt).dpar(7))**2 +
     >           abs(trk(ipt).dpar(1)) )
        y0 = abs(trk(ipt).p(2))*sin(trk(ipt).p(5))+trk(ipt).p(4)
        dy0= sqrt( sin(trk(ipt).p(5))**2*abs(trk(ipt).dpar(6)) +
     >          (trk(ipt).p(2)*cos(trk(ipt).p(5))*trk(ipt).dpar(7))**2 +
     >           abs(trk(ipt).dpar(4)) )
C
C       azimuthal angle for this point
        track(ipt).phi0 = atan2(y0,x0)
        track(ipt).cov(3)  = ((y0*dx0/(x0*x0))**2 + (dy0/x0)**2) /
     >                  ((1 + (y0/x0)**2)**2)
C
C       radius for this point
        track(ipt).r0 = sqrt(x0**2+y0**2)
        track(ipt).cov(6) = ((x0*dx0)**2 + (y0*dy0)**2)/(track(ipt).r0**2)
        track(ipt).r0 = sqrt(x0**2+y0**2)
        track(ipt).cov(10) = trk(ipt).dpar(8)
        track(ipt).cov(15) = trk(ipt).dpar(9)
C       azimuthal angle for the momentum at the first point
        sgn = sign(1.0,trk(ipt).p(2))
        track(ipt).psi = trk(ipt).p(5)-sgn*0.25*C_2PI
      endif
      if(track(ipt).phi0.lt.0) track(ipt).phi0=
     >track(ipt).phi0+C_2PI
      track(ipt).phi0 = track(ipt).phi0*C_DEG_PER_RAD ! convert to degrees
C
C     tangent of the dip angle
      track(ipt).tanl = trk(ipt).p(7)
C     z coordinate of the point where track parameters are given
      track(ipt).z0 = trk(ipt).p(6)
      track(ipt).nrec = trk(ipt).nhit
      ld = trk(ipt).ipnt(trk(ipt).nhit)
      track(ipt).hitid = hit(ld).id
c
      xold = hit(ld).x
      yold = hit(ld).y
      ds=0.0
      do ld=trk(ipt).nhit-1,1,-1
         ient = trk(ipt).ipnt(ld)
         iret = tpt_cross_fact(hit(ient).row,hit(ient).x,
     >          hit(ient).y,xold,yold,cf,hit(ient).alpha)
c         if(abs(bfield(3)).gt.0.1)
c     >   ds=ds+2.0*abs(trk(ipt).p(2))*asin(0.5*sqrt((xold-hit(ient).x)**2+
c     >   (yold-hit(ient).y)**2)/abs(trk(ipt).p(2)))
         xold = hit(ient).x
         yold = hit(ient).y
         hit(ient).lambda=atand(trk(ipt).p(7))
      end do

      hit( trk(ipt).ipnt(trk(ipt).nhit)).alpha =
     >hit( trk(ipt).ipnt(trk(ipt).nhit-1)).alpha
      hit( trk(ipt).ipnt(trk(ipt).nhit)).lambda =
     >hit( trk(ipt).ipnt(trk(ipt).nhit-1)).lambda
C
      if(abs(bfield(3)).gt.0.1) then
           ds=(( hit( trk(ipt).ipnt(trk(ipt).nhit)).x -trk(ipt).p(3))*
     >        ( hit( trk(ipt).ipnt(1)).x-trk(ipt).p(3))+ 
     >        ( hit( trk(ipt).ipnt(trk(ipt).nhit)).y -trk(ipt).p(4))*
     >        ( hit( trk(ipt).ipnt(1)).y-trk(ipt).p(4)))/
     >     sqrt((( hit( trk(ipt).ipnt(trk(ipt).nhit)).x -trk(ipt).p(3))**2+
     >           ( hit( trk(ipt).ipnt(trk(ipt).nhit)).y -trk(ipt).p(4))**2)*
     >          (( hit( trk(ipt).ipnt(1)).x-trk(ipt).p(3))**2+
     >           ( hit( trk(ipt).ipnt(1)).y-trk(ipt).p(4))**2))
           ds=acos(ds)
           if(ds.lt.0)write(16,*) "negative ds"

           track(ipt).length =abs(trk(ipt).p(2))*ds*sqrt(1.0+track(ipt).tanl**2)
           write(16,*)ipt,track(ipt).length
      else
           track(ipt).length = trk(ipt).p(3)
      endif

      if(track(ipt).psi.lt.0) track(ipt).psi=track(ipt).psi+
     >                        C_2PI
      track(ipt).psi = track(ipt).psi*C_DEG_PER_RAD ! convert to degrees
**cc      if (track(ipt).psi.gt.180.0) track(ipt).psi = 
**cc     +     track(ipt).psi - 360.0
C

      if(nposOn.and.abs(bfield(3)).gt.0.1) then
c---- Calculation of possible (maximum) number of points

C convert to Tesla
      bfield(1) = bfield(1)/10.
      bfield(2) = bfield(2)/10.
      bfield(3) = bfield(3)/10.

C*** Copy parameters to local table
              trakl(1)   = track(ipt).r0
              trakl(2)   = track(ipt).phi0*C_RAD_PER_DEG !degrees to radians
              trakl(3)   = track(ipt).z0
              trakl(4)   = track(ipt).psi*C_RAD_PER_DEG !degrees to radians
              trakl(5)   = track(ipt).tanl
              trakl(6)   = track(ipt).q
              IF((abs(bfield(3)).gt.0.001).AND.((track(ipt).invp).gt.0.001)) THEN
                 rr=1./(C_D_CURVATURE*bfield(3)*10.*track(ipt).invp)
              ELSE
                 rr=9999999.
              ENDIF
              call n_max(trakl,rr,nmax,bfield(3))
      endif
      track(ipt).npos = nmax
c--------------------------------------------------------

      tpt_load_track = STAFCV_OK
      end
