
C>--------------------------------------------------------------------
C     EGR_FILL_GLOBTRK - fills the global track table
C<--------------------------------------------------------------------
       INTEGER FUNCTION EGR_LOAD_GLOBTRK(globtrk_h,globtrk
     +     ,igtrk,gtrk,tphit_h,tphit,covar,itpct)
       
C>--------------------------------------------------------------------
C
C     Input arguments
C     globtrk_h    - header for the globtrk table
C     globtrk     - rows of the globtrk table
C     igtrk    - current fitting array id
C     gtrk     - the refitting array
C     tphit_h     - header for the tphit (tpc hits) table
C     hit      - rows of the tphit table
C     covar    - the covariance matrix

C     Functional description:
C     Fills the global track array for different situations
C<--------------------------------------------------------------------
      IMPLICIT NONE
#include "PAM.inc"
#include "egr_globtrk.inc"
#include "tcl_tphit.inc"
#include "egr_track_pointers.inc"

C______________________________________________________________________
 
      RECORD /table_head_st/     globtrk_h
      RECORD /egr_globtrk_st/        globtrk(*)
      RECORD /table_head_st/     tphit_h
      RECORD /tcl_tphit_st/          tphit(*)
      
C_____________________________________________________________________

C     Local variables

      RECORD /track_pointers/ gtrk(mxtrack)
      
      INTEGER igtrk,sgn,lasthit,lastsec,lastrow,iglob,itpct
      INTEGER imsg
      REAL    covar(3,3)
      REAL x0,y0,lambda,pi,radtodeg
      DATA    imsg /0/
      parameter   (pi=3.141592653589)
      parameter   (radtodeg = 180.0/pi)
C

 
      globtrk_h.nok = globtrk_h.nok + 1
      iglob = globtrk_h.nok
      globtrk(iglob).icharge = sign(1.0,gtrk(igtrk).p(2))
      globtrk(iglob).chisq = gtrk(igtrk).p(8)
      globtrk(iglob).invpt = 1.0/(abs(gtrk(igtrk).p(2))*0.001499)
      if (globtrk(iglob).invpt.gt.99999.9) globtrk(iglob).invpt=99999.9
      globtrk(iglob).sflag = (sign(1.0,float(gtrk(igtrk).
     +     flag)))*500.+gtrk(igtrk).flag
      globtrk(iglob).ndegf = gtrk(igtrk).nfit
      globtrk(iglob).id = iglob
      x0 = abs(gtrk(igtrk).p(2))*cos(gtrk(igtrk).p(5))+
     +     gtrk(igtrk).p(3)
      y0 = abs(gtrk(igtrk).p(2))*sin(gtrk(igtrk).p(5))+
     +     gtrk(igtrk).p(4)
      globtrk(iglob).phi0 = atan2(y0,x0)
      globtrk(iglob).phi0 = globtrk(iglob).phi0*radtodeg ! to degrees
      if (globtrk(iglob).phi0.gt.180.0) then
         globtrk(iglob).phi0 = globtrk(iglob).phi0 - 360.0
      elseif( globtrk(iglob).phi0 .lt. -180.0) then
         globtrk(iglob).phi0 = globtrk(iglob).phi0 + 360.0
      endif
      globtrk(iglob).r0 = sqrt(x0**2+y0**2)
      globtrk(iglob).tanl = gtrk(igtrk).p(7)
      sgn = sign(1.0,gtrk(igtrk).p(2))
      globtrk(iglob).psi = gtrk(igtrk).p(5)-sgn*0.5*pi
      globtrk(iglob).psi = globtrk(iglob).psi*radtodeg ! to degrees
      if( globtrk(iglob).psi.gt.180.0) then
         globtrk(iglob).psi=  globtrk(iglob).psi-360.0
      elseif( globtrk(iglob).psi .lt. -180.0) then
         globtrk(iglob).psi = globtrk(iglob).psi + 360.0
      endif
      globtrk(iglob).z0 = gtrk(igtrk).p(6)
      globtrk(iglob).id_emc = 0
      globtrk(iglob).id_pver = 0
      globtrk(iglob).id_tof = 0
      globtrk(iglob).id_hypo_pid = 0
      globtrk(iglob).id_global_pid = 0
      globtrk(iglob).id_part = 0
      globtrk(iglob).cov(1) = covar(1,1)
      globtrk(iglob).cov(2) = covar(1,2)
      globtrk(iglob).cov(3) = covar(1,3)
      globtrk(iglob).cov(4) = covar(2,1)
      globtrk(iglob).cov(5) = covar(2,2)
      globtrk(iglob).cov(6) = covar(2,3)
      globtrk(iglob).cov(7) = covar(3,1)
      globtrk(iglob).cov(8) = covar(3,2)
      globtrk(iglob).cov(9) = covar(3,3)
      globtrk(iglob).cov(10) = gtrk(igtrk).nhit
      globtrk(iglob).cov(11) = 0
      globtrk(iglob).cov(12) = 0
      globtrk(iglob).cov(13) = 0
      globtrk(iglob).cov(14) = 0
      globtrk(iglob).cov(15) = 0

C     Now find last point on track and calculate track length.
      if (itpct.ne.0) then
         lasthit = gtrk(igtrk).ipnt(1)
         lastsec = tphit(lasthit).row/100
         lastrow = tphit(lasthit).row - lastsec*100
         globtrk(iglob).last_row = lastrow
         globtrk(iglob).xlast(1) = tphit(lasthit).x
         globtrk(iglob).xlast(2) = tphit(lasthit).y
         globtrk(iglob).xlast(3) = tphit(lasthit).z
         lambda = atan(globtrk(iglob).tanl)
         globtrk(iglob).length = (globtrk(iglob).xlast(3)
     +        -globtrk(iglob).z0)/sin(lambda)
         if (globtrk(iglob).length.gt.999.9) then
            globtrk(iglob).length=999.9
         endif
      else
         globtrk(iglob).last_row = 0.
         globtrk(iglob).xlast(1) = 0.
         globtrk(iglob).xlast(2) = 0.
         globtrk(iglob).xlast(3) = 0.
         globtrk(iglob).length = -0.
      endif

      return
      end
