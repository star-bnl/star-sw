      SUBROUTINE ev0_circle_param_r(trk,xc,r)
c:>------------------------------------------------------------------
C:ROUTINE:      ev0_circle_param_r
C:DESCRIPTION:  returns x,y coordinates and radius of circle in bending plane
C:ARGUMENTS:    see code
C:ARGUMENTS:    see code
C:RETURN VALUE: 
c:>------------------------------------------------------------------
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!Helix parameters of track
C   Output arguments :	xc(2)	!coordinates of the centre of circle
C			r	!radius of circle
C   Functional Description : Returns the x-y coordinates and the radius
C			     of the circle (projected-helix) on the
C			     bending plane.
C   Created  DEC-1991 S.Margetis   
C   Error conditions	: None
C   Status		: Tested
C----------------------------------------------------------------------
      INTEGER iflag
      REAL    trk(6),xc(2),r
      REAL    x,y,p(3),a,yt1,yt2,xt1,xt2,rxp,test,q
C--------find the radius and the (hit)x-y coordinates------------------
      r=abs(1./trk(6))
      IF(trk(1).eq.0.) trk(1)=0.01
      x=trk(1)*cos( trk(2)/trk(1) )
      y=trk(1)*sin( trk(2)/trk(1) )
C--------get the momentum components-----------------------------------
      iflag=0
      CALL ev0_track_mom_r(trk,iflag,p)
C--------find the two possible solutions------------------------------
      IF(p(1).eq.0.) p(1)=0.01
      a=p(2)/p(1)
      yt1= sqrt( r*r/(a*a+1) ) + y
      yt2=-sqrt( r*r/(a*a+1) ) + y
      xt1=-a*(yt1-y) + x
      xt2=-a*(yt2-y) + x
C-------decide which one is the right one------------------------------
      rxp=(xt1-x)*p(2) - (yt1-y)*p(1)
      test=rxp*trk(6)
      IF(test.gt.0.0) THEN
	xc(1)=xt1
	xc(2)=yt1
      ELSE
	xc(1)=xt2
	xc(2)=yt2
      ENDIF 	!test.lt.0.0
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
