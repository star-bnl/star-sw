#include "math_constants.h"
C
      SUBROUTINE   PROP_PROJECT_TRACK(psi,q,pt,tanl,x0,y0,bfld,z0,xp,x)
      IMPLICIT NONE
CC:>----------------------------------------------------------------------
CC: INPUT ARGUMENTS  :	
CC:                       psi     !azimuthal angle for pt at first point
CC:                               !on track
CC:                       q       !charge
CC:                       pt      !transverse momentum of track
CC:                       tanl    !tangent of dip angle at first point on track
CC:                       xx0     !x
CC:                       yy0     !y
CC:                       z0      !z coord of first point on track
CC:                       bfld    !magnetic field in kG
CC:                       xp      !x & y coord. of new point
CC:
CC: OUTPUT ARGUMENTS : 	x(4)	!xyz coord. of point of closest approach
CC:                               !and phi at that point
CC:
CC: DESCRIPTION :	Extrapolates a helix 
CC:				and calculates the 
CC:				coord. of the point of the closest approach
CC:				from a given point in the bending plane.
CC:
CC: AUTHOR:  JAN-1992 S.Margetis 
CC: MODIFIED NOV-1993 E. G. Judd  
CC: ERROR CONDITIONS : None
CC: STATUS	     : Test
CC:
CC:>---------------------------------------------------------------------

      REAL psi,pt,tanl,r0,phi,bfld,z0
      INTEGER q,i
      REAL rh,phih,psin
      REAL xp(2),xc(2),x(4)
      REAL x0,y0,axb,adb,arg,dphih,ds,dz
      REAL x1(2),x2(2),yy1,yy2,zz1,zz2
      REAL a,b,c,ka
      DATA ka / 0.000299792458 /
C
      i = i + 1
C     
C first find phih, azimuthal angle of starting point wrt helix axis
C
      phih = psi + float(q)*90.0
      if (phih.lt.-180.0) then
         phih = 360.0 + phih
      elseif (phih.gt.180.0) then
         phih = phih - 360.0
      endif
C
C now find rh, the radius of the helix
C
      rh = pt/abs(ka*float(q)*bfld)
C
C now calculate the coords of the centre of the helix
C
      xc(1) = x0 - rh*cosd(phih)
      xc(2) = y0 - rh*sind(phih)
C
C-----first find the two possible solutions for closest approach-----
C
      a=xc(1)-xp(1)
      b=xc(2)-xp(2)
      IF(b.eq.0.0) THEN
	x1(2)=xc(2)
	x2(2)=xc(2)
	x1(1)=xc(1)+rh
	x2(1)=xc(1)-rh
      ELSE
      	c=a/b
	yy1= rh/sqrt(c*c+1.)
	yy2=-rh/sqrt(c*c+1.)
	zz1=c*yy1
	zz2=c*yy2
	x1(1)=zz1+xc(1)
	x2(1)=zz2+xc(1)
	x1(2)=yy1+xc(2)
	x2(2)=yy2+xc(2)
      ENDIF	!b.eq.0
C-------choose the right one----------------------------------------
      a=(xp(1)-xc(1))*(x1(1)-xc(1)) + (xp(2)-xc(2))*(x1(2)-xc(2))
      IF(a.gt.0.) THEN
	x(1)=x1(1)
	x(2)=x1(2)
      ELSE
	x(1)=x2(1)
	x(2)=x2(2)
      ENDIF	
C
C     calculate the distance ds and the angle between the old and new position
C
      axb = (x0-xc(1))*(x(2)-xc(2)) - (y0-xc(2))*(x(1)-xc(1))
      arg = axb/(rh*rh)
      adb = ( x0-xc(1))*(x(1)-xc(1)) + (y0-xc(2))*(x(2)-xc(2))
      adb = adb/(rh*rh)
      if( arg.ge.1.0) arg = 1.0
      if( arg.le.-1.0) arg = -1.0


C      if( arg .gt. 0) then
C         dphih=acos(adb)
C      elseif( arg .lt. 0 .and. adb .gt. 0) then
C         dphih = asin(arg)
C      else
C         dphih = - acos(adb)
C      endif
C      ds = dphih*rh
C      dz = ds*tanl
C
C     calculate z0 and psin
C

      dphih = acos(adb)
      if( arg .gt.0) dphih = (2*C_PI)-dphih
      if(q .lt. 0) dphih = 2*C_PI - dphih

      dphih = dphih-(2*C_PI)

*      dphih = asin(arg)
      x(3) = z0 + rh*tanl*dphih

      psin = phih + (dphih*180./C_PI)
      if( psin.lt.-180.0 ) then
         psin = 360.0 + psin
      elseif (psin.gt.180.0) then
         psin = psin - 360.0
      endif
      x(4) = psin - float(q)*90.0
      
  999 CONTINUE
      RETURN
      END


C********************************************************************

      SUBROUTINE   PROP_CIRCLE_PARAM(trk,xc,r)
      IMPLICIT NONE
C   Input arguments  :	trk(7)	!custom helix parameters of track
C			r	!radius of circle
C   Output arguments :	xc(2)	!coordinates of the centre of circle
C   Functional Description : Returns the x-y coordinates and the radius
C			     of the circle (projected-helix) on the
C			     bending plane.
C   Created  DEC-1991 S.Margetis   
C   Error conditions	: None
C   Status		: Tested
C----------------------------------------------------------------------
      REAL    trk(7),xc(2),r
      REAL    x,y,p(3),a,yt1,yt2,xt1,xt2,rxp,test
C--------find the radius and the (hit)x-y coordinates------------------
      x=trk(1)
      y=trk(2)
C--------get the momentum components-----------------------------------
      CALL prop_track_mom(trk,p)
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

C

C************************************************************************



      SUBROUTINE    PROP_VZERO_GEOM(cut,xc1,xc2,r1,r2,x,y,iflag)
      IMPLICIT NONE
C   Input arguments  :	xc1(2)	!centre x-y coord. of the first circle
C			xc2(2)	!centre x-y coord. of the sec. circle
C			r1	!radius of first circle
C			r2	!radius of second circle
C   Output arguments :	x(2)	!x-coord. of two possible solutions
C			y(2)	!y-coord. of   "  "          "
C			iflag	!iflag=5 if no solution, =3 if only one
C   Functional Description :	It finds all possible intersections in
C				the bending plane of two circles.
C   Created  DEC-1991 S.Margetis   
C   Error conditions	: None
C   Status		: Tested
C----------------------------------------------------------------------
      INTEGER	iflag
      REAL	xc1(2),xc2(2),r1,r2,r1sq,r2sq
      REAL	x(2),y(2),om1,om2,ph1,ph2
      REAL	a,b,c,d,dia,dtouch,cut
C---clear variables cleared before entering routine--------------------
c      CALL VZERO(x,2)
c      CALL VZERO(y,2)
C---find the two intersections----------------------------------------
      r1sq = r1*r1
      r2sq = r2*r2
      a=xc1(1)-xc2(1)
      b=xc1(2)-xc2(2)
      c=(r1sq-r2sq+a*a+b*b)/2.
      d=(a*a+b*b)*r1sq-c*c
C---check if there is any solution, one or two -----------------------
      IF(d.lt.0.0) GOTO 137
      IF(d.eq.0.0) iflag=3 
      d = sqrt(d)
      om1=( -b*c+abs(a)*d ) / (a*a+b*b)
      om2=( -b*c-abs(a)*d ) / (a*a+b*b)
C----find the right pair---------------------------------------------
      IF( (r1sq-om1*om1).ge.0.) THEN
	ph1=sqrt(r1sq-om1*om1)
	ph2=-ph1
	IF( abs((ph1+a)**2+(om1+b)**2-r2sq).le.
     +      abs((ph2+a)**2+(om1+b)**2-r2sq) ) THEN
		y(1)=om1+xc1(2)
		x(1)=ph1+xc1(1)
	ELSE
		y(1)=om1+xc1(2)
		x(1)=ph2+xc1(1)
	ENDIF	
      ENDIF
C------second pair------------------------------------------------
      IF( (r1sq-om2*om2).ge.0.) THEN
	ph1=sqrt(r1sq-om2*om2)
	ph2=-ph1
	IF( abs((ph1+a)**2+(om2+b)**2-r2sq).le.
     +      abs((ph2+a)**2+(om2+b)**2-r2sq) ) THEN
		y(2)=om2+xc1(2)
		x(2)=ph1+xc1(1)
	ELSE
		y(2)=om2+xc1(2)
		x(2)=ph2+xc1(1)
	ENDIF	
      ENDIF
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN

  137 continue
      dia=sqrt(a*a+b*b)
      dtouch=dia-r1-r2
      IF(dtouch .le. sqrt(cut))  THEN
        iflag=3
        x(1)=xc1(1) + r1*(xc2(1)-xc1(1))/dia
        y(1)=xc1(2) + r1*(xc2(2)-xc1(2))/dia
        x(2)=x(1)
        y(2)=y(1)
      ELSE
        iflag=5
      RETURN
      ENDIF

      END

C**********************************************************************
C----------------------------------------------------------------------
      SUBROUTINE PROP_FINE_APPROACH(xv,x,p,x0)
C----------------------------------------------------------------------
      IMPLICIT NONE
C   Input arguments     xv      ! 3-dimensional position vector 
C                               ! of the main vertex
C                       x       ! 3-dimensional position vector 
C                               ! of the secondary track
C                       p       ! 3-dimensional momentum vector of the 
C                               ! reconstructed secondary particle
C   Output arguments :  x0      ! coordinates of closest approach
C
C   Functional Description:     Finds the coordinates of the point of
C                               closest approach from the main vertex
C                               The input vectors must be calculated
C                               at a point close to the vertex.
C   Created  JUN-1992  S.Margetis
C   Modified: 4/2/99 R.L.Ray
C   Error conditions            : Error for p(1)=0 fixed (4/2/99 RLR)
C   Status                      : Tested
C----------------------------------------------------------------------
      REAL      xv(3),x(3),p(3),x0(3),nom,cut,det
      DATA  cut /1.0E-06/
*----------------------------------------------------------------------

        nom=(p(2)*p(2)+p(3)*p(3))*x(1)-(x(2)*p(2)+x(3)*p(3))*p(1) 
        nom=nom+(xv(1)*p(1)+xv(2)*p(2)+xv(3)*p(3))*p(1)
        x0(1)=nom/( p(1)*p(1)+p(2)*p(2)+p(3)*p(3) )

C       IF(p(1).ne.0.) THEN
        IF(abs(p(1)) .gt. cut) THEN              ! RLR (4/99)
         x0(2)=x(2)+(p(2)/p(1))*(x0(1)-x(1))
         x0(3)=x(3)+(p(3)/p(1))*(x0(1)-x(1))
        ELSE
         det = p(2)*p(2) + p(3)*p(3)             ! RLR (4/99)
         x0(2)=(x(2)*p(3)*p(3)+xv(2)*p(2)*p(2)+  ! RLR (4/99)
     1         (xv(3)-x(3))*p(2)*p(3))/det       ! RLR (4/99)
         x0(3)=(x(3)*p(2)*p(2)+xv(3)*p(3)*p(3)+  ! RLR (4/99)
     1         (xv(2)-x(2))*p(2)*p(3))/det       ! RLR (4/99)

C        x0(3)=p(2)*(x(3)*p(2)-x(2)*p(3))/(p(2)*p(2)+p(3)*p(3))
C               IF(p(2).eq.0.) THEN
C                x0(2)=x(2)
C               ELSE
C                x0(2)=-x0(3)*p(3)/p(2)
C               ENDIF

        ENDIF

      RETURN
      END


C*********************************************************************

      SUBROUTINE PROP_TRACK_MOM(trk,p)
      IMPLICIT NONE
C   Input arguments  :	trk(7)	!custom helix parameters of the track
C
C   Output arguments : 	p(3)	!momentum vector of track
C   Functional Description :  	returns the momentum components
C				of a track
C   Created  DEC-1991  S.Margetis
C   Error conditions	:	None
C   Status 		:	Tested
C----------------------------------------------------------------------

      REAL    trk(7),p(3),psi
      REAL    sph,cph,pt

C-----Extract momentum components from helix --------------------------

	psi  =trk(4)
	cph  =cosd(psi)
	sph  =sind(psi)

	pt   =trk(7)

        p(3) = pt*trk(5)
	p(1) =pt*cph
	p(2) =pt*sph

  999 CONTINUE
      RETURN
      END


C******************************************************************************


