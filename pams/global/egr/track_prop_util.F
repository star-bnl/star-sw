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
      REAL x0,y0,axb,arg,dphih,ds,dz
      REAL x1(2),x2(2),yy1,yy2,zz1,zz2
      REAL a,b,c,ka
      DATA ka / 0.0003 /
C
      i = i + 1
C     
C first find phih, azimuthal angle of starting point wrt helix axis
C
      phih = psi + float(q)*90.0
      if (phih.lt.-180.0) then
         phih = 360.0 - phih
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
      if( arg.ge.1.0) arg = 1.0
      if( arg.le.-1.0) arg = -1.0
      dphih = asin(arg)
      ds = dphih*rh
      dz = ds*tanl
C
C     calculate z0 and psin
C
      x(3) = z0 - float(q)*dz
      dphih = asind(arg)
      psin = phih + dphih
      if( psin.lt.-180.0 ) then
         psin = 360.0 - psin
      elseif (psin.gt.180.0) then
         psin = psin - 360.0
      endif
      x(4) = psin - float(q)*90.0

  999 CONTINUE
      RETURN
      END


C********************************************************************

       SUBROUTINE   PROP_CIRCLE_PARAM(trk,xc,r,beta)
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!Helix parameters of track
C			r	!radius of circle
C                       beta    !MAg field in tesla
C   Output arguments :	xc(2)	!coordinates of the centre of circle
C   Functional Description : Returns the x-y coordinates and the radius
C			     of the circle (projected-helix) on the
C			     bending plane.
C   Created  DEC-1991 S.Margetis   
C   Error conditions	: None
C   Status		: Tested
C----------------------------------------------------------------------
      INTEGER iflag
      REAL    trk(6),xc(2),r,beta
      REAL    x,y,p(3),a,yt1,yt2,xt1,xt2,rxp,test
C--------find the radius and the (hit)x-y coordinates------------------
      IF(trk(1).eq.0.) trk(1)=0.01
      x=trk(1)
      y=trk(2)
C--------get the momentum components-----------------------------------
      iflag=0
      CALL prop_track_mom(trk,r,iflag,p,beta)
C--------find the two possible solutions------------------------------
      IF(p(1).eq.0.) p(1)=0.01
      a=p(2)/p(1)
      yt1= sqrt( r*r/(a*a+1) ) + y
      yt2=-sqrt( r*r/(a*a+1) ) + y
      xt1=-a*(yt1-y) + x
      xt2=-a*(yt2-y) + x
C-------decide which one is the right one------------------------------
      rxp=(xt1-x)*p(2) - (yt1-y)*p(1)
      test=rxp*trk(6)/r
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


**********

      SUBROUTINE prop_track_mom(trk,rc,iflag,p,beta)
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!helix parameters of the track
C			iflag	!if =5 it is a neutral particle and
C				!trk(6)=1./momentum of particle
C                        beta    !field in Tesla
C   Output arguments : 	p(3)	!momentum vector of track
C   Functional Description :  	returns the momentum components
C				of a track
C   Created  DEC-1991  S.Margetis
C   Error conditions	:	None
C   Status 		:	Tested
C----------------------------------------------------------------------

      INTEGER iflag
      REAL    trk(6),p(3),phi,rc
      REAL    const,beta,lamda
      REAL    sph,cph,pt,ptot
      PARAMETER (const=0.299792458)

C-----Extract momentum components from helix --------------------------

	phi  =trk(4)
	cph  =cos(phi)
	sph  =sin(phi)

C-------check if it is a neutral particle------------------------------

       IF(iflag.eq.5) THEN
	lamda=atan(trk(5))
	ptot =1./trk(6)
	pt   =ptot*cos(lamda)
       ELSE
	pt   =(beta*const*rc)/100.
	ptot =sqrt(pt*pt+p(3)*p(3))
       ENDIF	!iflag.eq.5

        p(3) = pt*trk(5)
	p(1) =pt*cph
	p(2) =pt*sph

  999 CONTINUE
      RETURN
      END
