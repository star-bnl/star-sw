c:>------------------------------------------------------------------
C:FILE:         ems_ev0_routines.F
C:DESCRIPTION:  Service routines for EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       Spiros Margetis?
C:BUGS:         None known.
C:HISTORY:      05dec95-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
      SUBROUTINE eut_extra_rad ( p,  rd,     zp,   ang
     +,                          td, cosphi, zint, iercod, dirvec )
c:>------------------------------------------------------------------
C:ROUTINE:      eut_extra_rad
C:DESCRIPTION:  use helix parameters to compute incercept w/ given cylinder...
C:ARGUMENTS:    see code
C:ARGUMENTS:    see code
C:RETURN VALUE: 
c:>------------------------------------------------------------------
      IMPLICIT NONE
CCC
CCC   NEW SUBR: eut_extra_rad 
CCC
CCC        Uses the wayne state helix parameters xctr, yctr, etc. passed
CCC        as arguments.
CCC
CCC        This takes the input helix parameters and computes the intercept
CCC        point with an imaginary cylinder at radius rd (cm).
CCC
CCC    p(I=1,6) : Array of input parameters
CCC    rd       : Radius for point of intercept.
CCC    zp       : Charge
CCC    ang      : Intercept azimuthal angle
CCC    td       : Phase angle of intercept point in helix equation.
CCC    cosphi   : Cosine of angle between particle momentum vector at intercept
CCC               and normal vector to cylinder at intercept point.
CCC    zint     : Z value of intercept point.
CCC    iercod   : 0 , track is intercepted ok.
CCC             : 1 , helix does not intercept the cylinder
CCC    dirvec(I=1,3) : (X,Y,Z) components of unit vector in direction of
CCC                    particle momentum at the intercept point.
CCC
CCC  Variable and array type declarations
CCC
      REAL    pi
      PARAMETER (pi=3.14159265)
      REAL    ang,cosphi,ddd,dsx,dsy,dsz,eq
      REAL    fac1,sfac, fac2,r,rd,sl,t0,td,deltat
      REAL    xc,yc,z0,zint,zp
      INTEGER iercod
      REAL    p(6), dirvec(3)
CCC
CCC   Set to zero
CCC
      iercod = 0
CCC
CCC   Copy input array
CCC
      eq = zp/abs(zp)
      xc = p(1)
      yc = p(2)
      z0 = p(3)
      r  = p(4)
      sl = p(5)
      t0 = p(6)
CCC
CCC    Check helix and cylinder intersect
CCC
      fac1 = xc*xc + yc*yc
      sfac = sqrt( fac1 )
CCC
CCC    If they don't intersect return
CCC
      if ( abs(sfac-r).gt.rd .or. abs(sfac+r).lt.rd ) then
         iercod = 1
         return
      endif
CCC
CCC    Find intersection
CCC
      fac2   = ( rd*rd + fac1 - R*R) / (2.00 * rd * sfac )
      ang    = atan2(yc,xc) + eq*acos(fac2)
      td     = atan2(rd*sin(ang) - yc,rd*cos(ang) - xc)
      cosphi = eq*sin(td-ang) / sqrt(1.00 + sl*sl)
CCC
CCC   Intersection in z
CCC
*     zint = z0 + r*sl*(-eq*td + eq*t0)
      if ( td .lt. 0 ) td = td + 2. * pi
      deltat = mod((-eq*td + eq*t0),2*pi)
      if ( deltat .lt. 0. ) deltat = deltat + 2. * pi
      zint = z0 + r * sl * deltat
CCC
CCC   Momentum trajectory at intersection
CCC
      dsx       =  eq*r*sin(td)
      dsy       = -eq*r*cos(td)
      dsz       = r*sl
      ddd       = sqrt(dsx*dsx + dsy*dsy + dsz*dsz)
CCC
      dirvec(1) = dsx/ddd
      dirvec(2) = dsy/ddd
      dirvec(3) = dsz/ddd
CCC
CCC   That's it
CCC
      END

c:>------------------------------------------------------------------
C:FILE:         ems_ev0_routines.F
C:DESCRIPTION:  Service routines for EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       Spiros Margetis?
C:BUGS:         None known.
C:HISTORY:      05dec95-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
      SUBROUTINE ev0_circle_param(trk,xc,r)
c:>------------------------------------------------------------------
C:ROUTINE:      ev0_circle_param
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
      CALL ev0_track_mom(trk,iflag,p)
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


c:>------------------------------------------------------------------
C:FILE:         ems_ev0_routines.F
C:DESCRIPTION:  Service routines for EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       Spiros Margetis?
C:BUGS:         None known.
C:HISTORY:      05dec95-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
      SUBROUTINE ev0_project_track(xc,r,xp,x)
c:>------------------------------------------------------------------
C:ROUTINE:      ev0_project_track
C:DESCRIPTION:  extrapolate helix and calculate coord of point of closest approach to another point...
C:ARGUMENTS:    see code
C:ARGUMENTS:    see code
C:RETURN VALUE: 
c:>------------------------------------------------------------------
      IMPLICIT NONE
C   Input arguments  :	
C			xc(2)	!position-coord. in x-y plane of the 
C				!centre of the circle (charged part.)
C			r	!radius of the above circle
C			xp(2)	!x-y coord. of new point
C   Output arguments : 	x(2)	!x-y coord. of point of closest approach
C   Functional Description :	Extrapolates a helix and calculates the 
C				coord. of the point of the closest approach
C				from a given point in the bending plane.
C   Created  JAN-1992 S.Margetis   
C   Error conditions : None
C   Status	     : Test
C---------------------------------------------------------------------
      REAL r,xp(2),xc(2),x(2)
      REAL x1(2),x2(2),yy1,yy2,zz1,zz2
      REAL a,b,c
C-----first find the two possible solutions for closest approach-----
      a=xc(1)-xp(1)
      b=xc(2)-xp(2)
      IF(b.eq.0.0) THEN
	x1(2)=xc(2)
	x2(2)=xc(2)
	x1(1)=xc(1)+r	
	x2(1)=xc(1)-r	
      ELSE
      	c=a/b
	yy1= r/sqrt(c*c+1.)
	yy2=-r/sqrt(c*c+1.)
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
      ENDIF	!a.gt.0.

  999 CONTINUE
      RETURN
      END

c:>------------------------------------------------------------------
C:FILE:         ems_ev0_routines.F
C:DESCRIPTION:  Service routines for EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       Spiros Margetis?
C:BUGS:         None known.
C:HISTORY:      05dec95-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
      SUBROUTINE ev0_update_track_param(xc,r,x,trk1,trk2)
c:>------------------------------------------------------------------
C:ROUTINE:      ev0_update_track_param
C:DESCRIPTION:  extrapolate helix and calculate new track parameters at another point
C:ARGUMENTS:    see code
C:ARGUMENTS:    see code
C:RETURN VALUE: 
c:>------------------------------------------------------------------
      IMPLICIT NONE
C   Input arguments  :	xc(2)	!position-coord. in x-y plane of the 
C				!centre of the circle (charged part.)
C			r	!radius of the above circle
C			x(2)	!x-y coord. of new point
C			trk1(6)	!old track parameters
C   Output arguments : 	trk2(6)	!updated track parameters
C   Functional Description :	Extrapolates a helix and calculates the 
C				new helix parameters at another point.
C   Created  JAN-1992 S.Margetis   
C   Error conditions : None
C   Status	     : Tested
C---------------------------------------------------------------------
      REAL r,xc(2),x(2),trk1(6),trk2(6)
      REAL xi,yi,dphi,ds
      REAL axb,dz,arg
C-----calculate the distance ds and the angle between old/new position
      xi = trk1(1)*cos(trk1(2)/trk1(1))
      yi = trk1(1)*sin(trk1(2)/trk1(1))
      axb = ( xi-xc(1))*(x(2)-xc(2)) - (yi-xc(2))*(x(1)-xc(1))
      arg =axb/(r*r)
      IF( arg.ge.1.0 )  arg = 1.
      IF( arg.le.-1.0 ) arg =-1.
      dphi=asin(arg)
      ds =dphi*r
      dz =ds*trk1(5)
C-----calculate the new track parameters
      trk2(1)=sqrt( x(1)*x(1)+x(2)*x(2) )
      IF(x(1).eq.0.) x(1)=0.01
      trk2(2)=atan2( x(2),x(1) )*trk2(1)
      IF(trk1(6).eq.0.) trk1(6)=0.001
      trk2(3)=trk1(3)-(trk1(6)/abs(trk1(6)))*dz
      trk2(4)=trk1(4)+dphi
      trk2(5)=trk1(5)
      trk2(6)=trk1(6)

  999 CONTINUE
      RETURN
      END

c:>------------------------------------------------------------------
C:FILE:         ems_ev0_routines.F
C:DESCRIPTION:  Service routines for EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       Spiros Margetis?
C:BUGS:         None known.
C:HISTORY:      05dec95-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
      SUBROUTINE ev0_track_mom(trk,iflag,p)
c:>------------------------------------------------------------------
C:ROUTINE:      ev0_track_mom
C:DESCRIPTION:  returns track momentum components from track parameters
C:ARGUMENTS:    see code
C:ARGUMENTS:    see code
C:RETURN VALUE: 
c:>------------------------------------------------------------------
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!helix parameters of the track
C			iflag	!if =5 it is a neutral particle and
C				!trk(6)=1./momentum of particle
C   Output arguments : 	p(3)	!momentum vector of track
C   Functional Description :  	returns the momentum components
C				of a track
C   Created  DEC-1991  S.Margetis
C   Error conditions	:	None
C   Status 		:	Tested
C----------------------------------------------------------------------

      INTEGER iflag
      REAL    trk(6),p(3),phi
      REAL    const,beta,lamda,cl
      REAL    sph,cph,pt,ptot
      PARAMETER (const=0.299792458,beta=5.)	!beta(field) in KGauss

C-----Extract momentum components from helix --------------------------

	lamda=atan(trk(5))
	cl   =cos(lamda)
	phi  =trk(4)
	cph  =cos(phi)
	sph  =sin(phi)

C-------check if it is a neutral particle------------------------------

       IF(iflag.eq.5) THEN
	ptot =1./trk(6)
	pt   =ptot*cl
	p(3) =pt*trk(5)
       ELSE
	pt   =(beta*const)/(1000.*abs(trk(6)))
	p(3) =pt*trk(5)
	ptot =sqrt(pt*pt+p(3)*p(3))
       ENDIF	!iflag.eq.5

	p(1) =ptot*cl*cph
	p(2) =ptot*cl*sph

  999 CONTINUE
      RETURN
      END


C******************************************************************************
