      SUBROUTINE circle_param(trk,xc,r,beta)
      IMPLICIT NONE
C----------------------------------------------------------------------
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
      REAL    trk(6),xc(2),r,beta
      REAL    x,y,p(3),a,b,ab,test
C--------find the radius and the (hit)x-y coordinates------------------
      r=abs(1./trk(6))
      IF(trk(1).eq.0.) trk(1)=0.01
      a=trk(2)/trk(1)
      x=trk(1)*cos(a)
      y=trk(1)*sin(a)
C--------get the momentum components-----------------------------------
      iflag=0
      CALL track_mom(trk,iflag,p,beta)
C--------find the two possible solutions------------------------------
      IF(p(1).eq.0.) p(1)=0.01
      a=p(2)/p(1)
      b=sqrt( r*r/(a*a+1) )
      ab=a*b
C-------decide which one is the right one------------------------------
      test=(ab*p(2) + b*p(1))*trk(6)
      IF(test.lt.0.0) THEN
	xc(1)=-ab + x
	xc(2)= b + y
      ELSE
	xc(1)=ab + x
	xc(2)=-b + y
      ENDIF 	!test.lt.0.0
C---------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END

C*********************************************************************

      SUBROUTINE inv_mass(trk1,trk2,m,p,invm,iflag,beta)
      IMPLICIT NONE
C---------------------------------------------------------------------
C   Input arguments  :	trk(6)	!helix parameters of the two tracks
C				!at the (secondary) vertex position.
C				!If one of them is neutral it should be
C				!the second and the 3 first components 
C				!of trk hold the momentum vector.
C			m(2)	!mass hypothesis for the daughter 
C				particles
C			iflag	!if 0 then both particles are charged
C				!if not 0 then one particle is neutral
C                       beta    !magnetic field (kGauss)
C   Output arguments : 	p(3)	!momentum vector of reconstructed
C				particle.
C		      invm	!mass of rec. particle
C   Functional Description :  	Invariant mass analysis for 
C				particle decays into two daughter
C				particles with a mass hypothesis
C   Created  DEC-1991  S.Margetis
C   Error conditions : 	None
C   Status :		Development
C---------------------------------------------------------------------
#include "phys_constants.inc"
      INTEGER iflag
      REAL    trk1(6),trk2(6),m(2),p(3),invm,phi1,phi2
      REAL    beta,lamda1,lamda2,cl1,cl2,sl1,sl2
      REAL    sph1,sph2,cph1,cph2,pt1,pt2,p1,p2,e1,e2
      REAL    px1,px2,py1,py2,pz1,pz2,psq    

C-----Extract usefull parameters from helix --------------------------
      lamda1=atan(trk1(5))
      cl1   =cos(lamda1)
      sl1   =sin(lamda1)
      phi1  =trk1(4)
      cph1  =cos(phi1)
      sph1  =sin(phi1)
      pt1   =abs(beta*C_D_CURVATURE/trk1(6))
      pz1   =pt1*trk1(5)
      p1    =sqrt(pt1*pt1 + pz1*pz1)
      e1    =sqrt(p1*p1+m(1)*m(1))
      px1   =p1*cl1*cph1
      py1   =p1*cl1*sph1
C----second track ----------------------------------------------------
      IF(iflag.eq.0) THEN
        lamda2=atan(trk2(5))
	cl2   =cos(lamda2)
	sl2   =sin(lamda2)
	phi2  =trk2(4)
	cph2  =cos(phi2)
	sph2  =sin(phi2)
	pt2   =abs(beta*C_D_CURVATURE/trk2(6))
	pz2   =pt2*trk2(5)
	p2    =sqrt(pt2*pt2 + pz2*pz2)
	e2    =sqrt(p2*p2+m(2)*m(2))
	px2   =p2*cl2*cph2
	py2   =p2*cl2*sph2
      ELSE
	px2   =trk2(1)
	py2   =trk2(2)
	pz2   =trk2(3)
	p2    =sqrt(px2*px2+py2*py2+pz2*pz2)
	e2    =sqrt(p2*p2+m(2)*m(2))
      ENDIF !if(iflag.eq.0)
C----calculate the momentum components of the primary-----------------
      p(1)  =px1+px2
      p(2)  =py1+py2
      p(3)  =pz1+pz2
C----calculate the invariant mass ------------------------------------
      psq   =p(1)*p(1)+p(2)*p(2)+p(3)*p(3)	
      invm=sqrt( (e1+e2)**2. - psq )

  999 CONTINUE
      RETURN
      END

C*********************************************************************

      SUBROUTINE track_mom(trk,iflag,p,beta)
      IMPLICIT NONE
C---------------------------------------------------------------------
C   Input arguments  :	trk(6)	!helix parameters of the track
C			iflag	!if =5 it is a neutral particle and
C				!trk(6)=1./momentum of particle
C                       beta    !magnetic field (kGauss)
C   Output arguments : 	p(3)	!momentum vector of track
C   Functional Description :  	returns the momentum components
C				of a track
C   Created  DEC-1991  S.Margetis
C   Error conditions	:	None
C   Status 		:	Tested
C----------------------------------------------------------------------
#include "phys_constants.inc"
      INTEGER iflag
      REAL    trk(6),p(3),phi
      REAL    beta
      REAL    sph,cph,pt

C-----Extract momentum components from helix --------------------------

	phi  =trk(4)
	cph  =cos(phi)
	sph  =sin(phi)

C-------check if it is a neutral particle------------------------------

       IF(iflag.eq.5) THEN
	pt   =cos(atan(trk(5)))/trk(6)
       ELSE
	pt   =abs(beta*C_D_CURVATURE/trk(6))
       ENDIF	!iflag.eq.5

	p(3) =pt*trk(5)
	p(1) =pt*cph
	p(2) =pt*sph

  999 CONTINUE
      RETURN
      END

C*********************************************************************

      SUBROUTINE update_track_param(xc,r,x,trk1,trk2)
      IMPLICIT NONE
C---------------------------------------------------------------------
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
      arg = trk1(2)/trk1(1)
      xi = trk1(1)*cos(arg)
      yi = trk1(1)*sin(arg)
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

C*********************************************************************

      SUBROUTINE casc_geom(dcut,p,xn,xc,r,x,y,z,iflag)
      IMPLICIT NONE
C---------------------------------------------------------------------
C   Input arguments  :	dcut    !dca cut value 
C                       p(3)	!three-momentum of neutral particle
C			xn(3)	!position-coord. of neutral particle
C			xc(2)	!position-coord. in x-y plane of the 
C				!centre of the circle (charged part.)
C			r	!radius of the above circle
C   Output arguments : 	x(2)	!x-coord. of the two possible intersections
C			y(2)	!y-coord. "   "        "        "
C			z(2)	!z-coord. "   "        "        "
C			iflag	!if iflag=5 there is no solution
C				!if iflag=3 there only one solution
C   Functional Description :	It finds the two possible intersections
C				in the bending plane of a circle and a
C				straight line (charge-neutral particle). 
C   Created  DEC-1991 S.Margetis   
C   Error conditions : None
C   Status	     : Tested
C-------------------------------------------------------------------
      INTEGER iflag
      REAL r,p(3),xn(3),xc(2),dcut
      REAL x(2),y(2),z(2),xd,yd,rd
      REAL*8 a,b,c,d,rsq,ab,theta
C-----find the intersection points of a circle and a straight line--
      rsq=r*r
      xd=xn(1)-xc(1)
      yd=xn(2)-xc(2)
      IF(p(1).ne.0.0) THEN
	a=p(2)/p(1)
	b=-a*xd +yd
	d=a*a+1
	c=d*rsq-b*b
	IF(c.lt.0.0) GOTO 137
	IF(c.eq.0.0) iflag=3
	c=sqrt(c)
	ab=a*b
	x(1)=( -ab+c )/d + xc(1)
	x(2)=( -ab-c )/d + xc(1)
	y(1)=a*(x(1)-xc(1)) + b + xc(2)
	y(2)=a*(x(2)-xc(1)) + b + xc(2)
      ELSE
	x(1)=xn(1)
	x(2)=xn(1)
	c   =rsq-(xd**2.)
	IF(c.lt.0.0) GOTO 137
	IF(c.eq.0.0) iflag=3
	c=sqrt(c)
	y(1)=xc(2) + c
	y(2)=xc(2) - c
      ENDIF	!if(p(1).ne.0.0)
C---------find the z1,z2 points for the neutral---------------------
      d=p(3)/p(2)
      z(1)=d*(y(1)-xn(2)) + xn(3)
      z(2)=d*(y(2)-xn(2)) + xn(3)
C-------------------------------------------------------------------
  999 CONTINUE
      RETURN

  137 theta=xd*p(1)+yd*p(2)
      rd = sqrt(xd**2. + yd**2.)
      theta=theta/rd
      theta=theta/sqrt(p(1)*p(1)+p(2)*p(2))
      theta=acos(theta)
      c=rd*sin(theta)
      IF(c.ge.(r+dcut)) THEN
        iflag=5
      ELSE
        iflag=3
        IF(p(1).ne.0.0) THEN
                a=p(2)/p(1)
                y(1)=sqrt(rsq/(a*a+1))
                y(2)=-y(1)
                x(1)=-a*y(1)
                x(2)=-a*y(2)
                y(1)=y(1)+xc(2)
                y(2)=y(2)+xc(2)
                x(1)=x(1)+xc(1)
                x(2)=x(2)+xc(1)
                b=(x(1)-xc(1))*xd+(y(1)-xc(2))*yd
                IF(b.gt.0.0) THEN
                  x(2)=x(1)
                  y(2)=y(1)
                ELSE
                  x(1)=x(2)
                  y(1)=y(2)
                ENDIF
        ELSE
                y(1)=xc(2)
                y(2)=xc(2)
                x(1)=xc(1)+r
                x(2)=xc(1)-r
                IF(xn(1).gt.xc(1)) THEN
                  x(2)=x(1)
                  y(2)=y(1)
                ELSE
                  x(1)=x(2)
                  y(1)=y(2)
                ENDIF
        ENDIF
C---------find the z1 point for the neutral---------------------------
      z(1)=(p(3)/p(2))*(y(1)-xn(2)) + xn(3)
      z(2)=z(1)
C---------------------------------------------------------------------
      ENDIF
      RETURN
      END
