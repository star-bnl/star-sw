
      SUBROUTINE circle_param(trk,xc,r)
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
      CALL track_mom(trk,iflag,p)
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

C----------------------------------------------------------------------
C
C				*
C			       ***
C			      ** **
C			     **   **
C		     *********	   *********
C		       ****	      ****
C			 ***	     ***
C			 **	*     **
C			**   *******   **
C		       **  ***     ***  **
C		      **		 **
C
C----------------------------------------------------------------------
      SUBROUTINE fine_approach(xv,x,p,x0)
C----------------------------------------------------------------------
      IMPLICIT NONE
C   Input arguments   	xv	! 3-dimensional position vector 
C				! of the main vertex
C   		   	x	! 3-dimensional position vector 
C				! of the secondary track
C			p	! 3-dimensional momentum vector of the 
C				! reconstructed secondary particle
C   Output arguments :	x0      ! coordinates of closest approach
C
C   Functional Description:	Finds the coordinates of the point of
C				closest approach from the main vertex
C				The input vectors must be calculated
C				at a point close to the vertex.
C   Created  JUN-1992  S.Margetis
C   Error conditions 		: None
C   Status	     		: Tested
C----------------------------------------------------------------------
      REAL 	xv(3),x(3),p(3),x0(3),nom
*----------------------------------------------------------------------

	nom=(p(2)*p(2)+p(3)*p(3))*x(1)-(x(2)*p(2)+x(3)*p(3))*p(1) 
	nom=nom+(xv(1)*p(1)+xv(2)*p(2)+xv(3)*p(3))*p(1)
	x0(1)=nom/( p(1)*p(1)+p(2)*p(2)+p(3)*p(3) )

	IF(p(1).ne.0.) THEN
	 x0(2)=x(2)+(p(2)/p(1))*(x0(1)-x(1))
	 x0(3)=x(3)+(p(3)/p(1))*(x0(1)-x(1))
	ELSE
	 x0(3)=p(2)*(x(3)*p(2)-x(2)*p(3))/(p(2)*p(2)+p(3)*p(3))
		IF(p(2).eq.0.) THEN
		 x0(2)=x(2)
		ELSE
		 x0(2)=-x0(3)*p(3)/p(2)
		ENDIF
	ENDIF

 999  RETURN
      END


C******************************************************************************


      SUBROUTINE inv_mass(trk1,trk2,m,p,invm,iflag)
      IMPLICIT NONE
C   Input arguments  :	trk(6)	!helix parameters of the two tracks
C				!at the (secondary) vertex position.
C				!If one of them is neutral it should be
C				!the second and the 3 first components 
C				!of trk hold the momentum vector.
C			m(2)	!mass hypothesis for the daughter 
C				particles
C			iflag	!if 0 then both particles are charged
C				!if not 0 then one particle is neutral
C   Output arguments : 	p(3)	!momentum vector of reconstructed
C				particle.
C		      invm	!mass of rec. particle
C   Functional Description :  	Invariant mass analysis for 
C				particle decays into two daughter
C				particles with a mass hypothesis
C   Created  DEC-1991  S.Margetis
C   Error conditions : 	None
C   Status :		Development
C----------------------------------------------------------------------
      INTEGER iflag
      REAL    trk1(6),trk2(6),m(2),p(3),invm,phi1,phi2
      REAL    const,beta,lamda1,lamda2,cl1,cl2,sl1,sl2
      REAL    sph1,sph2,cph1,cph2,pt1,pt2,p1,p2,e1,e2
      REAL    px1,px2,py1,py2,pz1,pz2,psq    
      PARAMETER (const=0.29979,beta=5.)

C-----Extract usefull parameters from helix --------------------------
	lamda1=atan(trk1(5))
	cl1   =cos(lamda1)
	sl1   =sin(lamda1)
	phi1  =trk1(4)
	cph1  =cos(phi1)
	sph1  =sin(phi1)
	pt1   =(beta*const)/(1000.*abs(trk1(6)))
	pz1   =pt1*trk1(5)
	p1    =sqrt(pt1*pt1 + pz1*pz1)
	e1    =sqrt(p1*p1+m(1)*m(1))
	px1   =p1*cl1*cph1
	py1   =p1*cl1*sph1
C----second track ------------------------------------------------
	  IF(iflag.eq.0) THEN
	lamda2=atan(trk2(5))
	cl2   =cos(lamda2)
	sl2   =sin(lamda2)
	phi2  =trk2(4)
	cph2  =cos(phi2)
	sph2  =sin(phi2)
	pt2   =(beta*const)/(1000.*abs(trk2(6)))
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
	  ENDIF		!if(iflag.eq.0)
C----calculate the momentum components of the primary-----------
	p(1)  =px1+px2
	p(2)  =py1+py2
	p(3)  =pz1+pz2
C----calculate the invariant mass -----------------------------
	psq   =p(1)*p(1)+p(2)*p(2)+p(3)*p(3)	
	invm=sqrt( (e1+e2)**2. - psq )

  999 CONTINUE
      RETURN
      END


C******************************************************************************

      SUBROUTINE exi_project_track(xc,r,xp,x)
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

C******************************************************************************


      SUBROUTINE track_mom(trk,iflag,p)
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
      PARAMETER (const=0.29979,beta=5.)	!beta(field) in KGauss

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

      SUBROUTINE update_track_param(xc,r,x,trk1,trk2)
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

C******************************************************************************


      SUBROUTINE v0_impact_param(mv,x,p,r_min,iflag)
      IMPLICIT NONE
C   Input arguments   	mv      ! 3-dimensional main vertex position
C                       x	! 3-dimensional position vector 
C				! of the secondary vertex
C			p	! 3-dimensional momentum vector of the 
C				! reconstructed secondary particle
C   Output arguments :	r_min 	!Distance from the main vertex at the 
C			     	!point of closest approach 
C			iflag	!if equal 2 wrong pointing/false particle
C   Functional Description:	It checks first if the candidate particle
C				points away from the vertex (as it should).	
C				Then it finds the impact parameter of a
C				NEUTRAL reconstructed particle 
C				from the main vertex
C   Created  DEC-1991  S.Margetis
C   Error conditions 		: None
C   Status	     		: Tested
C----------------------------------------------------------------------
      INTEGER	iflag
      REAL 	r_min,mv(3),x(3),p(3),check
*
*---------assume a good candidate, reset flag-------------------------
*
	iflag=0
*
*---------check first if the candidate flies away from vertex----------
*---------through the dot product of position and momentum  vector-----
*
	check=(x(1)-mv(1))*p(1)+(x(2)-mv(2))*p(2)+(x(3)-mv(3))*p(3)
	IF(check.lt.0.0) THEN
	  iflag=2
	  goto 999
	ENDIF !if(check<0)
*
*---------calculate the impact parameter of the track from vetex-------
*------------------ Normal way using vector-cross-product -------------
*
	r_min=((x(2)-mv(2))*p(3)-p(2)*(x(3)-mv(3)))**2.+
     +        ((x(3)-mv(3))*p(1)-p(3)*(x(1)-mv(1)))**2.+
     +        ((x(1)-mv(1))*p(2)-p(1)*(x(2)-mv(2)))**2.
	r_min=r_min/(p(1)**2.+p(2)**2.+p(3)**2.)
	r_min=sqrt(r_min)
*----------------------------------------------------------------------

 999  RETURN
      END


C******************************************************************************



      SUBROUTINE vzero_geom(cut,xc1,xc2,r1,r2,x,y,iflag)
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
      REAL	xc1(2),xc2(2),r1,r2
      REAL	x(2),y(2),om1,om2,ph1,ph2
      REAL	a,b,c,d,dia,dtouch,cut
C---clear variables---------------------------------------------------
      CALL VZERO(x,2)
      CALL VZERO(y,2)
C---find the two intersections----------------------------------------
      a=xc1(1)-xc2(1)
      b=xc1(2)-xc2(2)
      dia=sqrt(a*a+b*b)
      dtouch=dia-r1-r2
      c=(r1*r1-r2*r2+a*a+b*b)/2.
      d=(a*a+b*b)*r1*r1-c*c
C---check if there is any solution, one or two -----------------------
      IF(d.lt.0.0) GOTO 137
      IF(d.eq.0.0) iflag=3
      om1=( -b*c+abs(a)*sqrt(d) ) / (a*a+b*b)
      om2=( -b*c-abs(a)*sqrt(d) ) / (a*a+b*b)
C----find the right pair---------------------------------------------
      IF( (r1*r1-om1*om1).ge.0.) THEN
	ph1=sqrt(r1*r1-om1*om1)
	ph2=-ph1
	IF( abs((ph1+a)**2+(om1+b)**2-r2*r2).le.
     +      abs((ph2+a)**2+(om1+b)**2-r2*r2) ) THEN
		y(1)=om1+xc1(2)
		x(1)=ph1+xc1(1)
	ELSE
		y(1)=om1+xc1(2)
		x(1)=ph2+xc1(1)
	ENDIF	
      ENDIF
C------second pair------------------------------------------------
      IF( (r1*r1-om2*om2).ge.0.) THEN
	ph1=sqrt(r1*r1-om2*om2)
	ph2=-ph1
	IF( abs((ph1+a)**2+(om2+b)**2-r2*r2).le.
     +      abs((ph2+a)**2+(om2+b)**2-r2*r2) ) THEN
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

  137 IF(dtouch.le.cut)  THEN
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

C***********************************************************************8

      SUBROUTINE casc_geom(dcut,p,xn,xc,r,x,y,z,iflag)
      IMPLICIT NONE
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
C---------------------------------------------------------------------
      INTEGER iflag
      REAL r,p(3),xn(3),xc(2),dcut
      REAL x(2),y(2),z(2),theta
      REAL a,b,c
C-----find the intersection points of a circle and a straight line----
      IF(p(1).ne.0.0) THEN
	a=p(2)/p(1)
	b=a*( xc(1)-xn(1) ) - xc(2)+xn(2)
	c=(a*a+1)*r*r-b*b
	IF(c.lt.0.0) GOTO 137
	IF(c.eq.0.0) iflag=3
	x(1)=( -a*b+sqrt((a*a+1)*r*r-b*b) )/(a*a+1) + xc(1)
	x(2)=( -a*b-sqrt((a*a+1)*r*r-b*b) )/(a*a+1) + xc(1)
	y(1)=a*(x(1)-xc(1)) + b + xc(2)
	y(2)=a*(x(2)-xc(1)) + b + xc(2)
      ELSE
	x(1)=xn(1)
	x(2)=xn(1)
	c   =r*r-( xn(1)-xc(1) )**2.
	IF(c.lt.0.0) GOTO 137
	IF(c.eq.0.0) iflag=3
	y(1)=xc(2) + sqrt(r*r-( xn(1)-xc(1) )**2.)
	y(2)=xc(2) - sqrt(r*r-( xn(1)-xc(1) )**2.)
      ENDIF	!if(p(1).ne.0.0)
C---------find the z1,z2 points for the neutral---------------------
      z(1)=(p(3)/p(2))*(y(1)-xn(2)) + xn(3)
      z(2)=(p(3)/p(2))*(y(2)-xn(2)) + xn(3)
C-------------------------------------------------------------------
  999 CONTINUE
      RETURN

  137 theta=(xn(1)-xc(1))*p(1)+(xn(2)-xc(2))*p(2)
      theta=theta/sqrt((xn(1)-xc(1))**2.+(xn(2)-xc(2))**2.)
      theta=theta/sqrt(p(1)*p(1)+p(2)*p(2))
      theta=acos(theta)
      c=sqrt((xn(1)-xc(1))**2.+(xn(2)-xc(2))**2.)*sin(theta)
      IF(c.ge.(r+dcut)) THEN
        iflag=5
      ELSE
        iflag=3
        IF(p(1).ne.0.0) THEN
                a=p(2)/p(1)
                y(1)=sqrt(r*r/(a*a+1))
                y(2)=-sqrt(r*r/(a*a+1))
                x(1)=-a*y(1)
                x(2)=-a*y(2)
                y(1)=y(1)+xc(2)
                y(2)=y(2)+xc(2)
                x(1)=x(1)+xc(1)
                x(2)=x(2)+xc(1)
                b=(x(1)-xc(1))*(xn(1)-xc(1))+(y(1)-xc(2))*(xn(2)-xc(2))
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
C---------find the z1 point for the neutral---------------------
      z(1)=(p(3)/p(2))*(y(1)-xn(2)) + xn(3)
      z(2)=z(1)
C------------------------------------------------
      ENDIF
      RETURN
      END

C***********************************************************

      SUBROUTINE casc_impact_param(vertex,x,p,r_min,iflag)
      IMPLICIT NONE
C   Input arguments   	vertex  ! 3-dimensional main vertex position
C                       x	! 3-dimensional position vector 
C				! of the secondary vertex
C			p	! 3-dimensional momentum vector of the 
C				! reconstructed secondary particle
C   Output arguments :	r_min 	!Distance from the main vertex at the 
C			     	!point of closest approach 
C			iflag	!if equal 2 wrong pointing/false particle
C   Functional Description:	It checks first if the candidate particle
C				points away from the vertex (as it should).	
C				Then it finds the impact parameter of a
C				CHARGED reconstructed particle 
C				from the main vertex
C   Created  DEC-1991  S.Margetis
C   Error conditions 		: None
C   Status	     		: 
C----------------------------------------------------------------------
      INTEGER	iflag
      REAL 	r_min,x(3),p(3),check,trk(6)
      REAL	xc(2),r,vertex(3),xn(2),track(6)
*
*---------assume a good candidate, reset flag-------------------------
*
	iflag=0
*
*---------check first if the candidate flies away from vertex----------
*---------through the dot product of position and momentum  vector-----
*
	check=(x(1)-vertex(1))*p(1)+
     +  (x(2)-vertex(2))*p(2)+(x(3)-vertex(3))*p(3)
	IF(check.lt.0.0) THEN
	  iflag=2
	  goto 999
	ENDIF 						!if(check<0)
*
*---------calculate the impact parameter of the track from vetex-------
*

	call make_track(x,p,track)
	call circle_param(track,xc,r)
	call exi_project_track(xc,r,vertex,xn)
	call update_track_param(xc,r,xn,track,trk)
	r_min=sqrt( xn(1)**2+xn(2)**2+trk(3)**2 )

 999  RETURN
      END

*----------------------------------------------------------------------
      SUBROUTINE make_track(x,p,tr)
      IMPLICIT NONE
      REAL	x(3),p(3),tr(6),ph,pt

	ph   =atan2(x(2),x(1))
	tr(1)=x(1)/cos(ph)
	tr(2)=tr(1)*ph
	tr(3)=x(3)
	tr(4)=atan2(p(2),p(1))
	pt   =sqrt( p(1)*p(1)+p(2)*p(2) )
	tr(5)=p(3)/pt
	tr(6)=-5./(3335.668*pt)

 999  RETURN
      END
