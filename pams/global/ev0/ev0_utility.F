       SUBROUTINE ev0_circle_param(trk,xc,r,beta)
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
      REAL    x,y,p(3),a,b,ab,rxp,test
C--------find the radius and the (hit)x-y coordinates------------------
      IF(trk(1).eq.0.) trk(1)=0.01
      x=trk(1)*cos( trk(2) )
      y=trk(1)*sin( trk(2))
C--------get the momentum components-----------------------------------
      iflag=0
      CALL ev0_track_mom(trk,r,iflag,p,beta)
C--------find the two possible solutions------------------------------
      IF(p(1).eq.0.) p(1)=0.01
      a=p(2)/p(1)
      b = sqrt( r*r/(a*a+1) )
C     yt1= b + y
C     yt2=-b + y
C     xt1=-a*b + x
C     xt2= a*b + x
      ab = a*b
C-------decide which one is the right one------------------------------
C     rxp=(xt1-x)*p(2) - b*p(1)
      rxp=-(ab*p(2) + b*p(1))
      test=rxp*trk(6)/r
      IF(test.gt.0.0) THEN
C	xc(1)=xt1
C	xc(2)=yt1
	xc(1)=-ab + x
	xc(2)= b + y
      ELSE
C	xc(1)=xt2
C	xc(2)=yt2
	xc(1)=ab + x
	xc(2)=-b + y
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
      SUBROUTINE ev0_fine_approach(xv,x,p,x0)
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
      REAL 	xv(3),x(3),p(3),x0(3),nom,a
*----------------------------------------------------------------------

	nom=(p(2)*p(2)+p(3)*p(3))*x(1)-(x(2)*p(2)+x(3)*p(3))*p(1) 
	nom=nom+(xv(1)*p(1)+xv(2)*p(2)+xv(3)*p(3))*p(1)
	x0(1)=nom/( p(1)*p(1)+p(2)*p(2)+p(3)*p(3) )

	IF(p(1).ne.0.) THEN
C	 x0(2)=x(2)+(p(2)/p(1))*(x0(1)-x(1))
C	 x0(3)=x(3)+(p(3)/p(1))*(x0(1)-x(1))
         a = (x0(1)-x(1))/p(1)
	 x0(2)=x(2)+p(2)*a
	 x0(3)=x(3)+p(3)*a
	ELSE
C	 x0(3)=p(2)*(x(3)*p(2)-x(2)*p(3))/(p(2)*p(2)+p(3)*p(3))
         a = p(3)/p(2)
	 x0(3)=(x(3)-x(2)*a)/(1+a*a)
		IF(p(2).eq.0.) THEN
		 x0(2)=x(2)
		ELSE
C		 x0(2)=-x0(3)*p(3)/p(2)
		 x0(2)=-x0(3)*a
		ENDIF
	ENDIF

 999  RETURN
      END


C******************************************************************************


      SUBROUTINE ev0_inv_mass(p1,p2,m,psq,ppar,inv_mass,env0,e1,rap)
      IMPLICIT NONE
C   Input arguments  :	p1,p2   ! Total momenta of the two tracks
C				!at the (secondary) vertex position.
C			m(2)	!mass hypothesis for the daughter 
C				particles
C                       psq	! momentum squared of reconstructed
C				particle.
C                       ppar    ! Parallel mtm of rec. particle
C   Output arguments : inv_mass ! mass of rec. particle
C                      env0     ! Energy of reconstructed particle
C                      e1       ! Energy of positive daughter particle
C                      rap      ! Rapidity of rec. particle
C   Functional Description :  	Invariant mass analysis for 
C				particle decays into two daughter
C				particles with a mass hypothesis
C   Created  DEC-1991  S.Margetis
C   Error conditions : 	None
C   Status :		Development
C----------------------------------------------------------------------
      REAL    p1,p2,m(2),inv_mass
      REAL    e1,e2,psq,env0,ppar,rap

         
	e1    = sqrt(p1*p1 + m(1)*m(1))
	e2    = sqrt(p2*p2 + m(2)*m(2))
C----calculate the invariant mass -----------------------------
	inv_mass=( (e1+e2)**2. - psq )
        env0 = sqrt(psq+inv_mass)
        inv_mass = sqrt(inv_mass)
C----Calculate rapidity
        rap = 0.5*log( (env0+ppar)/(env0-ppar))

  999 CONTINUE
      RETURN
      END


C******************************************************************************

      SUBROUTINE ev0_project_track(xc,r,xp,x)
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
      REAL x1,x2,yy,zz
      REAL a,b,c,d
C-----first find the two possible solutions for closest approach-----
      d=xc(1)-xp(1)
      a=xc(2)-xp(2)
      IF(a.eq.0.0) THEN
        IF(d.ge.0) THEN
          x(1) = xc(1)-r
        ELSE
          x(1) = xc(1)+r
        ENDIF
        x(2) = xc(2)
      ELSE
        c=d/a
	yy= r/sqrt(c*c+1.)
	zz=c*yy
        IF(a.ge.0) THEN
          x(1) = -zz+xc(1)
          x(2) = -yy+xc(2)
        ELSE
          x(1) = zz+xc(1)
          x(2) = yy+xc(2)
        ENDIF
      ENDIF	!a.eq.0

C----Old, slow way
C     d=xc(1)-xp(1)
C     a=xc(2)-xp(2)
C     IF(a.eq.0.0) THEN
C       x(2)=xc(2)
C       x2=xc(2)
C       x(1)=xc(1)+r    
C       x1=xc(1)-r      
C     ELSE
C       c=d/a
C       yy= r/sqrt(c*c+1.)
C       zz=c*yy
C       x(1)=zz+xc(1)
C       x1=-zz+xc(1)
C       x(2)=yy+xc(2)
C       x2=-yy+xc(2)
C     ENDIF     !a.eq.0
C-------choose the right one----------------------------------------
C     b=(xp(1)-xc(1))*(x(1)-xc(1)) + (xp(2)-xc(2))*(x(2)-xc(2))
C     IF(b.le.0.) THEN
C	x(1)=x1
C	x(2)=x2
C     ENDIF	!b.le.0.

  999 CONTINUE
      RETURN
      END

C******************************************************************************


      SUBROUTINE ev0_track_mom(trk,rc,iflag,p,beta)
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
      REAL    const,beta
      REAL    sph,cph,pt
      PARAMETER (const=0.299792458)

C-----Extract momentum components from helix --------------------------

C	phi  =trk(4)
	cph  =cos(trk(4))
	sph  =sin(trk(4))

C-------check if it is a neutral particle------------------------------

       IF(iflag.eq.5) THEN
	pt   = cos( atan(trk(5)))/trk(6)
       ELSE
	pt   = (beta*const*rc)/100.
       ENDIF	!iflag.eq.5

        p(3) = pt*trk(5)
	p(1) = pt*cph
	p(2) = pt*sph

  999 CONTINUE
      RETURN
      END


C******************************************************************************
      SUBROUTINE ev0_update_track_param(xc,r,x,trk1,trk2,beta)
      IMPLICIT NONE
C   Input arguments  :  xc(2)   !position-coord. in x-y plane of the 
C                               !centre of the circle (charged part.)
C                       r       !radius of the above circle
C                       x(2)    !x-y coord. of new point
C                       trk1(6) !old track parameters
C                       beta    !z component of magnetic field
C   Output arguments :  trk2(6) !updated track parameters
C   Functional Description :    Extrapolates a helix and calculates the 
C                               new helix parameters at another point.
C   Created  March, 1999 A. Saulys   
C   Error conditions : None
C     trk(1) -> r0  (radius from 0,0)
C     trk(2) -> phi (angle about 0,0)
C     trk(3) -> z
C     trk(4) -> psi (the circle angle)
C     trk(5) -> tanl
C     trk(6) -> charge of track ( = =- 1.0)


C---------------------------------------------------------------------
#include "math_constants.inc"

      REAL r,xc(2),x(2),trk1(6),trk2(6),beta
      REAL psi,psir,dpsi

C-----calculate the new track parameters
      trk2(1)=sqrt( x(1)**2+x(2)**2 )
      trk2(2)=atan2( x(2),x(1) )
      psi = atan2(x(2)-xc(2),x(1)-xc(1))

C -- put psi in 0-2pi range
      psi = amod(psi,real(C_2PI))
      if(psi.lt.0)psi=psi+C_2PI

      psir = psi - trk1(6)*C_PI_2
C -- put psir in 0-2pi range
      psir = amod(psir,real(C_2PI))
      if(psir.lt.0)psir=psir+C_2PI

      dpsi = psir - trk1(4)

C -- put dpsi in 0-2pi range
      dpsi = amod(dpsi,real(C_2PI))
      trk2(4)=trk1(4)+dpsi

C -- check for rotation direction
      if(trk1(6).gt.0)then
        if(dpsi.lt.0)then
          dpsi = (C_2PI+dpsi)
        endif
      else
        if(dpsi.gt.0)then
           dpsi = -(C_2PI-dpsi)
        endif
      endif
   
C --  z = zi + dz = zi - dpsi*signedradius*tanl
      trk2(3)=trk1(3)-dpsi*trk1(6)*sign(r,beta)*trk1(5)


      trk2(5)=trk1(5)
      trk2(6)=trk1(6)

      RETURN
      END


C******************************************************************************


      SUBROUTINE ev0_v0_impact_param(mv,x,p,ptotsq,r_min,iflag)
      IMPLICIT NONE
C   Input arguments   	mv      ! 3-dimensional main vertex position
C                       x	! 3-dimensional position vector 
C				! of the secondary vertex
C			p	! 3-dimensional momentum vector of the 
C				! reconstructed secondary particle
C                       ptotsq  ! Square of the sec. particles momentum
C   Output arguments :	r_min 	!Squared distance from the main vertex at the 
C			     	!point of closest approach 
C			iflag	!if equal 2 wrong pointing/false particle
C   Functional Description:	It checks first if the candidate particle
C				points away from the vertex (as it should)
C				Then it finds the impact parameter of a
C				NEUTRAL reconstructed particle 
C				from the main vertex
C   Created  DEC-1991  S.Margetis
C   Error conditions 		: None
C   Status	     		: Tested
C----------------------------------------------------------------------
      INTEGER	iflag
      REAL 	r_min,mv(3),x(3),p(3),ptotsq,rcheck
      REAL      x1,y1,z1
*
*---------assume a good candidate, reset flag-------------------------
*
	iflag=0
*
*---------check first if the candidate flies away from vertex----------
*---------through the dot product of position and momentum  vector-----
*
        x1 = x(1)-mv(1)
        y1 = x(2)-mv(2)
        z1 = x(3)-mv(3)
	rcheck=(x1*p(1))+(y1*p(2))+(z1*p(3))
	IF(rcheck.lt.0.0) THEN
	  iflag=2
          r_min = 99999.
	  goto 999
	ENDIF !if(rcheck<0)
*
*---------calculate the impact parameter of the track from vertex-------
*------------------ Normal way using vector-cross-product -------------
*
	r_min=((y1*p(3))-(p(2)*z1))**2.+
     +        ((z1*p(1))-(p(3)*x1))**2.+ ((x1*p(2))-(p(1)*y1))**2.
	r_min=r_min/(ptotsq)
*----------------------------------------------------------------------

 999    RETURN
      END


C************************************************************************



      SUBROUTINE ev0_vzero_geom(cut,xc1,xc2,r1,r2,x,y,iflag)
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
      REAL	asq_plus_bsq,r1sq_minus_om1sq,r1_dia,r1sq_minus_om2sq
      REAL	ph1_plus_a_sq,ph2_plus_a_sq,om_1,om_2
      REAL	om1_plus_b_sq_minus_r2sq,om2_plus_b_sq_minus_r2sq
C---clear variables cleared before entering routine--------------------
c      CALL VZERO(x,2)
c      CALL VZERO(y,2)
C---find the two intersections----------------------------------------
      r1sq = r1*r1
      r2sq = r2*r2
      a=xc1(1)-xc2(1)
      b=xc1(2)-xc2(2)
      asq_plus_bsq = a*a + b*b
      c=(r1sq-r2sq + asq_plus_bsq)/2.
      d=asq_plus_bsq*r1sq - c*c
C---check if there is any solution, one or two -----------------------
      IF(d.lt.0.0) GOTO 137
      IF(d.eq.0.0) iflag=3
      d = sqrt(d)
C     om1=( -b*c+abs(a)*d ) / asq_plus_bsq
C     om2=( -b*c-abs(a)*d ) / asq_plus_bsq
      om_1=-b*c
      om_2=abs(a)*d
      om1=( om_1+om_2 ) / asq_plus_bsq
      om2=( om_1-om_2 ) / asq_plus_bsq
      r1sq_minus_om1sq = r1sq-om1*om1
      r1sq_minus_om2sq = r1sq-om2*om2
C----find the right pair---------------------------------------------
      IF( r1sq_minus_om1sq.ge.0.) THEN
	ph1=sqrt(r1sq_minus_om1sq)
	ph2=-ph1
        ph1_plus_a_sq = (ph1+a)**2
        ph2_plus_a_sq = (ph2+a)**2
        om1_plus_b_sq_minus_r2sq = (om1+b)**2 - r2sq
	IF( abs(ph1_plus_a_sq + om1_plus_b_sq_minus_r2sq).le.
     +      abs(ph2_plus_a_sq + om1_plus_b_sq_minus_r2sq) ) THEN
		x(1)=ph1+xc1(1)
	ELSE
		x(1)=ph2+xc1(1)
	ENDIF	
	y(1)=om1+xc1(2)
      ENDIF	
C------second pair------------------------------------------------
      IF( r1sq_minus_om2sq.ge.0.) THEN
	ph1=sqrt(r1sq_minus_om2sq)
	ph2=-ph1
        ph1_plus_a_sq = (ph1+a)**2
        ph2_plus_a_sq = (ph2+a)**2
        om2_plus_b_sq_minus_r2sq = (om2+b)**2 - r2sq
	IF( abs(ph1_plus_a_sq + om2_plus_b_sq_minus_r2sq).le.
     +      abs(ph2_plus_a_sq + om2_plus_b_sq_minus_r2sq) ) THEN
		x(2)=ph1+xc1(1)
	ELSE
		x(2)=ph2+xc1(1)
	ENDIF	
	y(2)=om2+xc1(2)
      ENDIF
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN

  137 continue
      dia=sqrt(asq_plus_bsq)
      dtouch=dia-r1-r2
      r1_dia = r1/dia
      IF(dtouch .le. sqrt(cut))  THEN
        iflag=3
        x(1)=xc1(1) + r1_dia*(xc2(1)-xc1(1))
        y(1)=xc1(2) + r1_dia*(xc2(2)-xc1(2))
        x(2)=x(1)
        y(2)=y(1)
      ELSE
        iflag=5
      RETURN
      ENDIF

      END


C******************************************************************************

      SUBROUTINE ev0_v0_dca(xn1, xn2, sxz1, syz1, sxz2, syz2, xv0, v0dca)
      IMPLICIT NONE
C   Input arguments  :  xn1(3)  !coord. of first track
C                       xn2(3)  !  "      second track
C   Output arguments :  xv0(3)  !coord. of seed vertex at point of 
C                                 closest approach
C                       v0dca   !dca betwee the two tracks.
C                       iflag   !iflag=5 if no solution, =3 if only one
C   Functional Description :    Calculates the (3D) distance of closest 
C                                approach 
C                               of two tracks (vectors) as well as the 
C                                     'seed' vertex 
C                               coordinates.
C   Created  JUL-1995 S.Margetis
C   Error conditions    : None
C   Status              : Tested
C----------------------------------------------------------------------
        real xn1(3),xn2(3),xv0(3),sxz1, syz1, sxz2, syz2
        real x1,x2,y1,y2,z1,z2
        real v0dca
        real a1,a2,a3,c,k,l,m,b,v,AA,BB,CC,DD,EE,FF,GG
        real x,y,z,xp,yp,zp


        x1=xn1(1)
        y1=xn1(2)
        z1=xn1(3)
        x2=xn2(1)
        y2=xn2(2)
        z2=xn2(3)
        a1=syz1-syz2
        a2=sxz2-sxz1
        a3=sxz1*syz2-syz1*sxz2

        v0dca = ((x1-x2)*a1 + (y1-y2)*a2 + (z1-z2)*a3) /
     /                                sqrt(a1*a1 + a2*a2 + a3*a3)

       IF ( (syz1.ne.0.).AND.(syz2.ne.0.) ) THEN
        c =sxz1/syz1
        k =(c*y1) - x1
        l =y1/syz1 - z1
        b =sxz2/syz2
        m =(b*y2) - x2
        v =y2/syz2 - z2
        AA =c*a2-a1
        BB =b*a2-a1
        CC =(k-m)
        DD =a2/syz1 - a3
        EE =a2/syz2 - a3
        FF =(l-v)
        GG = a2/(DD*BB - AA*EE)
      
        yp=(AA*FF - CC*DD)*GG
        y =(BB*FF - CC*EE)*GG
        xv0(2)=(y+yp)/2.

        zp=yp/syz2 - v
        z =y/syz1 - l
        xv0(3)=(z+zp)/2.

        xp=b*yp - m
        x =c*y  - k
        xv0(1)=(x+xp)/2.
       ELSE
        xv0(1)=.001
        xv0(2)=.001
        xv0(3)=.001
       ENDIF

        RETURN
        END
**********


