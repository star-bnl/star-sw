C******************************************************************************
      SUBROUTINE prop_update_track_param(xc,r,x,trk1,xout,beta)
      IMPLICIT NONE
C   Input arguments  :  xc(2)   !position-coord. in x-y plane of the 
C                               !centre of the circle (charged part.)
C                       r       !radius of the above circle
C                       x(2)    !x-y coord. of new point
C                       trk1(7) !old track parameters
C                       beta    !z component of magnetic field
C   Output arguments :  xout(2) !new z and psi
C   Functional Description :    Extrapolates a helix and calculates the 
C                               new helix parameters at another point.
C   Created  March, 1999 A. Saulys   
C   Error conditions : None
C     trk(3) -> z
C     trk(4) -> psi (the circle angle)
C     trk(5) -> tanl
C     trk(6) -> charge of track ( = =- 1.0)


C---------------------------------------------------------------------
#include "math_constants.inc"

      REAL r,xc(2),x(2),trk1(7),xout(2),beta
      REAL psi,psir,dpsi

C-----calculate the new track parameters

      psi = atan2(x(2)-xc(2),x(1)-xc(1))

C -- put psi in 0-2pi range
      psi = amod(psi,real(C_2PI))
      if(psi.lt.0.)psi=psi+C_2PI

      psir = psi - trk1(6)*C_PI_2
C -- put psir in 0-2pi range
      psir = amod(psir,real(C_2PI))
      if(psir.lt.0.)psir=psir+C_2PI

      dpsi = psir - trk1(4)

C -- put dpsi in 0-2pi range
      dpsi = amod(dpsi,real(C_2PI))
      xout(2)=trk1(4)+dpsi

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
      xout(1)=trk1(3)-dpsi*trk1(6)*sign(r,beta)*trk1(5)

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
      REAL    x,y,p(3),a,b,ab,rxp,test
C--------find the radius and the (hit)x-y coordinates------------------
      x=trk(1)
      y=trk(2)
C--------get the momentum components-----------------------------------
      CALL prop_track_mom(trk,p)
C--------find the two possible solutions------------------------------
      IF(abs(p(1)).lt.0.001) p(1)=0.001
      a=p(2)/p(1)
      b = sqrt( r*r/(a*a+1) )
      ab = a*b
C-------decide which one is the right one------------------------------
      rxp=-(ab*p(2) + b*p(1))
      test=rxp*trk(6)
      IF(test.gt.0.0) THEN
	xc(1)=-ab + x
	xc(2)= b + y
      ELSE
	xc(1)=ab + x
	xc(2)=-b + y
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
      IF(d.lt.0.001) iflag=3 
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
	cph  =cos(psi)
	sph  =sin(psi)

	pt   =trk(7)

        p(3) = pt*trk(5)
	p(1) =pt*cph
	p(2) =pt*sph

  999 CONTINUE
      RETURN
      END


C******************************************************************************


