C
      SUBROUTINE PROJECT_TRACK(psi,q,pt,tanl,x0,y0,bfld,z0,xp,x)
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
