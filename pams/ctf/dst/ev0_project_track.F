      SUBROUTINE ev0_project_track(xc,r,xp,x)
c:>------------------------------------------------------------------
C:FILE:         ems_ev0_routines.F
C:DESCRIPTION:  Service routines for EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       Spiros Margetis?
C:BUGS:         None known.
C:HISTORY:      05dec95-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
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
