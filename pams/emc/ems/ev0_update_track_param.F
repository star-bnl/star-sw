      SUBROUTINE ev0_update_track_param(xc,r,x,trk1,trk2)
c:>------------------------------------------------------------------
C:FILE:         ems_ev0_routines.F
C:DESCRIPTION:  Service routines for EMC Analysis Module, package ems, STAF compliant.
c:AUTHOR:       Spiros Margetis?
C:BUGS:         None known.
C:HISTORY:      05dec95-v1.0-wjl- Creation.
c:>------------------------------------------------------------------
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
