      SUBROUTINE svt_project_track(xc,r,xp,x)
      IMPLICIT NONE
C   Input arguments  :
C                       xc(2)   !position-coord. in x-y plane of the
C                               !centre of the circle (charged part.)
C                       r       !radius of the above circle
C                       xp(2)   !x-y coord. of new point
C   Output arguments :  x(2)    !x-y coord. of point of closest approach
C   Functional Description :    Extrapolates a helix and calculates the
C                               coord. of the point of the closest approach
C                               from a given point in the bending plane.
C   Created  JAN-1992 S.Margetis
C   Error conditions : None
C   Status           : Test
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
      ENDIF     !b.eq.0
C-------choose the right one----------------------------------------
      a=(xp(1)-xc(1))*(x1(1)-xc(1)) + (xp(2)-xc(2))*(x1(2)-xc(2))
      IF(a.gt.0.) THEN
        x(1)=x1(1)
        x(2)=x1(2)
      ELSE
        x(1)=x2(1)
        x(2)=x2(2)
      ENDIF     !a.gt.0.

  999 CONTINUE
      RETURN
      END

***********************************************************************
      SUBROUTINE svt_update_track_param(xc,r,x,trk1,trk2)
      IMPLICIT NONE
C   Input arguments  :  xc(2)   !position-coord. in x-y plane of the
C                               !centre of the circle (charged part.)
C                       r       !radius of the above circle
C                       x(2)    !x-y coord. of new point
C                       trk1(6) !old track parameters
C   Output arguments :  trk2(6) !updated track parameters
C   Functional Description :    Extrapolates a helix and calculates the
C                               new helix parameters at another point.
C   Created  JAN-1992 S.Margetis
C   Error conditions : None
C   Status           : Tested
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
C-----The sign in the rhs of the expression for trk2(3) was changed to plus 
C-----in order to be consistent with the definition of trk2(6)=h/rh in
C-----egr_gltrk_eval.c. This function originally considered trk2(6)=q/rh
C-----     Ricardo Mastroleo, 12/12/94.
      trk2(3)=trk1(3)+(trk1(6)/abs(trk1(6)))*dz
      trk2(4)=trk1(4)+dphi
      trk2(5)=trk1(5)
      trk2(6)=trk1(6)

  999 CONTINUE
      RETURN
      END

