* $Id: tpt_ptcrs_uv_p.F,v 1.5 1999/10/26 18:18:30 liq Exp $
* $Log: tpt_ptcrs_uv_p.F,v $
* Revision 1.5  1999/10/26 18:18:30  liq
* change r defination from (x-x0)**2+(y-y0)**2 to (x**2-x0**2)+(y**2-y0**2), fixed the bug
*
* Revision 1.4  1998/03/31 17:00:56  sakrejda
* sector boundry crossing calculation corrected-was using old sector naming convention
*
* Revision 1.2  1998/01/27 00:35:51  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_PTCRS_UV_P(as,bs,cs,at,bt,x0,y0,xp1,yp1)
C>---------------------------------------------------------------------
C
C TPT_PTCRS_UV_P - calculates crossing of a track with a padrow 
C                in mapped coors.
C
C DESCRIPTION:
C Routine finds solutions for a set of 2 equations. A TPC row is 
C parametrized by as*x+bs*y+cs=0, a track is parametrized by -(y-y0)/r
C =at*(x-x0)/r+bt where r=(x**2-x0**2)+(y**2-y0**2).
C
C INPUT PARAMETERS:
C  as - slope parameter for the row
C  bs - intercept parameter for the row
C  at - slope parameter for the track
C  bt - intercept parameter for the track
C
C OUTPUT PARAMETERS:
C  xp1 - x coordinates of the solutions
C  yp1 - y coordinates of the solutions
C
C AUTHOR:
C Iwona Sakrejda
C
C<---------------------------------------------------------------------

      IMPLICIT NONE

      REAL as,bs,cs,at,bt,xp1(2),yp1(2),x0,y0
      REAL a,b,c,delta
C______________________________________________________________________
      if(abs(as).gt.abs(bs)) then
       a=(bt+bt*bs**2/as**2)
       b=(1.0-at*bs/as+2.0*bt*cs*bs/as**2)
       c=(-y0-at*cs/as-at*x0+bt*cs**2/as**2-y0**2*bt-x0**2*bt)
       delta=b**2-4.0*a*c
       if(delta.ge.0) then
          yp1(1)=(-b-sqrt(delta))*0.5/a
          yp1(2)=(-b+sqrt(delta))*0.5/a
          xp1(1)=-(cs+yp1(1)*bs)/as
          xp1(2)=-(cs+yp1(2)*bs)/as
          tpt_ptcrs_uv_p=1
        else
          tpt_ptcrs_uv_p=0
        endif
      else
        a=(bt+bt*as**2/bs**2)
        b=(-as/bs+at+2.0*bt*cs*as/bs**2)
        c=(-cs/bs-y0-at*x0+bt*cs**2/bs**2-bt*x0**2-bt*y0**2)
        delta=b**2-4.0*a*c
        if(delta.ge.0) then
          xp1(1)=(-b-sqrt(delta))*0.5/a
          xp1(2)=(-b+sqrt(delta))*0.5/a
          yp1(1)=-(cs+as*xp1(1))/bs
          yp1(2)=-(cs+as*xp1(2))/bs
          tpt_ptcrs_uv_p=1
        else
          tpt_ptcrs_uv_p=0
        endif
      endif
      end





