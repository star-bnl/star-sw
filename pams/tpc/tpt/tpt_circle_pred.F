      INTEGER FUNCTION TPT_CIRCLE_PRED(as,bs,acent,bcent,r,n,xp,yp)
C>---------------------------------------------------------------------
C
C TPT_CIRCLE_PRED - prediction for a row crossing (helix track model).
C
C DESCRIPTION:
C A set of two equations (one for a circle and one for a line)
C is solved.
C
C INPUT ARGUMENTS:
C  as    - x coefficient for the row
C  bs    - y coefficient for the row
C  acent - x coordinate of the circle center
C  bcent - y coordinate of the circle ccenter
C  r     - radius of the circle
C
C OUTPUT ARGUMENTS:
C  n     - number of crossing points
C  xp    - vector of x coordinates of the crossing points
C  yp    - vector of y coordinates of the crossing points
C
C<---------------------------------------------------------------------

      IMPLICIT NONE
      REAL     a,b,c,delta
      REAL     as,bs,acent,bcent,r,xp(2),yp(2)
      INTEGER  n
C
      n = 0
      if(abs(as).gt.0.000001)then
        a = (1.0+bs**2/as**2)
        b = -2.0*(bs/as*(1.0/as - acent)+bcent)
        c = bcent**2+(acent-1.0/as)**2-r**2
        delta = b**2-4.0*a*c
        if(delta.lt.0.0) then
          tpt_circle_pred = 1
        else
          yp(1) = (-b-sqrt(delta))/a*0.5
          yp(2) = (-b+sqrt(delta))/a*0.5
          xp(1) = 1.0/as-yp(1)*bs/as
          xp(2) = 1.0/as-yp(2)*bs/as
          n=2
          tpt_circle_pred = 0
        endif
      else
        a = 1.0+as**2/bs**2
        b = -2.0*(acent+as/bs*(1.0/bs-bcent))
        c = acent**2+(bcent-1.0/bs)**2-r**2
        delta = b**2-4.0*a*c
        if(delta.lt.0.0) then
          tpt_circle_pred = 1
        else
          xp(1) = (-b-sqrt(delta))/a*0.5
          xp(2) = (-b+sqrt(delta))/a*0.5
          yp(1) = 1.0/as-xp(1)*bs/as
          yp(2) = 1.0/as-xp(2)*bs/as
          n=2
          tpt_circle_pred = 0
        endif
      endif
C
      end
