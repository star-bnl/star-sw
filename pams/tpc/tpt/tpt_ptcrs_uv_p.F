      INTEGER FUNCTION TPT_PTCRS_UV_P(as,bs,at,bt,x0,y0,xp1,yp1)

      IMPLICIT NONE

      REAL as,bs,at,bt,xp1(2),yp1(2),x0,y0
      REAL a,b,c,delta
C______________________________________________________________________
      if(abs(as).gt.0.000001) then
        a=(bt+bt*bs**2/as**2)
        b=(-at*bs/as-2.0*bt*bs/as*(1.0/as-x0)-2.0*bt*y0+1)
        c=(at*(1.0/as-x0)+bt*(1.0/as-x0)**2+bt*y0**2-y0)
        delta=b**2-4.0*a*c
        if(delta.ge.0) then
          yp1(1)=(-b-sqrt(delta))*0.5/a
          yp1(2)=(-b+sqrt(delta))*0.5/a
          xp1(1)=(1.0-yp1(1)*bs)/as
          xp1(2)=(1.0-yp1(2)*bs)/as
          tpt_ptcrs_uv_p=1
        else
          tpt_ptcrs_uv_p=0
        endif
      else
        a=(bt+bt*as**2/bs**2)
        b=(at-2.0*bt*x0-2.0*bt*as/bs*(1.0/bs-y0)-as/bs)
        c=(-at*x0+bt*x0**2+bt*(1.0/bs-y0)+1.0/bs-y0)
        delta=b**2-4.0*a*c
        if(delta.ge.0) then
          xp1(1)=(-b-sqrt(delta))*0.5/a
          xp1(2)=(-b+sqrt(delta))*0.5/a
          yp1(1)=1.0/bs-as/bs*xp1(1)
          yp1(2)=1.0/bs-as/bs*xp1(2)
          tpt_ptcrs_uv_p=1
        else
          tpt_ptcrs_uv_p=0
        endif
      endif
      end
