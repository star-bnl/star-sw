* $Id: cros.F,v 1.1 1998/01/27 01:54:47 fisyak Exp $
* $Log: cros.F,v $
* Revision 1.1  1998/01/27 01:54:47  fisyak
* Split sources
*
      integer function cros(flag,c,d,r,a,b,xe,ye,x1,y1)
c     c - x coordinate of the center
c     d - y coordinate of the center
c     r - radius
c     a ,b parametrisation of the row boundary (ax+by = 1)

c     find a crossing point between a straigth line parametrized by
c     (ax+by=1) and a circle (x-c)**2+(y-d)**2=r**2 that is
c     closest to a given point (xe,ye)
c
      implicit none
      integer flag
      real c,d,r,a,b,xe,ye,x1,y1,x2,y2,a1,b1,c1,delta,d1,d2

      if(abs(a).le.abs(b)) then
        a1=(b**2+a**2)
        b1=-2.0*((1.0-b*d)*a+b**2*c)
        c1=(1.0-b*d)**2+b**2*(c**2-r**2)
        delta=b1**2-4.0*a1*c1
        if(delta.ge.0) then
           x1=0.5*(-b1-sqrt(delta))/a1
           x2=0.5*(-b1+sqrt(delta))/a1
           y1=(1.0-a*x1)/b
           y2=(1.0-a*x2)/b
           d1=sqrt((xe-x1)**2+(ye-y1)**2)
           d2=sqrt((xe-x2)**2+(ye-y2)**2)
           if((d2.lt.d1.and.flag.eq.0).or.
     >     (flag.gt.0.and.d1.lt.d2)) then
             x1=x2
             y1=y2
           endif
           cros=0
        else
           cros=-1
        endif
      else
        a1=(b**2+a**2)
        b1=-2.0*((1.0-a*c)*b+a**2*d)
        c1=(1.0-a*c)**2+a**2*(d**2-r**2)
        delta=b1**2-4.0*a1*c1
        if(delta.ge.0) then
           y1=0.5*(-b1-sqrt(delta))/a1
           y2=0.5*(-b1+sqrt(delta))/a1
           x1=(1.0-b*y1)/a
           x2=(1.0-b*y2)/a
           d1=sqrt((xe-x1)**2+(ye-y1)**2)
           d2=sqrt((xe-x2)**2+(ye-y2)**2)
           if((d2.lt.d1.and.flag.eq.0).or.
     >     (flag.gt.0.and.d1.lt.d2)) then
             x1=x2
             y1=y2
           endif
           cros=0
        else
           cros=-1
        endif
      endif
      end
