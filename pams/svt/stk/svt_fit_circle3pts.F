      integer function svt_fit_circle3pts(x,y,xc,yc,rc)
      implicit none
      
c     description: routine to calculate the radius of a circle and
c                  its center position given only 3 points.
c
c     Input:
c     x(3),y(3): 3 points (x,y) coordinates 
c
c     Output:
c     xc, yx   : coordinates (x,y) of the circle center
c     rc       : circle radius
c
c     Return:
c     0    : success
c     1    : calculation fails because of limited accuracy
c
c     author:  W.K.Wilson
c
c     history:  W.K.Wilson          written
c               C.A.P. march 30, 94 modified and integrated to TAS
c		C.A.P. april  1, 94 using double precision internally
c
c     arguments
c     =========
      real x(3), y(3), xc, yc, rc

c     local declarations
c     ==================
      real*8 r1sq,r2sq,r3sq,xc1,yc1,rc1
      real*8 check1,check2
 

c     executable code
c     ===============
c     assume success

      svt_fit_circle3pts = 0  

      
      r1sq=(x(1)**2+y(1)**2)
      r2sq=(x(2)**2+y(2)**2)
      r3sq=(x(3)**2+y(3)**2)

c     calculate circle center coordinates 

      xc1=(r2sq-r1sq)*(y(3)-y(2))+(r3sq-r2sq)*(y(1)-y(2))
      xc1=0.5*xc1/( (x(2)-x(1))*(y(3)-y(2))- (x(2)-x(3))*(y(1)-y(2)) )
      
      yc1=r3sq-r2sq+2.0*xc1*(x(2)-x(3))
      yc1=yc1/(2.0*(y(3)-y(2)))

c     calculate radius

      rc1=sqrt((x(1)-xc1)**2.0+(y(1)-yc1)**2.0)

c     check consistency

      check1=sqrt((x(2)-xc1)**2.0+(y(2)-yc1)**2.0)-rc1
      check2=sqrt((x(3)-xc1)**2.0+(y(3)-yc1)**2.0)-rc1
      
      if ((check1.gt.1e-3).or.(check2.gt.1e-3)) then
         call StWarning(' svt_fit_circle3pts_bchk-e : 3pts fit failed')
         svt_fit_circle3pts = 1
      endif

      xc = xc1
      yc = yc1
      rc = rc1
      
      return
      end


