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
      real*8 r1sq,r2sq,r3sq
      real*8 check1,check2

c     error report
c     ============
      character msg*132
      integer   msg_id

c     executable code
c     ===============
c     assume success

      svt_fit_circle3pts = 0  

      
      r1sq=(x(1)**2+y(1)**2)
      r2sq=(x(2)**2+y(2)**2)
      r3sq=(x(3)**2+y(3)**2)

c     calculate circle center coordinates 

      xc=(r2sq-r1sq)*(y(3)-y(2))+(r3sq-r2sq)*(y(1)-y(2))
      xc=0.5*xc/( (x(2)-x(1))*(y(3)-y(2))- (x(2)-x(3))*(y(1)-y(2)) )
      
      yc=r3sq-r2sq+2.0*xc*(x(2)-x(3))
      yc=yc/(2.0*(y(3)-y(2)))

c     calculate radius

      rc=sqrt((x(1)-xc)**2.0+(y(1)-yc)**2.0)

c     check consistency

      check1=sqrt((x(2)-xc)**2.0+(y(2)-yc)**2.0)-rc
      check2=sqrt((x(3)-xc)**2.0+(y(3)-yc)**2.0)-rc
      
      if ((check1.gt.1e-3).or.(check2.gt.1e-3)) then
         msg = ' svt_fit_circle3pts_bchk-e : 3pts fit failed'
         call message(msg,1,msg_id)
         svt_fit_circle3pts = 1
      endif
      
      return
      end


