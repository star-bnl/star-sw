C:===========================================================================
C     SUBROUTINE SVG_G2L
C
C     Description:
C     Point mapping from global to local SVT wafer coordinate
C
C:===========================================================================
      subroutine svg_g2l(x,xp,wx,wd,wt,wn)
      implicit none

c     DESCRIPTION:   global to local mapping for svt points
c
c     Input Arguments:
c       x     : global coordinate
c       wx    : wafer origin
c       wd    : wafer drift direction
c       wt    : wafer transverse direction
c       wn    : wafer normal
c
c     Output Arguments:
c       xp    : local coordinate
c
c     Functional Description: 
c      Make a global to local mapping by using the origin of the wafer coordinate and the
c     3 vectors defining the normal to the plane, the drift direction, and the transverse direction.
c     xp(1)  : component in the drift direction
c     xp(2)  : component in the transverse direction
c     xp(3)  : component in the direction normal to the wafer; should be small ...
c
c     Created 1-Nov-93     C. Pruneau, WSU
c     Modified 10-Sept-96  C. Pruneau, WSU, name changed and added to svg package
c
c     Error Conditions: none
c
c     Argument declarations:
c     ======================
      real  x(3), xp(3), wx(3), wd(3), wt(3), wn(3)

c     Local Declarations
c     ==================
      real  xl(3)

c     Executable Code
c     ===============
      xl(1) = x(1) - wx(1)
      xl(2) = x(2) - wx(2)
      xl(3) = x(3) - wx(3)

      xp(1) = xl(1)*wd(1) + xl(2)*wd(2) + xl(3)*wd(3) 
      xp(2) = xl(1)*wt(1) + xl(2)*wt(2) + xl(3)*wt(3) 
      xp(3) = xl(1)*wn(1) + xl(2)*wn(2) + xl(3)*wn(3) 

      return
      end
