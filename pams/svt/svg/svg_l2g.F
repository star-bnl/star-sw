C:===========================================================================
C     SUBROUTINE SVG_L2G
C
C     Description:
C     Point mapping from  local SVT to global STAR coordinates
C
C:===========================================================================
      subroutine svg_l2g(xp,x,wx,wd,wt,wn)
      implicit none

c     DESCRIPTION:  local to global mapping for svt points
c
c     Input Arguments:
c       xp    : local coordinate
c       wx    : wafer origin
c       wd    : wafer drift direction
c       wt    : wafer transverse direction
c       wn    : wafer normal
c
c     Output Arguments:
c       x     : global coordinate
c
c     Functional Description: 
c      Make a local to global mapping by using the origin of the wafer coordinate and the
c     3 vectors defining the normal to the plane, the drift direction, and the transverse direction.
c     xp(1)  : component in the drift direction
c     xp(2)  : component in the transverse direction
c     xp(3)  : component in the direction normal to the wafer; should be small ...
c
c     Created 1-Nov-93  C. Pruneau, WSU
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
      xl(1) = xp(1)
      xl(2) = xp(2)
      xl(3) = xp(3)

      x(1) = wx(1) + xl(1)*wd(1) + xl(2)*wt(1) + xl(3)*wn(1) 
      x(2) = wx(2) + xl(1)*wd(2) + xl(2)*wt(2) + xl(3)*wn(2) 
      x(3) = wx(3) + xl(1)*wd(3) + xl(2)*wt(3) + xl(3)*wn(3) 

      return
      end
