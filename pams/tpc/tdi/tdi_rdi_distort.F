* $Id: tdi_rdi_distort.F,v 1.2 2000/02/23 23:25:35 hardtke Exp $
* $Log: tdi_rdi_distort.F,v $
* Revision 1.2  2000/02/23 23:25:35  hardtke
* replace tpg functions with tpc functions
*
* Revision 1.1  1998/01/27 00:59:19  fisyak
* Split sources
*
      SUBROUTINE TDI_RDI_DISTORT(i_sector,drho,x_g,p_g)
C>----------------------------------------------------------------------
C TDI_RDI_DISTORT - Distorts the MC hit
C
C DESCRIPTION:
C Distorts the MC hit by projecting the radial distortion on to the
C pad plane. Theta is the angle between the cosmic ray track and the
C radial line defined by the undistorted MC point. In general theta
C is not equal to the track crossing angle alpha, which is defined
C with respect to the line perpendicular to the pad row. Only when
C radial line is perpendicular to the pad row are theta and alpha the
C same value. 
C
C INPUT ARGUMENTS:
C i_sector - sector number
C drho     - radial distortion
C x_g(3)   - undistorted MC hit (global coordinates)
C p_g(3)   - MC momentum (or direction cosines in global coordinates)
C
C OUTPUT ARGUMENTS:
C x_g(3)   - distorted MC hit (global coordinates)
C
C AUTHOR:
C 18-Mar-1997: Peter G. Jones: University of Birmingham
C<----------------------------------------------------------------------

      IMPLICIT NONE
c-----------------------------------------------------------------------
      integer i_sector
      real drho, x_g(3), p_g(3)

      real x_l(3), p_l(3), theta, tan_theta, tan_alpha, dy
c-----------------------------------------------------------------------
      call tpc_global_to_local(i_sector,x_g,x_l)
      tan_theta = x_l(1)/x_l(2)
      theta = atan(tan_theta)
      dy = drho*cos(theta)
    
      call tpc_global_to_local_p(i_sector,p_g,p_l)
      tan_alpha = p_l(1)/p_l(2)

      x_l(1) = x_l(1) - dy*(tan_alpha-tan_theta)
      call tpc_local_to_global(i_sector,x_l,x_g)

      return
      end
