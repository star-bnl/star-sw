* $Id: tdi_rdi_uniform.F,v 1.1 1998/01/27 00:59:21 fisyak Exp $
* $Log: tdi_rdi_uniform.F,v $
* Revision 1.1  1998/01/27 00:59:21  fisyak
* Split sources
*
      REAL FUNCTION TDI_RDI_UNIFORM(rho,rho_min,rdi_max)
C>----------------------------------------------------------------------
C TDI_RDI_UNIFORM - Calculates the Radial Distortion Integral
C
C DESCRIPTION:
C This simplified distortion scenario has no physical basis. A uniform
C (z-independent) radial distortion is introduced, which has a maximum
C value of rdi_max at the radius of the OFC, and decreases linearly to
C zero distortion at a radius of rho_min.
C
C INPUT ARGUMENTS:
C rho     - radial position of space point, rho = sqrt(x**2 + y**2)
C rho_min - minimum radius of distortion
C rdi_max - maximum amplitiude of the distortion (at OFC)
C
C OUTPUT ARGUMENTS:
C Function returns the radial distortion [cm]
C
C AUTHOR:
C 29-Nov-1996: Peter G. Jones: University of Birmingham
C<----------------------------------------------------------------------

      IMPLICIT NONE
c-----------------------------------------------------------------------
      real rho, rho_min, rdi_max

      real Rout
      parameter (Rout=200.)

      real rdi
c-----------------------------------------------------------------------
c*** Initialize

      rdi = 0

c*** Calculate distortion

      if( rho.gt.rho_min .and. rho.lt.Rout ) then
        rdi = rdi_max*(rho-rho_min)/(Rout-rho_min)
      endif

      TDI_RDI_UNIFORM = rdi

      end
