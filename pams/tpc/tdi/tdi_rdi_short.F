* $Id: tdi_rdi_short.F,v 1.1 1998/01/27 00:59:20 fisyak Exp $
* $Log: tdi_rdi_short.F,v $
* Revision 1.1  1998/01/27 00:59:20  fisyak
* Split sources
*
      REAL FUNCTION TDI_RDI_SHORT(z,rho,zshort)
C>----------------------------------------------------------------------
C TDI_RDI_SHORT - Calculates the Radial Distortion Integral
C
C DESCRIPTION:
C Calculates the distortion due to shorted stripes on the outer field
C cage. Uses formalism and nomenclature of STAR note 253 by H. Wieman.
C
C INPUT ARGUMENTS:
C z      - z coordinate of space point
C rho    - radial position of space point, rho = sqrt(x**2 + y**2)
C zshort - z coordinate of shorted stripe
C
C OUTPUT ARGUMENTS:
C Function returns the radial distortion [cm]
C
C AUTHOR:
C 29-Nov-1996: Peter G. Jones: University of Birmingham
C<----------------------------------------------------------------------

      IMPLICIT NONE
c-----------------------------------------------------------------------
      real z, rho, zshort

      integer NResistors, Nn
      real    DriftLength, Pi, Rin, Rout

      parameter (NResistors=182)
      parameter (DriftLength=210.)
      parameter (Pi=3.141592654)
      parameter (Nn=80)
      parameter (Rin=47.476)
      parameter (Rout=200.)

      logical converged

      integer n

      double precision sor_n, rdi, rdi_last
      double precision ai, ao, ar, az
      double precision a_n, b_n, besfun
      double precision I0_ai, K0_ao, I0_ao, K0_ai
      double precision DBESI0, DBESI1, DBESK0, DBESK1
c-----------------------------------------------------------------------
c*** Initialize

      converged = .false.
      rdi = 0
      n = 0

c*** Perform sum, coverge test prevents NaN from Inexact/Underflow/Overflow

      do while( n.lt.Nn .and. .not.converged )

        n = n + 1

        sor_n = -2*cos(n*Pi*zshort/Driftlength)/(n*Pi*(NResistors-1))

        ai  = n*Pi*Rin/DriftLength
        ao  = n*Pi*Rout/DriftLength
        ar  = n*Pi*rho/DriftLength
        az  = n*Pi*z/Driftlength

        I0_ai = DBESI0(ai)
        K0_ao = DBESK0(ao)
        I0_ao = DBESI0(ao)
        K0_ai = DBESK0(ai)

        besfun = I0_ai*K0_ao - I0_ao*K0_ai

        a_n = -sor_n*K0_ai/besfun
        b_n =  sor_n*I0_ai/besfun

        rdi_last = rdi
        rdi = rdi + (a_n*DBESI1(ar) - b_n*DBESK1(ar))*(cos(az) - 1.)
c        if( abs(rdi-rdi_last).lt.1e-30 ) converged = .true.

      enddo

      TDI_RDI_SHORT = real(rdi)*DriftLength

      end
