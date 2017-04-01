*
*  7-7-2011 Eulogio Serradilla <eulogio.serradilla@ciemat.es>
*
*  Antiproton + Nucleus reaction cross section parametrization
*
*  Astroparticle Physics Volume 6, Issues 3-4, April 1997, Pages 379-386
*
      REAL FUNCTION SIGAPA(E_K, A_T)

        IMPLICIT NONE

        REAL E_K, A_T, Z_T
        REAL a0, a1, a2

        REAL BIG
        PARAMETER (BIG=10000000000.)

        IF(E_K.LE.0.02) GOTO 100 ! GeV

C       Parameters that take into account the Coulomb potential

        a0 = 48.2+19.*(E_K-0.02)**(-0.55)
        a1 = 0.1-0.18*E_K**(-1.2)
        a2 = 0.0012*E_K**(-1.5)

        Z_T = 0.59*A_T**0.927;

C       Cross section parametrization (mb)

        SIGAPA = A_T**(2./3.)*(a0 + a1*Z_T + a2*Z_T**2.)

        RETURN

100     SIGAPA = BIG

      END
