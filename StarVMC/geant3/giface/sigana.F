*
*  7-7-2011 Eulogio Serradilla <eulogio.serradilla@ciemat.es>
*
*  Antineutron + Nucleus reaction cross section parametrization
*
*  Astroparticle Physics Volume 6, Issues 3-4, April 1997, Pages 379-386
*
      REAL FUNCTION SIGANA(E_K, A_T)

        IMPLICIT NONE

        REAL E_K, A_T
        REAL BIG
        PARAMETER (BIG=10000000000.)

        IF(E_K.LE.0.001) GOTO 100 ! GeV

C       Reaction cross section (mb)

        SIGANA = A_T**(2./3.)*(51.+16.*E_K**(-0.4))

        RETURN

100     SIGANA = BIG

      END
