*
*  7-7-2011 Eulogio Serradilla <eulogio.serradilla@ciemat.es>
*
*  Proton + Nucleus reaction cross section parametrization
*
*  Astroparticle Physics Volume 6, Issues 3-4, April 1997, Pages 379-386
*
      REAL FUNCTION SIGPA(E_K, A_T)

        IMPLICIT NONE

        REAL E_K, A_T, Z_T, SIGHE
        REAL BIG
        PARAMETER (BIG=10000000000.)

        IF(E_K.LE.0.00001) GOTO 100 ! GeV

        SIGHE = 45.*A_T**0.7*(1.+0.016*sin(5.3-2.63*log(A_T)))

        SIGPA = SIGHE*(1.-0.62*exp(-5.*E_K)*sin(1.58*E_K**(-0.28)))

        RETURN

100     SIGPA = BIG

      END
