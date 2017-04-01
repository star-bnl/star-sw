*
*  7-7-2011 Eulogio Serradilla <eulogio.serradilla@ciemat.es>
*
*  Neutron + Nucleus reaction cross section parametrization
*
*  Astroparticle Physics Volume 6, Issues 3-4, April 1997, Pages 379-386
*
      REAL FUNCTION SIGNA(E_K, A_T)

        IMPLICIT NONE

        REAL E_K, A_T

        SIGNA = 43.2*A_T**(0.719)

        RETURN

      END
