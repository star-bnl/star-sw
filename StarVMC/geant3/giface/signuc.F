*
*  7-7-2011 Eulogio Serradilla <eulogio.serradilla@ciemat.es>
*
*  Empirical formula for light (anti)nuclei reaction cross section
*
*  Astroparticle Physics Volume 6, Issues 3-4, April 1997, Pages 379-386
*  Phys. Rev. C 53, 2919-2925 (1996)
*  Nuclear Physics A Volume 696, Issues 1-2, 17 December 2001, Pages 3-30
*  Nuclear Physics A Volume 676, Issues 1-4, 28 August 2000, Pages 3-31
*
      REAL FUNCTION SIGNUC(IPART, E_K, A_T)

        IMPLICIT NONE

        INTEGER IPART
        REAL E_K, A_T, Z_T, N_P, Z_P, E_KN
        REAL KA
        REAL SIGPA, SIGNA, SIGAPA, SIGANA

        KA = 1.
        SIGNUC = 0

C       Correction factors

        IF(IPART.EQ.45.OR.IPART.EQ.76) THEN ! (anti)deuteron
          N_P = 1.
          Z_P = 1.
          KA = 2.24206*log(A_T+2.)**(-5.14701e-01)

        ELSE IF(IPART.EQ.46.OR.IPART.EQ.77) THEN ! (anti)triton
          N_P = 2.
          Z_P = 1.
          KA = 2.57912*log(A_T+2.)**(-8.17582e-01)

        ELSE IF(IPART.EQ.49.OR.IPART.EQ.78) THEN ! (anti)he3
          N_P = 1.
          Z_P = 2.
          KA = 2.57912*log(A_T+2.)**(-8.17582e-01)

        ELSE IF(IPART.EQ.47.OR.IPART.EQ.79) THEN ! (anti)alpha
          N_P = 2.
          Z_P = 2.
          KA = 2.29773*log(A_T+2.)**(-8.56596e-01)

        ELSE
          WRITE(*,*) "Unkown projectile ", IPART
          GOTO 999

        END IF

C       Kinetic energy per nucleon (GeV)

        E_KN = E_K/(N_P+Z_P)

C       Reaction cross section (mb)

        IF(IPART.LE.49) THEN

          SIGNUC = KA*( Z_P*SIGPA(E_KN,A_T)**(3./2.)
     +           + N_P*SIGNA(E_KN,A_T)**(3./2.))**(2./3.)

        ELSE

          SIGNUC = KA*( Z_P*SIGAPA(E_KN,A_T)**(3./2.)
     +           + N_P*SIGANA(E_KN,A_T)**(3./2.))**(2./3.)
        END IF

        RETURN

999   END
