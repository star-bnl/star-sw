
      SUBROUTINE PHO_LOOREG(S,Ga,Aa,Gb,Bb,Delta,Alphap,Gppp,Bppp,Vir2a,
     &                      Vir2b,Siglo,Blo)
C**********************************************************************
C
C     calculation of loop-Pomeron total cross section
C     according to Gribov's Regge theory
C
C     input:        S        squared cms energy
C                   GA       coupling constant to diffractive line
C                   AA       slope related to GA (GeV**-2)
C                   GB       coupling constant to elastic line
C                   BB       slope related to GB (GeV**-2)
C                   DELTA    effective pomeron delta (intercept-1)
C                   ALPHAP   slope of pomeron trajectory (GeV**-2)
C                   GPPP     triple-Pomeron coupling
C                   BPPP     slope related to B0PPP (GeV**-2)
C                   VIR2A    virtuality of particle a (GeV**2)
C                   VIR2B    virtuality of particle b (GeV**2)
C                   note: units of all coupling constants are mb**1/2
C
C     output:       SIGLO    total loop-Pomeron cross section
C                   BLO      effective loop-Pomeron slope
C                            (differs from double diffractive slope!)
C
C     uses E_i (Exponential-Integral function)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Aa , Alphap , alsca , Bb , Blo , Bppp , Delta , 
     &                 EPS , Ga , Gb , Gppp , part1 , parta , partb , 
     &                 PHO_EXPINT , S , sigl , Siglo , sigu , Vir2a
      DOUBLE PRECISION Vir2b
      SAVE 
 
      PARAMETER (EPS=0.0001D0)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  some constants
      INCLUDE 'inc/pocons'
 
C  integration cut-off Sigma_U ( see Nucl.Phys.B97(1975)493 )
      sigu = 2.5
C  integration cut-off Sigma_L (min. squared mass of diff. blob)
      sigl = 5. + Vir2a + Vir2b
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(51).GE.10 )
     &      WRITE (LO,'(1X,A,/1X,1P,9E10.3)')
     &      'PHO_LOOREG: S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP ' , S , 
     &     Ga , Aa , Gb , Bb , Delta , Alphap , Gppp , Bppp
C
      IF ( S.LT.5.D0 ) THEN
         Siglo = 0.D0
         Blo = 2.D0*Bppp
         RETURN
      END IF
 
C
C  change units of ALPHAP to mb
      alsca = Alphap*GEV2mb
C
C  cross section
      part1 = Ga*Gb*Gppp**2/(16.*PI*2.*alsca)
     &        *S**Delta*EXP(-Delta*Bppp/Alphap)
      parta = Bppp/Alphap + LOG(S/sigl**2)
      partb = Bppp/Alphap + LOG(sigu)
      Siglo = part1*
     &        (parta*(PHO_EXPINT(parta*Delta)-PHO_EXPINT(partb*Delta))
     &        +EXP(parta*Delta)/Delta-EXP(partb*Delta)/Delta)
C
C  slope
      part1 = LOG(ABS(parta/partb))*(parta-LOG(1.D0+S/(sigl**2*sigu)))
      part1 = 0.25*Alphap*LOG(1.D0+S/(sigu*sigl))**2/part1
      Blo = (Aa+Bb)/2. + 2.*Bppp + Alphap*LOG(S/4.D0)
      Blo = Blo - part1
C
      IF ( Siglo.LT.EPS ) Siglo = 0.D0
      IF ( Blo.LT.2.D0*Bppp ) Blo = 2.D0*Bppp
C
      IF ( LPRi.GT.4 .AND. IDEb(51).GE.7 ) WRITE (LO,'(1X,A,1P,3E12.3)')
     &      'PHO_LOOREG: ENERGY,SIGLO,BLO' , SQRT(S) , Siglo , Blo
      END SUBROUTINE
