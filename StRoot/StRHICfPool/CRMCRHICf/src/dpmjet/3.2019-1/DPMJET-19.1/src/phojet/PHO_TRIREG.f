
      SUBROUTINE PHO_TRIREG(S,Ga,Aa,Gb,Bb,Delta,Alphap,Gppp,Bppp,Vir2a,
     &                      Sigtr,Btr)
C**********************************************************************
C
C     calculation of triple-Pomeron total cross section
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
C                   note: units of all coupling constants are mb**1/2
C
C     output:       SIGTR    total triple-Pomeron cross section
C                   BTR      effective triple-Pomeron slope
C                            (differs from diffractive slope!)
C
C     uses E_i (Exponential-Integral function)
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Aa , Alphap , alsca , Bb , Bppp , Btr , Delta , 
     &                 EPS , Ga , Gb , Gppp , part1 , part2 , part3 , 
     &                 PHO_EXPINT , S , sigl , Sigtr , sigu , Vir2a
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
      sigl = 5. + Vir2a
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(50).GE.10 )
     &      WRITE (LO,'(1X,A,/1X,1P,9E10.3)')
     &      'PHO_TRIREG: S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP ' , S , 
     &     Ga , Aa , Gb , Bb , Delta , Alphap , Gppp , Bppp
C
      IF ( S.LT.5.D0 ) THEN
         Sigtr = 0.D0
         Btr = Bppp + Bb
         RETURN
      END IF
C  change units of ALPHAP to mb
      alsca = Alphap*GEV2mb
C
C  cross section
      part1 = Ga*Gb**2*Gppp/(16.*PI*2.*alsca)
     &        *S**Delta*EXP(-(Bb+Bppp)/(2.*Alphap)*Delta)
      part2 = PHO_EXPINT(((Bb+Bppp)/(2.*Alphap)+LOG(S/sigl))*Delta)
      part3 = PHO_EXPINT(((Bb+Bppp)/(2.*Alphap)+LOG(sigu))*Delta)
C
      Sigtr = part1*(part2-part3)
C
C  slope
      part1 = (Bb+Bppp+2.*Alphap*LOG(S/sigl))
     &        /(Bb+Bppp+2.*Alphap*LOG(sigu))
      part2 = LOG(part1)
      part1 = 0.5D0*Alphap*LOG(1.D0+S/(sigu*sigl))/part2
      Btr = (Aa+Bb/2.D0)/2.D0 + Bppp + Alphap*LOG(S/4.D0)
      Btr = Btr - part1
C
      IF ( Sigtr.LT.EPS ) Sigtr = 0.D0
      IF ( Btr.LT.Bb ) Btr = Bb
C
      IF ( LPRi.GT.4 .AND. IDEb(50).GE.7 ) WRITE (LO,'(1X,A,1P,3E12.3)')
     &      'PHO_TRIREG: ENERGY,SIGTR,BTR ' , SQRT(S) , Sigtr , Btr
      END SUBROUTINE
