
      SUBROUTINE PHO_TRXPOM(S,Ga,Aa,Gb,Bb,Delta,Alphap,Gppp,Bppp,Sigdp,
     &                      Bdp)
C**********************************************************************
C
C     calculation of total cross section of two tripe-Pomeron
C     graphs in X configuration according to Gribov's Reggeon field
C     theory
C
C     input:        S        squared cms energy
C                   GA       coupling constant to elastic line 1
C                   AA       slope related to GA (GeV**-2)
C                   GB       coupling constant to elastic line 2
C                   BB       slope related to GB (GeV**-2)
C                   DELTA    effective pomeron delta (intercept-1)
C                   ALPHAP   slope of pomeron trajectory (GeV**-2)
C                   BPPP     triple-Pomeron coupling
C                   BTR      slope related to B0PPP (GeV**-2)
C                   note: units of all coupling constants are mb**1/2
C
C     output:       SIGDP    total cross section for double-Pomeron
C                            scattering
C                   BDP      effective double-Pomeron slope
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Aa , alcsmx , aloc , alosmx , alpha2 , Alphap , 
     &                 amxsq , Bb , Bdp , Bppp , c , Delta , EPS , fac , 
     &                 Ga , Gb , Gppp , S , Sigdp , sigl
      DOUBLE PRECISION sigu , w , wn , wsc , xi , xil , xiu , xpos1 , 
     &                 xsum , xwgh1
      INTEGER i1 , ngaus1
      SAVE 
 
      PARAMETER (EPS=0.0001D0)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
 
      DIMENSION xwgh1(96) , xpos1(96)
 
C  lower integration cut-off Sigma_L
      sigl = PARmdl(71)**2
C  upper integration cut-off Sigma_U
      c = 1.D0 - 1.D0/PARmdl(70)**2
      c = MAX(PARmdl(72),c)
      sigu = (1.D0-c)**2*S
C  integration precision
      ngaus1 = 16
C
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(52).GE.10 )
     &      WRITE (LO,'(1X,A,/1X,1P,9E10.3)')
     &      'PHO_TRXPOM: S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP' , S , 
     &     Ga , Aa , Gb , Bb , Delta , Alphap , Gppp , Bppp
C
      IF ( sigu.LE.sigl ) THEN
         Sigdp = 0.D0
         Bdp = Aa + Bb
         RETURN
      END IF
C
C  cross section
C
      xil = LOG(sigl)
      xiu = LOG(sigu)
      xi = LOG(S)
      fac = (Gppp*Ga*Gb)**2/(256.D0*PI2)/Alphap/GEV2mb**2
      alpha2 = 2.D0*Alphap
      aloc = LOG(1.D0/(1.D0-c))
      CALL PHO_GAUSET(xil,xiu,ngaus1,xpos1,xwgh1)
      xsum = 0.D0
      DO i1 = 1 , ngaus1
         amxsq = EXP(xpos1(i1))
         alosmx = LOG(S/amxsq)
         alcsmx = LOG((1.D0-c)*S/amxsq)
         w = LOG((Aa+Bppp+alpha2*alcsmx)/(Bb+Bppp+alpha2*aloc))
         w = MAX(0.D0,w)
         wn = (Aa+Bb+2.D0*Bppp+alpha2*alosmx)
C  supercritical part
         wsc = amxsq**Delta*(S/amxsq)**(2.D0*Delta)
         xsum = xsum + w*xwgh1(i1)/wn*wsc
      END DO
      Sigdp = xsum*fac
C
C  slope
      Bdp = 0.5*(Aa+Bb+Bppp+Alphap*xi)
C
      IF ( LPRi.GT.4 .AND. IDEb(52).GE.7 ) WRITE (LO,'(1X,A,1P,3E12.3)')
     &      'PHO_TRXPOM: ENERGY,SIGDP,BDP' , SQRT(S) , Sigdp , Bdp
      END SUBROUTINE
