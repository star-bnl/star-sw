
      SUBROUTINE PHO_SREGPAR
C*********************************************************************
 
C     Save reggeon and pomeron parameter common block according
C     to current multiparticle index
 
C     input:   in /popreg/
 
C     output:  in /pospre/
 
C*********************************************************************
      IMPLICIT NONE
      INTEGER i
 
      INCLUDE 'inc/pobeam'
      INCLUDE 'inc/poinou'
      INCLUDE 'inc/podebg'
      INCLUDE 'inc/popreg'
      INCLUDE 'inc/pospre'
      INCLUDE 'inc/po2cha'
 
      IF ( LPRi.GT.4 ) THEN
         IF ( IDEb(90).GT.10 ) WRITE (LO,'(1X,A,I2)')
     &         'PHO_SREGPAR: Saving reggeon parameters.' , IDXmpar
      END IF
      SALpom(IDXmpar) = ALPom
      SALpomp(IDXmpar) = ALPomp
      SALreg(IDXmpar) = ALReg
      SALregp(IDXmpar) = ALRegp
      SGPpp(IDXmpar) = GPPp
      SGPpr(IDXmpar) = GPPr
      SB0ppp(IDXmpar) = B0Ppp
      SB0ppr(IDXmpar) = B0Ppr
      SB0har(IDXmpar) = B0Har
      SAKfac(IDXmpar) = AKFac
      DO i = 1 , 2
         SGP(i,IDXmpar) = GP(i)
         SGR(i,IDXmpar) = GR(i)
         SB0pom(i,IDXmpar) = B0Pom(i)
         SB0reg(i,IDXmpar) = B0Reg(i)
      END DO
      DO i = 1 , 4
         SVDmfac(i,IDXmpar) = VDMfac(i)
         SVDmq2f(i,IDXmpar) = VDMq2f(i)
      END DO
      DO i = 1 , 2
         SPHisup(i,IDXmpar) = PHIsup(i)
         SRMass(i,IDXmpar) = RMAss(i)
      END DO
 
      END SUBROUTINE
