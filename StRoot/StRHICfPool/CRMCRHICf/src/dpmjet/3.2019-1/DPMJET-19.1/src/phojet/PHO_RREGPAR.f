
      SUBROUTINE PHO_RREGPAR
C*********************************************************************
 
C     Restore reggeon and pomeron parameter common block according
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
     &         'PHO_RREGPAR: Restoring reggeon parameters.' , IDXmpar
      END IF
      ALPom = SALpom(IDXmpar)
      ALPomp = SALpomp(IDXmpar)
      ALReg = SALreg(IDXmpar)
      ALRegp = SALregp(IDXmpar)
      GPPp = SGPpp(IDXmpar)
      GPPr = SGPpr(IDXmpar)
      B0Ppp = SB0ppp(IDXmpar)
      B0Ppr = SB0ppr(IDXmpar)
      B0Har = SB0har(IDXmpar)
      AKFac = SAKfac(IDXmpar)
      DO i = 1 , 2
         GP(i) = SGP(i,IDXmpar)
         GR(i) = SGR(i,IDXmpar)
         B0Pom(i) = SB0pom(i,IDXmpar)
         B0Reg(i) = SB0reg(i,IDXmpar)
      END DO
      DO i = 1 , 4
         VDMfac(i) = SVDmfac(i,IDXmpar)
         VDMq2f(i) = SVDmq2f(i,IDXmpar)
      END DO
      DO i = 1 , 2
         PHIsup(i) = SPHisup(i,IDXmpar)
         RMAss(i) = SRMass(i,IDXmpar)
      END DO
 
      END SUBROUTINE
